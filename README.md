# Rebar Alien Plugin

This plugin allows you to include *alien* sub_dirs and/or dependencies in a
project built by rebar. To see how this works, take a look at the `examples`
directory, which contains a sub directory including a simple junit test and
maven build. You can run the example (assuming you have java + maven installed
on your system) by changing to the `examples` directory and typing:

    $ rebar compile

If you pass `-v` you'll get more verbose output.

## How it works

The plugin looks in your `rebar.config` for a list of sub-directories to process,
under the `alien_dirs` configuration element. For each of these directories, the
plugin will ensure that a valid OTP application file (i.e., a template in the
`src/<project>.app.src`) is created if it does not already exist.

Secondly, the plugin will look in your `rebar.config` for any additional build
instructions in the `alien_conf` element. These instructions must take one of the
following forms:

1. `{copy, Src, Dest}` which copies `Src` to `Dest`
2. `{create, Dest, Data}` which creates a file `Dest` with the contents `Data`
3. `{mkdir, Dest}` which creates the specified directory
4. `{exec, Cmd}` some arbitrary shell command to run in the directory

These instructions allow you to generate, copy or otherwise insert your own custom
rebar configuration into the target directory. The project skeleton in `examples`
uses this mechanism to push a set of custom `post_hooks` into the `inttest` folder,
which instruct rebar to run the `mvn clean test` command and execute the *alien*
tests.

Running `rebar clean` will undo any actions specified in `alien_conf` (i.e., it
will delete any generated/copied files and folder) and remove any `rebar.config`
that was generated in `src/<project>.app.src` on your behalf.

## Installation

The *rebar_alien_plugin* requires a recent version rebar with support for plugins
hooking into the `Module:preprocess/2` mechanism. At the time of writing, you will
need to use [this branch](https://github.com/hyperthunk/rebar/tree/first-class-plugins).

Include the following tuple in your rebar deps:

```erlang
{deps, [{rebar_alien_plugin, ".*", {git,
    "git://github.com/hyperthunk/rebar_alien_plugin.git", "master"}}]}.
```

Then you will be able to fetch and install the plugin (locally) with rebar:

    user@host$ rebar get-deps compile
    user@host$ rebar dist skip_deps=true

Alternatively, you may put the plugin into your `ERL_LIBS` path somewhere and
use it in many projects. This can be done manually, or using a package manager:

    user@host$ epm install hyperthunk/rebar_alien_plugin    # or
    user@host$ sutro install hyperthunk/rebar_alien_plugin  # or
    user@host$ agner install rebar_alien_plugin

## Usage

Configure the plugin in your `rebar.config` like so:

```erlang
%% *alien_dirs get treated like local sub_dirs at runtime*
{alien_dirs, ["inttest"]}.
{alien_conf, [
    {"inttest", [
        {create, "rebar.config", "{post_hooks, [{compile, \"mvn clean test\"}]}."}
    ]}
]}.
```
