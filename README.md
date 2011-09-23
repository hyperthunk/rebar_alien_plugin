# Rebar Alien Plugin

This plugin allows you to include *alien* sub_dirs and/or dependencies in a
project built by rebar. To see how this works, take a look at the `examples`
directory, which contains a sub directory including a simple junit test and
maven build. You can run the example (assuming you have java + maven installed
on your system) by changing to the `examples/simple` directory and typing:

    $ rebar compile

If you pass `-v` you'll get more verbose output.

Going into `examples/custom` you will find a more complex sample. Try running
`rebar alien:commands` to list the available commands and then running the special
`mvn:test` command which is added (via configuration!) to rebar for the *inttest*
subfolder.

    $ rebar alien:commands
    ==> inttest (alien:commands)
    Commands for Alien Directory inttest:
    * alien:commands              list Alien commands
    * alien:clean                 clean Alien artefacts

    Commands for Alien Directory inttest:
    * mvn:test                    Run Maven Tests
    $ rebar mvn:test
    ==> inttest (mvn:test)
    [INFO] Scanning for projects...
    [INFO] ------------------------------------------------------------------------
    [INFO] Building Test Demo
    [INFO]    task-segment: [clean, test]
    [INFO] ------------------------------------------------------------------------
    [INFO] [clean:clean {execution: default-clean}]
    [INFO] Deleting directory /Users/t4/work/hyperthunk/rebar_alien_plugin/examples/custom/inttest/target
    [INFO] [resources:resources {execution: default-resources}]
    [WARNING] Using platform encoding (MacRoman actually) to copy filtered resources, i.e. build is platform dependent!
    [INFO] skip non existing resourceDirectory /Users/t4/work/hyperthunk/rebar_alien_plugin/examples/custom/inttest/src/main/resources
    [INFO] [compiler:compile {execution: default-compile}]
    [INFO] No sources to compile
    [INFO] [resources:testResources {execution: default-testResources}]
    [WARNING] Using platform encoding (MacRoman actually) to copy filtered resources, i.e. build is platform dependent!
    [INFO] skip non existing resourceDirectory /Users/t4/work/hyperthunk/rebar_alien_plugin/examples/custom/inttest/src/test/resources
    [INFO] [compiler:testCompile {execution: default-testCompile}]
    [INFO] Compiling 1 source file to /Users/t4/work/hyperthunk/rebar_alien_plugin/examples/custom/inttest/target/test-classes
    [INFO] [surefire:test {execution: default-test}]
    [INFO] Surefire report directory: /Users/t4/work/hyperthunk/rebar_alien_plugin/examples/custom/inttest/target/surefire-reports

    -------------------------------------------------------
     T E S T S
    -------------------------------------------------------
    Running org.nebularis.demos.TestDemo
    Test Ok!

    Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 0.023 sec

    Results :

    Tests run: 1, Failures: 0, Errors: 0, Skipped: 0

    [INFO] ------------------------------------------------------------------------
    [INFO] BUILD SUCCESSFUL
    [INFO] ------------------------------------------------------------------------
    [INFO] Total time: 3 seconds
    [INFO] Finished at: Sat Sep 24 00:10:23 BST 2011
    [INFO] Final Memory: 13M/784M
    [INFO] ------------------------------------------------------------------------
    ==> custom (mvn:test)
    $

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
5. `{rule, Test, Action}` applies *Action* only if *Test* passes
6. `{command, Name, Desc, RulesOrActions}` creates a new rebar command

These instructions allow you to generate, copy or otherwise insert your own custom
rebar configuration into the target directory. The project skeleton in `examples`
uses this mechanism to push a set of custom `post_hooks` into the `inttest` folder,
which instruct rebar to run the `mvn clean test` command and execute the *alien*
tests.

The `{command, ...}` form creates a new rebar command, mapped to the supplied set
of actions/rules. See *examples/custom* for the usage pattern.

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
