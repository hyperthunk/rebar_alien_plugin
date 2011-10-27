# Rebar Alien Plugin

This plugin allows you to include *alien* sub_dirs and/or dependencies in a
project built by rebar. In practise, this means that the plugin can do two things
for you:

1. Allow you to work with sub_dirs that do not have an OTP .app file
2. Allow you to deal with custom behaviours for any kind of sub_dirs

To see how this works, take a look at the `examples` directory, which contains a
sub directory including a simple junit test and maven build. You can run the
example (assuming you have java + maven installed on your system) by changing to
the `examples/simple` directory and typing:

    $ rebar compile

If you pass `-v` you'll get more verbose output.

Going into `examples/custom` you will find a more complete sample, which does the
same thing, but using a more general approach to the plugin configuration.

Try running `rebar alien-commands` to list the available commands and then
running the special `mvn:test` command which is added (via configuration!) to
rebar only for the *inttest* subfolder. The choice of `mvn:test` as a command
name was intended to mirror the command being passed to maven.

    $ rebar alien-commands
    ==> inttest (alien-commands)
    Commands for Alien Directory inttest:
    * alien-commands              list Alien commands
    * alien-clean                 clean Alien artefacts

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
`src/<project>.app.src`) is created if not already present.

Secondly, the plugin will look in your `rebar.config` for any additional build
instructions in the `alien_conf` element. These instructions must take one of the
following forms:

1. `{copy, Src, Dest}` which copies `Src` to `Dest`
2. `{create, Dest, Data}` which creates a file `Dest` with the contents `Data`
3. `{mkdir, Dest}` which creates the specified directory
4. `{exec, Cmd}` some arbitrary shell command to run in the directory
5. `{exec, Cmd, Opts}` as (4), but with options passed to the shell environment
6. `{rule, Test, Action}` applies *Action* only if *Test* passes
7. `{make, Options}` runs `make:all/1` in the directory
8. `{call, {M,F,A}` evaluates `apply/3` with the supplied *MFA* tuple
9. `{call, {M,F,A,dir}}` as (7), but prepends the current directory to `A`
10. `{call, {M,F,A}, Extra}` as (8), passes `Extra` to `code:add_pathsa/1` first
11. `{command, Name, Desc, RulesOrActions}` creates a new rebar command

These instructions allow you to generate, copy or otherwise insert your own custom
rebar configuration into the target directory. The simple project skeleton in
`examples` uses this mechanism to push a set of custom `post_hooks` into
the `inttest` folder, which instruct rebar to run the `mvn clean test` command
and execute the *alien* tests.

The `{command, ...}` form creates a new rebar command, mapped to the supplied set
of actions/rules. See *examples/custom* for the usage pattern.

Running `rebar alien:clean` will undo any actions specified in `alien_conf` (i.e.,
it will delete any generated/copied files and folder) and remove any `rebar.config`
that was generated in `src/<project>.app.src` on your behalf.

## Usage Examples

The `rebar_alien_plugin's` purpose is to help you avoid writing plugins unless you
actually need reusable functionality across projects. Local (source loaded) plugins
are very useful, but for many simple tasks just using configuration is cleaner and
more maintainable.

[Here](http://github.com/hyperthunk/luigi_ibrowse) is an example that runs some
simple sanity checks. The code isn't part of the main build, nor the automated test
suites, but it's useful to keep a copy of it and having a separate project/sub_dir
really seems like overkill.

```erlang
{plugins, [rebar_alien_plugin]}.
{alien_dirs, ["checks"]}.
{alien_conf, [
    {"checks", [
        {command, 'sanity_checks', "Build + Run Sanity Checks", [
            %% compile the sources in the 'checks' folder
            {make, [load]},
            %% evaluate sanity_checks:run/0 with extra dirs on the path
            {call, {sanity_checks, run, []}, [".", "../ebin"]}
        ]}
    ]}
]}.
```

This example is taken from 
[another project](https://github.com/hyperthunk/edbc_oci). The custom
`spec-compile` command is being used to download and install a testing 
framework for C source code, which wouldn't be possible using standard 
rebar depedencies. It then uses this *alien* tool chain to turn test
specifications into source code, which in turn is compiled to produce
a binary. The second `spec-test` command simply executes the compiled
binary to run the tests. 

```erlang
%% *alien_dirs get treated like local sub_dirs at runtime*
{alien_dirs, ["spec"]}.
{alien_conf, [
    {"spec", [
        %% we're adding some new rebar commands here

        {command, 'spec-compile', "build cspec and all tests", [
            %% rebar get-deps doesn't support non-OTP sources,
            %% so we end up doing a fetch and install by hand
            {rule, "cspec",
                {exec, "git clone https://github.com/visionmedia/cspec.git cspec"}},
            {rule, "cspec/bin/cspec", {exec, "make -C cspec"}},
            {rule, "c_src", {mkdir, "c_src"}},
            {rule, "bin", {mkdir, "bin"}},
            %% the rule returns true when any member of B in {A, B} has a
            %% newer timestamp than any member of A
            {rule, {"c_src/edbc_oci_specs.c", "spec/*.spec"},
                %% TODO: make this work on win32 ;)
                {exec, "cat spec/*.spec | cspec/bin/cspec > c_src/edbc_oci_specs.c"}},

            {rule, {"bin/edbc_oci_specs", "c_src/edbc_oci_specs.c"},
                %% TODO: use the libconf/cc module to generate this
                {exec, "cc -Icspec/src cspec/build/cspec.o c_src/edbc_oci_specs.c "
                       "-o bin/edbc_oci_specs"}}
        ]},
        {command, 'spec-test', "run all test specs", [
            {exec, "bin/edbc_oci_specs"}
        ]}
    ]}
]}.

%% these config elements are only processed by running 'alien:clean'
{alien_clean, [
    {"spec", ["spec/cspec"]}
]}.
```

Doing this with `pre_hooks` is cumbersome and the configuration effectively 
creates a full blown build plugin on the fly, for minimal effort.


## Installation

The *rebar_alien_plugin* requires a recent version rebar with support for plugins
hooking into the `Module:preprocess/2` mechanism. 

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
