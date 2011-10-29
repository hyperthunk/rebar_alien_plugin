%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(rebar_alien_plugin).

-include_lib("kernel/include/file.hrl").

-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
-define(LOG(Lvl, Msg, Args), rebar_log:log(Lvl, Msg, Args)).
-define(ABORT(Msg, Args), rebar_utils:abort(Msg, Args)).

%% standard rebar hooks
-export([preprocess/2]).

%% special rebar hooks
-export(['alien-commands'/2, 'alien-clean'/2]).

%% command api hooks
-export([execute_command/3]).

%%
%% Plugin API
%%

preprocess(Config, AppFile) ->
    case is_pending_clean() of
        true ->
            %% Is this *really* true!?
            {ok, []};
        false ->
            Command = rebar_utils:command_info(current),
            case Command of
                'alien-commands' ->
                    {ok, []};
                _ ->
                    case load_conf(alien_dirs, Command, Config) of
                        [] ->
                            {ok, []};
                        AlienDirs ->
                            ?DEBUG("Processing Alien Dirs ~p~n", [AlienDirs]),
                            Cwd = rebar_utils:get_cwd(),
                            AlienSpecs = process(AlienDirs, AppFile, Config),
                            {Command, Extras} =
                                lists:foldl(fun calculate_pre_dirs/2,
                                            {Command, []}, AlienSpecs),
                            {ok, [ filename:join(Cwd, D) || D <- Extras ]}
                    end

            end
    end.

'alien-commands'(Config, _) ->
    ?DEBUG("Processing alien-commands~n", []),
    Dir = filename:basename(rebar_utils:get_cwd()),
    io:format("Global Alien Commands:~n"),
    show_command_info({Dir,
        [{command, 'alien-commands', "list Alien commands", []},
         {command, 'alien-clean', "clean Alien artefacts", []}]}),
    io:format("Commands for Alien Directory ~s:~n", [Dir]),
    case rebar_config:get(Config, alien_conf, []) of
        [] -> ok;
        CmdSet ->
            [ show_command_info(C) || C <- CmdSet ]
    end,
    halt(0).

'alien-clean'(Config, _AppFile) ->
    Clean = fun(Dir, Item) ->
        ?DEBUG("Cleaning ~p~n", [Item]),
        alien_conf_clean(Dir, Item)
    end,
    [ rebar_file_utils:rm_rf(D) || D <- cleanup(Config, Clean),
                                   D /= ignored ],
    ok.

%%
%% Internal Functions
%%

load_conf(Conf, Command, Config) ->
    Cwd = rebar_utils:get_cwd(),
    BaseDir = rebar_config:get_global(base_dir, undefined),
    ActualRoot = (Cwd -- (BaseDir ++ "/")),
    case rebar_config:get_global({ActualRoot, Command}, undefined) of
        undefined ->
            rebar_config:get(Config, Conf, []);
        done ->
            []
    end.

calculate_pre_dirs({{_Dir, explicit}, undefined}, Acc) ->
    Acc;
calculate_pre_dirs({{Dir, explicit}, Handler}, {Command, Acc}=AccIn) ->
    case erlang:function_exported(Handler, Command, 2) of
        true ->
            case get({Dir, Command}) of
                undefined ->
                    {Command, [Dir|Acc]};
                done ->
                    AccIn
            end;
        false ->
            AccIn
    end;
calculate_pre_dirs({Dir, _Handler}, {Command, Acc}) ->
    {Command, [Dir|Acc]}.

execute_command(Root, Config, _AppFile) ->
    Command = rebar_utils:command_info(current),
    BaseDir = rebar_config:get_global(base_dir, undefined),
    Cwd = rebar_utils:get_cwd(),
    case (Cwd -- (BaseDir ++ "/")) of
        [] ->
            ?DEBUG("Ignoring non-alien dir ~s for ~p~n", [Cwd, Command]);
        ActualRoot ->
            case load_conf(alien_conf, Command, Config) of
                [] ->
                    ok;
                AlienConf ->
                    ?DEBUG("Processing alien source ~s (actual ~s)~n",
                            [Root, ActualRoot]),
                    % ?DEBUG("alien config = ~p~n", [AlienConf]),
                    RuleBase = proplists:get_value(ActualRoot, AlienConf, []),
                    case lists:keyfind(Command, 2, RuleBase) of
                        {command, _, _, Rules} ->
                            [ apply_config(rebar_utils:get_cwd(), R) ||
                                                                R <- Rules ],
                            rebar_config:set_global({ActualRoot, Command}, done),
                            ok;
                        Other ->
                            ?WARN("Unknown command config ~p~n", [Other]),
                            ok
                    end
            end
    end.

show_command_info({_, Commands}) ->
    [ show_command(Name, Desc) || {command, Name, Desc, _} <- Commands ];
show_command_info({command, Name, Desc, _}) ->
    show_command(Name, Desc);
show_command_info(_) ->
    ok.

show_command(Name, Desc) ->
    Spacer = lists:concat(lists:duplicate(28 - length(atom_to_list(Name)), " ")),
    io:format("* ~s~s~s~n", [Name, Spacer, Desc]).

is_pending_clean() ->
    Commands = rebar_utils:command_info(issued),
    lists:any(fun(C) -> C =:= 'alien:clean' orelse C =:= 'clean' end, Commands).

cleanup(Config, Clean) ->
    ?DEBUG("Cleanup: ~p~n", [Config]),
    case rebar_config:get_local(Config, alien_dirs, []) of
        [] -> [];
        AlienDirs ->
            Conf = rebar_config:get_local(Config, alien_conf, []),
            ?DEBUG("Checking Alien Dirs ~p for OTP app file(s)~n", [AlienDirs]),
            DirClean =
            fun(Dir) ->
                case rebar_app_utils:is_app_dir(Dir) of
                    {true, Existing} ->
                        case is_alien_dependency(Existing) of
                            true ->
                                Items = proplists:get_value(Dir, Conf, []),
                                [ Clean(Dir, Item) || Item <- Items ],
                                Existing;
                            false -> ignored
                        end;
                    false ->
                        ignored
                end
            end,
            lists:map(DirClean, AlienDirs)
    end.

is_alien_dependency(AppFile) ->
    case get_app_description(AppFile) of
        {found, Desc} ->
            case re:run(Desc, "\\[(Alien Dependency)\\]",
                        [{capture, all, list}]) of
                {match, _} -> true;
                _Other -> false
            end;
        {error, Reason} ->
            ?DEBUG("Cannot process '~s' due to ~p, "
                   "ignoring...~n", [AppFile, Reason]),
            false
    end.

get_app_description(Existing) ->
    case rebar_app_utils:load_app_file(Existing) of
        {ok, _, AppData} ->
            case proplists:get_value(description, AppData) of
                false ->
                    {error, not_found};
                Value ->
                    {found, Value}
            end;
        Other ->
            {error, Other}
    end.

process(AlienDirs, AppFile, RebarConfig) ->
    Conf = rebar_config:get_local(RebarConfig, alien_conf, []),
    lists:foldl(fun(DirSpec, Acc) ->
                    Dir = case DirSpec of
                        {Path, explicit} -> Path;
                        _ ->
                            case rebar_app_utils:is_app_dir(DirSpec) of
                                {true, _Existing} ->
                                    ok;
                                false ->
                                    generate_app_file(DirSpec,
                                       rebar_app_utils:app_src_to_app(AppFile))
                            end,
                            DirSpec
                    end,
                    Items = proplists:get_value(Dir, Conf, []),
                    Cmds = [ apply_config(Dir, I) || I <- Items ],
                    BaseName = filename:basename(Dir),
                    Handler = maybe_generate_handler(BaseName, Cmds),
                    [{DirSpec, Handler}|Acc]
                end, [], AlienDirs).

apply_config(Dir, {rule, Rule, Instruction}) ->
    case check(Dir, Rule) of
        true -> ok;
        false -> apply_config(Dir, Instruction)
    end;
apply_config(Dir, {create, Dest, Data}) ->
    file:write_file(filename:join(Dir, Dest), Data, [write]);
apply_config(Dir, {copy, Src, Dest}) ->
    rebar_file_utils:cp_r([Src], filename:join(Dir, Dest));
apply_config(Dir, {mkdir, Dest}) ->
    rebar_utils:ensure_dir(filename:join([Dir, Dest, "FOO"]));
apply_config(Dir, {chmod, Target, Mode}) ->
    Filename = filename:join(Dir, Target),
    {ok, #file_info{mode = Prev}} = file:read_file_info(Filename),
    case file:change_mode(Filename, Prev bor Mode) of
        ok -> ok;
        {error, Reason} ->
            Err = file:format_error(Reason),
            ?WARN("Unable to change mode on ~s: ~s~n", [Target, Err]),
            ok
    end;
apply_config(Dir, {exec, Cmd}) ->
    apply_config(Dir, {exec, Cmd, [{cd, Dir}]});
apply_config(_Dir, {exec, Cmd, Opts}) ->
    case rebar_utils:sh(Cmd, [return_on_error|Opts]) of
        {error, {Rc, Err}} ->
            ?WARN("Command '~s' failed with ~p: ~s~n", [Cmd, Rc, Err]),
            ok;
        _ -> ok
    end;
apply_config(_, {make, Options}) ->
    make:all(Options);
apply_config(Dir, {call, {M, F, A, dir}}) ->
    apply_config(Dir, {call, {M, F, [Dir|A]}});
apply_config(Dir, {call, {_,_,_}=MFA, Extra}) ->
    code:add_pathsa(Extra),
    apply_config(Dir, {call, MFA});
apply_config(_, {call, {M, F, A}}) ->
    code:ensure_loaded(M),
    apply(M, F, A);
apply_config(_, {command, _, _, _}=Cmd) ->
    Cmd;
apply_config(Dir, InstructionSet) when is_list(InstructionSet) ->
    [ apply_config(Dir, I) || I <- InstructionSet ].

maybe_generate_handler(_, []) ->
    undefined;
maybe_generate_handler(Base, Cmds) ->
    case rebar_config:get_global({Base, alien_mod}, undefined) of
        undefined ->
            ?DEBUG("Generating alien command handler(s) for ~p~n", [Base]),
            Exports = [ {C, 2} || {command, C, _, _} <- Cmds ],
            Functions = [ gen_function(Base, C) || {command, C, _, _} <- Cmds ],
            {Forms, Loader} = case code:get_object_code(rebar_alien_plugin) of
                {_,Bin,_} ->
                    ?DEBUG("Compiling from existing binary~n", []),
                    {to_forms(atom_to_list(?MODULE), Exports, Functions, Bin),
                        fun load_binary/2};
                error ->
                    File = code:which(?MODULE),
                    case compile:file(File, [debug_info,
                                             binary, return_errors]) of
                        {ok, _, Bin} ->
                            ?DEBUG("Compiling from binary~n", []),
                            {to_forms(atom_to_list(?MODULE), Exports,
                                      Functions, Bin), fun load_binary/2};
                        Error ->
                            ?WARN("Unable to recompile ~p: ~p~n",
                                  [?MODULE, Error]),
                            {mod_from_scratch(Base, Exports, Functions),
                                                        fun evil_load_binary/2}
                    end;
                _ ->
                    ?WARN("Cannot modify ~p - generating new module~n",
                          [?MODULE]),
                    {mod_from_scratch(Base, Exports, Functions),
                                                        fun evil_load_binary/2}
            end,
            Loaded = case compile:forms(Forms, [report, return]) of
                {ok, ModName, Binary} ->
                    Loader(ModName, Binary);
                {ok, ModName, Binary, _Warnings} ->
                    Loader(ModName, Binary);
                CompileError ->
                    ?ABORT("Unable to compile: ~p~n", [CompileError])
            end,
            rebar_config:set_global({Base, alien_mod}, Loaded),
            Loaded;
        Handler ->
            Handler
    end.

mod_from_scratch(Base, Exports, Functions) ->
    [{attribute, ?LINE, module, list_to_atom(Base ++
                                             "_custom_build_plugin")},
     {attribute, ?LINE, export, Exports}] ++ Functions.

to_forms(Base, Exports, Functions, Bin) ->
    case beam_lib:chunks(Bin, [abstract_code]) of
        {ok, {_,[{abstract_code,{_,[FileDef, ModDef|Code]}}|_]}} ->
            Code2 = lists:keydelete(eof, 1, Code),
            [FileDef, ModDef] ++
            [{attribute,31,export,Exports}] ++
            Code2 ++ Functions; %%  ++ [EOF],
        _ ->
            [{attribute, ?LINE, module, list_to_atom(Base)},
             {attribute, ?LINE, export, Exports}] ++ Functions
    end.

gen_function(Base, Cmd) ->
    {function, ?LINE, Cmd, 2, [
        {clause, ?LINE,
            [{var,?LINE,'Config'},
             {var,?LINE,'File'}],
            [],
            [{call,?LINE,
                {remote,?LINE,
                    {atom,?LINE,rebar_alien_plugin},
                    {atom,?LINE,execute_command}},
                    [erl_parse:abstract(Base),
                     {var,?LINE,'Config'},
                     {var,?LINE,'File'}]}]}]}.

evil_load_binary(Name, Binary) ->
    %% this is a nasty hack - perhaps adding the function to *this*
    %% module would be a better approach, but for now....
    ?DEBUG("Evil Loading: ~p~n", [Name]),
    Name = load_binary(Name, Binary),
    {ok, Existing} = application:get_env(rebar, any_dir_modules),
    application:set_env(rebar, any_dir_modules, [Name|Existing]),
    Name.

load_binary(Name, Binary) ->
    case code:load_binary(Name, "", Binary) of
        {module, Name}  ->
            ?DEBUG("Module ~p loaded~n", [Name]),
            Name;
        {error, Reason} -> ?ABORT("Unable to load binary: ~p~n", [Reason])
    end.

alien_conf_clean(Dir, Conf) ->
    ?DEBUG("Cleaning '~p' ...~n", [Conf]),
    case Conf of
        {create, Dest, _} ->
            rebar_file_utils:rm_rf(filename:join(Dir, Dest));
        {copy, _, Dest} ->
            rebar_file_utils:rm_rf(filename:join(Dir, Dest));
        {mkdir, Dest} ->
            rebar_file_utils:rm_rf(filename:join(Dir, Dest));
        {rule, Rule, _} ->
            case Rule of
                {Targets, _} ->
                    [ rebar_file_utils:rm_rf(Match) ||
                                            Match <- match_rule(Dir, Targets) ];
                Target when is_list(Target) ->
                    [ rebar_file_utils:rm_rf(Match) ||
                                            Match <- match_rule(Dir, Target) ]
            end;
        _ -> ok
    end.

check(Dir, {Target, Deps}) ->
    Xs = match_rule(Dir, Target),
    case match_rule(Dir, Deps) of
        [] ->
            ?DEBUG("Skipping rule with no matching dependencies...~n", []),
            true;
        Ys ->
            NeedsUpdate = [ X || X <- Xs, Y <- Ys,
                   filelib:last_modified(X) < filelib:last_modified(Y) ],
            ?DEBUG("Modified dependencies: ~p~n", [NeedsUpdate]),
            %% there are some matching Xs, but no Ys are newer on the file system
            length(Xs) > 0 andalso length(NeedsUpdate) == 0
    end;
check(Dir, Rule) ->
    length(match_rule(Dir, Rule)) > 0.

match_rule(Dir, Rule) when is_list(Rule) ->
    ?DEBUG("Checking rule '~s' ...~n", [Rule]),
    FileList = case filelib:wildcard(filename:join(Dir, Rule)) of
        [] -> rebar_utils:find_files(Dir, Rule, true);
        Found when is_list(Found) -> Found
    end,
    ?DEBUG("Matched ~p~n", [FileList]),
    FileList.

generate_app_file(Dir, RootAppName) ->
    Name = filename:basename(Dir),
    Target = filename:join([Dir, "src", Name ++ ".app.src"]),
    ?DEBUG("Generating Appfile '~s'~n", [Target]),
    rebar_utils:ensure_dir(Target),
    file:write_file(Target, app(Name, RootAppName), [write]).

app(Name, RootAppName) ->
    Desc = "[Alien Dependency] " ++ Name ++
           " (loaded by " ++ RootAppName ++ ")",
    App = {application, list_to_atom(Name),
           [{description, Desc},
            {vsn, "1"},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
