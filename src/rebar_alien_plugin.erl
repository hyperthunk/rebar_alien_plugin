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

%% rebar_cmd_builder api hooks
-export([execute_command/4]).

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
                    case load_conf(alien_dirs, local, Command, Config) of
                        [] ->
                            {ok, []};
                        AlienDirs ->
                            ?DEBUG("Pre-processing ~p for Alien Dirs ~p~n",
                                    [Command, AlienDirs]),
                            Cwd = rebar_utils:get_cwd(),
                            AlienSpecs = process(AlienDirs, AppFile, Config),
                            {Command, Extras} =
                                lists:foldl(fun calculate_pre_dirs/2,
                                            {Command, []}, AlienSpecs),
                            ?DEBUG("Command: ~p, Extras: ~p~n",
                                    [Command, Extras]),
                            {ok, [ filename:join(Cwd, D) || D <- Extras ]}
                    end

            end
    end.

'alien-commands'(Config, _) ->
    ?DEBUG("Processing alien-commands~n", []),
    case is_basedir() of
        true ->
            io:format("Global Alien Commands:~n"),
            show_command_info({filename:basename(rebar_utils:get_cwd()),
                [{command, 'alien-commands', "list Alien commands", []},
                 {command, 'alien-clean', "clean Alien artefacts", []}]}),
            case rebar_config:get(Config, alien_dirs, []) of
                [] -> ok;
                DirSet ->
                    [ alien_commands(Dir, Config) || Dir <- DirSet ],
                    ok
            end;
        false ->
            ok
    end.

'alien-clean'(Config, _AppFile) ->
    Clean = fun(Dir, Item) ->
        ?DEBUG("Cleaning ~p~n", [Item]),
        alien_conf_clean(Dir, Item)
    end,
    [ rebar_file_utils:rm_rf(D) || D <- cleanup(Config, Clean),
                                   D /= ignored ],
    ok.

execute_command(Command, Root, Config, _AppFile) ->
    BaseDir = rebar_config:get_global(base_dir, undefined),
    Cwd = rebar_utils:get_cwd(),
    case (Cwd -- (BaseDir ++ "/")) of
        [] ->
            ?DEBUG("Ignoring non-alien dir ~s for ~p~n", [Cwd, Command]);
        ActualRoot ->
            case load_conf(alien_conf, global, Command, Config) of
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

%%
%% Internal Functions
%%

alien_commands({Dir, explicit}, Config) ->
    alien_commands(Dir, Config);
alien_commands(Dir, Config) ->
    Conf = rebar_config:get_local(Config, alien_conf, []),
    io:format("Commands for Alien Directory ~s:~n", [Dir]),
    case lists:keyfind(Dir, 1, Conf) of
        false ->
            ok;
        CmdSet ->
            show_command_info(CmdSet)
    end.

is_basedir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).

%only_dirs(DirSet) ->
%    lists:map(fun({Dir, explicit}) -> Dir;
%                 (Dir) -> Dir
%              end, DirSet).

load_conf(Conf, Type, Command, Config) ->
    Cwd = rebar_utils:get_cwd(),
    BaseDir = rebar_config:get_global(base_dir, undefined),
    ActualRoot = (Cwd -- (BaseDir ++ "/")),
    case rebar_config:get_global({ActualRoot, Command}, undefined) of
        undefined ->
            %% TODO: get_local doesn't work, but get causes infinite recursion!!!!
            case Type of
                local -> rebar_config:get_local(Config, Conf, []);
                global -> rebar_config:get(Config, Conf, [])
            end;
        done ->
            []
    end.

calculate_pre_dirs({{_Dir, explicit}, undefined}, Acc) ->
    Acc;
calculate_pre_dirs({{Dir, explicit}, Handler}, {Command, Acc}=AccIn) ->
    case erlang:function_exported(Handler, Command, 2) of
        true ->
            case rebar_config:get_global({Dir, Command}, undefined) of
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

show_command_info({_, Commands}) ->
    [ show_command(Name, Desc) || {command, Name, Desc, _} <- Commands ];
show_command_info({command, Name, Desc, _}) ->
    show_command(Name, Desc);
show_command_info(_) ->
    ok.

show_command(Name, Desc) ->
    %% TODO: use a proper format string here...
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
apply_config(Dir, {link, Target, Alias}) ->
    Filename = filename:join(Dir, Target),
    case file:make_symlink(Filename, filename:join(Dir, Alias)) of
        ok ->
            ok;
        {error, enotsup} ->
            %% TODO: do a copy on windows and elsewhere
            ?WARN("Unable to symlink ~s as ~s~n", [Target, Alias]),
            ok;
        {error, _} ->
            ?WARN("Unable to symlink ~s as ~s~n", [Target, Alias]),
            ok
    end;
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
    rebar_cmd_builder:generate_handler(Base, Cmds, ?MODULE).

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
