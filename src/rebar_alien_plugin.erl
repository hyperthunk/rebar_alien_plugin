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

-define(DEBUG(Msg, Args), rebar_log:log(debug, Msg, Args)).

-export(['alien:clean'/2, clean/2, preprocess/2]).

%%
%% Plugin API
%%

preprocess(Config, AppFile) ->
    case is_pending_clean() of
        true ->
            %% Is this *really* true!?
            {ok, []};
        false ->
            case rebar_config:get_local(Config, alien_dirs, []) of
                [] ->
                    {ok, []};
                AlienDirs ->
                    ?DEBUG("Processing Alien Dirs ~p~n", [AlienDirs]),
                    Cwd = rebar_utils:get_cwd(),
                    {ok, [ filename:join(Cwd, Dir) ||
                        Dir <- process(AlienDirs, AppFile, Config) ]}
            end
    end.

'alien:clean'(Config, _AppFile) ->
    Clean = fun(Dir, Item) -> alien_conf_clean(Dir, Item) end,
    [ rebar_file_utils:rm_rf(D) || D <- cleanup(Config, Clean) ],
    ok.

clean(Config, _AppFile) ->
    Clean = fun(Dir, Item) ->
        case lists:member(element(1, Item), [rule, command]) of
            true -> ok;
            false -> alien_conf_clean(Dir, Item)
        end
    end,
    [ rebar_file_utils:rm_rf(D) || D <- cleanup(Config, Clean) ],
    ok.

%%
%% Internal Functions
%%

is_pending_clean() ->
    Commands = rebar_config:get_global(issued_commands, []),
    lists:any(fun(C) -> C =:= 'alien:clean' orelse C =:= 'clean' end, Commands).

cleanup(Config, Clean) ->
    case rebar_config:get_local(Config, alien_dirs, []) of
        [] -> ok;
        AlienDirs ->
            Conf = rebar_config:get_local(Config, alien_conf, []),
            ?DEBUG("Checking Alien Dir '~s' for OTP app file(s)~n", [AlienDirs]),
            DirClean = fun(Dir) ->
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
            case re:run(Desc, "\\[(Alien Dependency)\\]", [{capture, all, list}]) of
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
    ?DEBUG("Checking Alien Dir '~s' for OTP app file(s)~n", [AlienDirs]),
    lists:foldl(fun(Dir, Acc) ->
                    case rebar_app_utils:is_app_dir(Dir) of
                        {true, Existing} ->
                            ?DEBUG("Found '~s'~n", [Existing]),
                            ok;
                        false ->
                            generate_app_file(Dir,
                                rebar_app_utils:app_src_to_app(AppFile))
                    end,
                    apply_config(Dir, proplists:get_value(Dir, Conf, [])),
                    [Dir|Acc]
                end, [], AlienDirs).

apply_config(_Dir, []) ->
    ok;
apply_config(Dir, [{rule, Rule, Instruction}|Rest]) ->
    Valid = case check(Dir, Rule) of
        true -> Rest;
        false -> [Instruction|Rest]
    end,
    apply_config(Dir, Valid);
apply_config(Dir, [Conf|Rest]) ->
    ?DEBUG("Processing instruction [~p]~n", [Conf]),
    case Conf of
        {create, Dest, Data} ->
            file:write_file(filename:join(Dir, Dest), Data, [write]);
        {copy, Src, Dest} ->
            rebar_file_utils:cp_r([Src], filename:join(Dir, Dest));
        {mkdir, Dest} ->
            rebar_utils:ensure_dir(filename:join([Dir, Dest, "FOO"]));
        {exec, Cmd} ->
            case rebar_utils:sh(Cmd, [return_on_error, {cd, Dir}]) of
                {error, {Rc, Err}} ->
                    rebar_log:log(warn, "Command '~s' failed with ~p: ~s~n",
                                 [Cmd, Rc, Err]),
                    ok;
                _ -> ok
            end
    end,
    apply_config(Dir, Rest).

alien_conf_clean(Dir, Conf) ->
    rebar_log:log(debug, "Cleaning '~p' ...~n", [Conf]),
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
            end
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
            ?DEBUG("Modified dependencies: [~p]~n", [NeedsUpdate]),
            length(NeedsUpdate) > 0
    end;
check(Dir, Rule) ->
    length(match_rule(Dir, Rule)) > 0.

match_rule(Dir, Rule) when is_list(Rule) ->
    rebar_log:log(debug, "Checking rule '~s' ...~n", [Rule]),
    FileList = case filelib:wildcard(filename:join(Dir, Rule)) of
        [] -> rebar_utils:find_files(Dir, Rule, true);
        Found when is_list(Found) -> Found
    end,
    rebar_log:log(debug, "Exists? ~p!~n", [FileList]),
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
