-module(runtests).
-export([run/0]).

run() ->
    [ io:format("Running test ~p~n", [I]) || I <- lists:seq(1, 100) ],
    ok.
