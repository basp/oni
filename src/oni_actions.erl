%%%----------------------------------------------------------------------------
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_actions).
-compile(export_all).

start_foo() ->
    io:format("You start fooing.~n", []),
    Cont = {?MODULE, continue_foo, [5]},
    {cont, 2000, Cont}.

continue_foo(0) ->
    io:format("You finish fooing.~n", []),
    {done, 0};
continue_foo(TimesLeft) ->
    io:format("You continue fooing.~n", []),
    Cont = {?MODULE, continue_foo, [TimesLeft - 1]},
    {cont, 2000, Cont}.

start_bar() ->
    oni_rt_server:apply_after(2000, ?MODULE, start_quux, []),
    io:format("You start barring.~n", []),
    Cont = {?MODULE, finish_bar, []},
    {cont, 5000, Cont}.

finish_bar() ->
    io:format("You finish barring.~n", []),
    {done, 0}.

start_quux() ->
    io:format("You start quuxing.~n", []),
    Cont = {?MODULE, finish_quux, []},
    {cont, 2000, Cont}.

finish_quux() ->
    io:format("You finish quuxing.~n", []),
    {done, 0}.