%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(test).
-compile(export_all).

match(Needle, Stack) ->
    match(Needle, Stack, nothing).

match([], _Stack, _Found) -> nothing;
match(_Needle, [], Found) -> Found;
match(Needle, [Thing|Rest], Found) ->
    Pattern = io_lib:format("^~s", [Needle]),   
    case re:run(Thing, Pattern, [{capture, none}]) of
        match when Found =/= nothing -> ambiguous;
        match -> match(Needle, Rest, Thing);
        nomatch -> match(Needle, Rest, Found)
    end.

notify(Socket, Msg) ->
    Line = io_lib:format("~s~n", [Msg]),
    gen_tcp:send(Socket, Line).

foo(Socket) ->
    notify(Socket, <<"You start fooing.">>),
    timer:sleep(2000),
    notify(Socket, <<"You finish fooing.">>).

start_bar() ->
    io:format("You start barring.~n"),
    MFA = {test, finish_bar, []},
    {continue, 2000, MFA}.

finish_bar() ->
    io:format("You finish barring.~n"),
    done.