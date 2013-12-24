%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(test).
-compile(export_all).

-record(object, {id, name, location}).

setup_db() ->
    mnesia:start(),
    mnesia:create_table(object, [{attributes, record_info(fields, object)}]),
    mnesia:stop().

notify(Socket, Msg) ->
    Line = io_lib:format("~s~n", [Msg]),
    gen_tcp:send(Socket, Line).

foo(Socket) ->
    notify(Socket, <<"You start fooing.">>),
    timer:sleep(2000),
    notify(Socket, <<"You finish fooing.">>).