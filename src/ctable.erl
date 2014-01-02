%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(ctable).

%% API
-export([create/0, insert/2, delete/1, lookup/1]).

-define(TABLE, connection).

%%%============================================================================
%%% API
%%%============================================================================
create() ->
    ets:new(?TABLE, [named_table, public]).

insert(Key, Data) ->
    ets:insert(?TABLE, {Key, Data}).  

delete(Key) ->
    ets:delete(?TABLE, Key).

lookup(Key) ->
    case ets:lookup(?TABLE, Key) of
        [] -> nothing;
        [{Key, Data}|_] -> Data
    end.