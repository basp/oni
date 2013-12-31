%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(ctable).
-behaviour(gen_server).

%% API
-export([start/0, insert/2, delete/1, lookup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, connection).

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

insert(Key, Data) ->
    gen_server:cast(?SERVER, {insert, {Key, Data}}).

delete(Key) ->
    gen_server:cast(?SERVER, {delete, Key}).

lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) ->
    ets:new(?TABLE, [named_table, public]),
    {ok, []}.

handle_call({lookup, Key}, _From, State) ->
    case ets:lookup(?TABLE, Key) of
        [] -> {reply, {ok, none}, State};
        [{Key, Data}|_] -> {reply, {ok, Data}, State}
    end.

handle_cast({insert, Record}, State) ->
    ets:insert(?TABLE, Record),
    {noreply, State};
handle_cast({delete, Key}, State) ->
    ets:delete(?TABLE, Key), 
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.