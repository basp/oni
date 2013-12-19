%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(pq_server).
-behaviour(gen_server).

%% API
-export([start_link/0, enqueue/1, info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [queue:new()], []).

enqueue(Action) -> 
    gen_server:cast(?SERVER, {enqueue, Action}).

info() -> 
    gen_server:cast(?SERVER, info).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Q]) -> {ok, Q}.

handle_call(info, _From, Q) -> 
    {reply, {ok, Q}, Q}.

handle_cast({enqueue, Action}, Q) ->
    NewQ = queue:in(Action, Q),
    case queue:is_empty(Q) of
        execute(Action),
        false -> ok
    end,
    {noreply, NewQ};
handle_cast(next, Q) ->
    {_Completed, NewQ} = queue:out(Q),
    case queue:is_empty(NewQ) of
        true -> ok;
        false ->
            {value, Action} = queue:peek(NewQ),
            execute(Action)
    end,
    {noreply, NewQ}.

handle_info(_Info, Q) -> {noreply, Q}.

terminate(_Reason, _Q) -> ok.

code_change(_OldVsn, Q, _Extra) -> {ok, Q}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
execute(Action) -> 
    F = fun() -> Action(), gen_server:cast(?SERVER, next),
    spawn(F).