%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(pq_server).
-behaviour(gen_server).

%% API
-export([start_link/0, enqueue/2, info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

%%%============================================================================
%%% API
%%%============================================================================
start_link() -> 
    gen_server:start_link(?MODULE, [queue:new()], []).

enqueue(Pid, Action) -> 
    gen_server:cast(Pid, {enqueue, Action}).

info(Pid) -> 
    gen_server:call(Pid, info).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Q]) -> {ok, Q}.

handle_call(info, _From, Q) -> 
    {reply, {ok, Q}, Q}.

handle_cast({enqueue, Action}, Q) ->
    NewQ = queue:in(Action, Q),
    case queue:is_empty(Q) of
        true -> execute(self(), Action);
        false -> ok
    end,
    {noreply, NewQ};
handle_cast(next, Q) ->
    {_Completed, NewQ} = queue:out(Q),
    case queue:peek(NewQ) of
        empty -> ok;
        {value, Action} -> execute(self(), Action)
    end,
    {noreply, NewQ}.

handle_info(_Info, Q) -> {noreply, Q}.

terminate(_Reason, _Q) -> ok.

code_change(_OldVsn, Q, _Extra) -> {ok, Q}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
execute(Pid, Action) -> 
    F = fun() -> Action(), gen_server:cast(Pid, next) end,
    spawn(F).