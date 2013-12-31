%%%----------------------------------------------------------------------------
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(rt).
-behaviour(gen_server).

%% API
-export([start_link/0, execute/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

execute(MFA) ->
    gen_server:cast(?SERVER, {execute, MFA}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({execute, {M, F, A}}, State) ->
    case apply(M, F, A) of
        {continue, Time, MFA} -> 
            timer:apply_after(Time, rt, execute, [MFA]), 
            ok;
        _Other -> ok
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================