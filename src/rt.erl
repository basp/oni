%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 Bas Pennings (TMG)
%%% @end
%%%----------------------------------------------------------------------------
-module(rt).
-behaviour(gen_server).

%% API
-export([start_link/0, execute/1, execute/3]).

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
    gen_server:call(?SERVER, {execute, MFA}).

execute(M, F, A) ->
    gen_server:call(?SERVER, {execute, {M, F, A}}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) -> 
    process_flag(trap_exit, true),
    {ok, []}.

handle_call({execute, {M, F, A}}, _From, State) ->
    R = apply(M, F, A),
    {reply, R, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.