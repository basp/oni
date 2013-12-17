%%%----------------------------------------------------------------------------
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_rt_server).
-behaviour(gen_server).

%% API
-export([start_link/0, apply/3, apply_after/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

apply(M, F, A) ->
    MFA = {M, F, A},
    gen_server:cast(?SERVER, {apply, MFA}).

apply_after(Time, M, F, A) ->
    MFA = {M, F, A},
    Args = [?SERVER, {apply, MFA}],
    timer:apply_after(Time, gen_server, cast, Args).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

init(State) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({apply, {M, F, A}}, State) ->
    case erlang:apply(M, F, A) of
        {done, _Res} -> 
            ok;
        {cont, Time, {CM, CF, CA}} ->
            apply_after(Time, CM, CF, CA)
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.