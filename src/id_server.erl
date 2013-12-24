%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(id_server).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, next/0, reset/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start_link() ->
    start_link(0).

start_link(Seed) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Seed], []).

next() ->
    gen_server:call(?SERVER, next).

reset(Seed) ->
    gen_server:cast(?SERVER, {reset, Seed}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Seed]) ->
    {ok, Seed}.

handle_call(next, _From, Last) ->
    Next = Last + 1,
    {reply, {ok, Next}, Next}.

handle_cast({reset, Seed}, _Last) ->
    {noreply, Seed}.

handle_info(_Info, Last) ->
    {noreply, Last}.

terminate(_Reason, _Last) ->
    ok.

code_change(_OldVsn, Last, _Extra) -> 
    {ok, Last}.