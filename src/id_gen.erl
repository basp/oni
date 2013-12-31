%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(id_gen).
-behaviour(gen_server).

%% API
-export([start/1, start_link/1, next/0, reset/1, last/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start(Seed) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Seed, nothing], []).

start_link(Seed) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Seed, nothing], []).

next() ->
    gen_server:call(?SERVER, next).

last() ->
    gen_server:call(?SERVER, last).

reset(Seed) ->
    gen_server:cast(?SERVER, {reset, Seed}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([Seed, nothing]) ->
    {ok, {Seed, nothing}}.

handle_call(next, _From, {Next, _Last}) ->
    {reply, {ok, Next}, {Next + 1, Next}};
handle_call(last, _From, {Next, Last}) ->
    {reply, {ok, Last}, {Next, Last}}.

handle_cast({reset, Seed}, {_Next, Last}) ->
    {noreply, {Seed, Last}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.