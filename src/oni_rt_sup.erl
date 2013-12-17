%%%----------------------------------------------------------------------------
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_rt_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%============================================================================
%%% supervisor callbacks
%%%============================================================================

init([]) ->
    RtServer = {oni_rt_server, {oni_rt_server, start_link, []},
                permanent, 10000, worker, [oni_rt_server]},
    Children = [RtServer],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.