%%%----------------------------------------------------------------------------
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_sup).
-behaviour(supervisor).

%% API
-export([start/0, start_link/0, start_in_shell/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    spawn(fun() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []) end).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_in_shell() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    unlink(Pid).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) ->
    Tag1 = {tag1, {idgen, start_link, [0]},
            permanent, 10000, worker, [idgen]},
    Tag2 = {tag2, {rt, start_link, []},
            permanent, 10000, worker, [rt]},
    Children = [Tag1, Tag2],
    RestartStrategy = {one_for_one, 3, 10},
    {ok, {RestartStrategy, Children}}.