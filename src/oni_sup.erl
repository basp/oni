%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 TMG
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
    connection = ctable:create(),
    true = listener:start(7777),
    Idgen = {idgen, {idgen, start_link, [0]},
            permanent, 10000, worker, [idgen]},
    Rt = {rt, {rt, start_link, []},
            permanent, 10000, worker, [rt]},
    Children = [Idgen, Rt],
    RestartStrategy = {one_for_one, 3, 10},
    {ok, {RestartStrategy, Children}}.