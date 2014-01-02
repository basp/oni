%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 Bas Pennings (TMG)
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).

-export([start/0, stop/0]).

start() ->
  mnesia:start(),
  application:start(oni).

stop() ->
  application:stop(oni),
  mnesia:stop().