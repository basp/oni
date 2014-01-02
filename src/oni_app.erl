%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 Bas Pennings (TMG)
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_app).
-behaviour(application).

-export([start/2, stop/1]).

-include_lib("include/builtins.hrl").

start(_Type, _Args) ->
    object:create_db(),
    case oni_sup:start_link() of
        {ok, Pid} -> genesis(), {ok, Pid};
        Other -> {error, Other}
    end.

stop(_State) ->
    object:delete_db(),
    ok.

genesis() ->
    Root = object:create(nothing),
    Room = object:create(Root),
    Wizard = object:create(Root),
    object:set_property(Room, ?NAME, "The First Room"),
    object:set_property(Wizard, ?NAME, "Wizard"),
    object:set_property(Wizard, ?WIZARD, true),
    object:set_property(Wizard, ?PROGRAMMER, true),
    object:set_player_flag(Wizard, true),
    object:move(Wizard, Room).