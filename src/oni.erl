%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).
-compile(export_all).

init() ->
    mnesia:start(),
    id_gen:start_link(),
    object:init_db(),
    Root = object:create(nothing),
    Room = object:create(Root),
    Wizard = object:create(Root),
    object:set_property(Room, "name", "The First Room"),
    object:set_property(Wizard, "name", "Wizard"),
    object:set_player_flag(Wizard, true),
    object:set_property(Wizard, "wizard", true),
    object:set_property(Wizard, "programmer", true),
    object:move(Wizard, Room),
    ets:new(connection, [named_table]),
    tcp_server:start_link(7777),
    ok.