%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).
-compile(export_all).

-include_lib("include/builtins.hrl").

init() ->
    %% We'll need mnesia for our objects, juse memory table for now
    mnesia:start(),
    
    %% This process will create our object id's
    id_gen:start_link(),
    
    %% Create the tables required for object storage
    object:init_db(),

    %% Initialize basic world
    %% First, the 3 genesis objects
    Root = object:create(nothing),
    Room = object:create(Root),
    Wizard = object:create(Root),

    %% And then initialize the properties
    object:set_property(Room, ?NAME, "The First Room"),
    object:set_property(Wizard, ?NAME, "Wizard"),
    object:set_property(Wizard, ?WIZARD, true),
    object:set_property(Wizard, ?PROGRAMMER, true),
    object:set_player_flag(Wizard, true),

    %% Move Wizard to the first (and only) room
    object:move(Wizard, Room),
    
    %% This will hold active connections so we can find
    %% sockets by object id, no need for mnesia here
    ets:new(connection, [named_table]),
    
    %% Finally, start up our listener
    tcp_server:start_link(7777),

    %% Made it!
    ok.