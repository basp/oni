%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).
-compile(export_all).

-include_lib("include/builtins.hrl").

-define(DEFAULT_PORT, 7777).
-define(DEFAULT_SEED, 0).

%% Init is not idempotent (yet) so don't run it 
%% multiple times in a single Erlang environment.
init() ->
    %% We'll need mnesia for our objects, juse memory table for now
    ok = mnesia:start(),
    
    %% This process will create our object id's
    {ok, _Pid} = id_gen:start(?DEFAULT_SEED),
    
    %% Create the tables required for object storage
    {atomic, ok} = object:init_db(),

    %% Genesis objects
    Root = object:create(nothing),
    Room = object:create(Root),
    Wizard = object:create(Root),

    %% And then initialize the properties
    ok = object:set_property(Room, ?NAME, "The First Room"),
    ok = object:set_property(Wizard, ?NAME, "Wizard"),
    ok = object:set_property(Wizard, ?WIZARD, true),
    ok = object:set_property(Wizard, ?PROGRAMMER, true),
    ok = object:set_player_flag(Wizard, true),

    %% Move Wizard to the first (and only) room
    ok = object:move(Wizard, Room),
    
    %% Start up the runtime
    {ok, _} = rt:start(),

    %% And the connection table
    {ok, _} = ctable:start(),

    %% Finally, start up our listener
    true = listener:start(?DEFAULT_PORT),

    %% Made it!
    ok.