-module(oni).
-compile(export_all).

init() ->
    mnesia:start(),
    id_gen:start_link(),
    object:init_db(),
    Root = object:create(nothing),
    Obj1 = object:create(Root),
    object:add_property(Obj1, "description", nothing),
    ok.