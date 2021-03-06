%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 Bas Pennings (TMG)
%%% @end
%%%----------------------------------------------------------------------------
-module(object).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("include/builtins.hrl").

-record(object, {id, 
                 parent,
                 name = "", 
                 location = nothing, 
                 owner = nothing, 
                 properties = [],
                 flags = 2#000000}).

-record(property, {name, 
                   value}).

%%%============================================================================
%%% Database operations
%%%============================================================================
create_db() ->
    mnesia:create_table(object, 
        [{attributes, record_info(fields, object)},
         {ram_copies, [node()]}]).

delete_db() ->
    mnesia:delete_table(object).

%%%============================================================================
%%% Fundamental operations on objects
%%%============================================================================
create(Parent) ->
    {ok, Id} = idgen:next(),
    O = #object{id = Id, parent = Parent},
    ok = mnesia:dirty_write(object, O),
    Id.

chparent(Object, NewParent) ->
    case mnesia:dirty_read(object, Object) of
        [O] -> mnesia:dirty_write(object, O#object{parent = NewParent}), ok;
        [] -> 'E_INVARG'
    end.

valid(Object) ->
    case mnesia:dirty_read(object, Object) of
        [_O] -> true;
        [] -> false
    end.

parent(Object) ->
    case mnesia:dirty_read(object, Object) of
        [O] -> O#object.parent;
        [] -> 'E_INVARG'
    end.

children(Object) ->
    Q = qlc:q([O#object.id || 
        O <- mnesia:table(object), O#object.parent =:= Object]),
    F = fun() -> qlc:e(Q) end,
    case mnesia:dirty_read(object, Object) of
        [_O] -> 
            {atomic, R} = mnesia:transaction(F), R;
        [] when Object =:= nothing -> 
            {atomic, R} = mnesia:transaction(F), R;
        [] -> 'E_INVARG'
    end.

recycle(Object) ->
    case mnesia:dirty_read(object, Object) of
        [O] ->
            Children = children(Object),
            F = fun(C) -> chparent(C, O#object.parent) end,
            lists:foreach(F, Children),
            mnesia:dirty_delete(object, Object),
            ok;
        [] -> 'E_INVARG'
    end.

%%%============================================================================
%%% Object movement
%%%============================================================================
move(What, Where) ->
    case mnesia:dirty_read(object, What) of
        [O] -> mnesia:dirty_write(object, O#object{location = Where}), ok;
        [] -> 'E_INVARG'
    end.

%%%============================================================================
%%% Operations on properties
%%%============================================================================
contents(Object) ->
    Q = qlc:q([O#object.id || 
        O <- mnesia:table(object), O#object.location =:= Object]),
    F = fun() -> qlc:e(Q) end,
    case mnesia:dirty_read(object, Object) of
        [_O] -> {atomic, R} = mnesia:transaction(F), R;
        [] -> 'E_INVARG'
    end.

properties(Object) ->
    case mnesia:dirty_read(object, Object) of
        [O] -> lists:map(fun(P) -> P#property.name end, O#object.properties);
        [] -> 'E_INVARG'
    end.

add_property(Object, Name, Value) ->
    case mnesia:dirty_read(object, Object) of
        [O] -> case lists:keyfind(Name, 2, O#object.properties) of
            false ->
                P = #property{name = Name, value = Value},
                Props = [P|O#object.properties],
                mnesia:dirty_write(object, O#object{properties = Props}),
                ok;
            _Found -> 'E_INVARG'
        end;
        [] -> 'E_INVARG'
    end.

set_property(Object, Name, Value) ->
    case mnesia:dirty_read(object, Object) of
        [O] -> case Name of
            ?NAME -> 
                mnesia:dirty_write(object, O#object{name = Value}), 
                ok;
            ?LOCATION -> 
                mnesia:dirty_write(object, O#object{location = Value}), 
                ok;
            ?OWNER -> 
                mnesia:dirty_write(object, O#object{owner = Value}), 
                ok;
            ?WIZARD -> 
                mnesia:dirty_write(object, set_flag(O, ?WIZARD_FLAG, Value)), 
                ok;
            ?PROGRAMMER -> 
                mnesia:dirty_write(object, set_flag(O, ?PROGRAMMER_FLAG, Value)), 
                ok;
            ?READ -> 
                mnesia:dirty_write(object, set_flag(O, ?READ_FLAG, Value)), 
                ok;
            ?WRITE ->
                mnesia:dirty_write(object, set_flag(O, ?WRITE_FLAG, Value)), 
                ok;
            ?FERTILE ->
                mnesia:dirty_write(object, set_flag(O, ?FERTILE_FLAG, Value)), 
                ok;
            ?PLAYER ->
                mnesia:dirty_write(object, set_flag(O, ?PLAYER_FLAG, Value)), 
                ok;
            _Other ->
                MapFun = fun(P) ->
                    case P#property.name =:= Name of
                        true -> P#property{value = Value};
                        false -> P
                    end
                end,
                case lists:keyfind(Name, 2, O#object.properties) of
                    false -> 'E_PROPNF';
                    _Found ->
                        Props = lists:map(MapFun, O#object.properties),
                        mnesia:dirty_write(
                            object, 
                            O#object{properties = Props}),
                        ok            
                end
        end;
        [] -> 'E_INVARG'
    end.

delete_property(Object, Name) ->
    FilterFun = fun(P) -> P#property.name =/= Name end,
    case mnesia:dirty_read(object, Object) of
        [O] ->
            case lists:keyfind(Name, 2, O#object.properties) of
                false -> 'E_PROPNF';
                _Found ->
                    Props = lists:filter(FilterFun, O#object.properties),
                    mnesia:dirty_write(object, O#object{properties = Props}),
                    ok
            end;
        [] -> 'E_INVARG'
    end.

get_property(Object, Name) ->
    case mnesia:dirty_read(object, Object) of
        [O] -> case Name of 
            ?NAME -> 
                O#object.name;
            ?LOCATION -> 
                O#object.location;
            ?OWNER -> 
                O#object.owner;
            ?WIZARD -> 
                is_flag_set(O, ?WIZARD_FLAG);
            ?PROGRAMMER -> 
                is_flag_set(O, ?PROGRAMMER_FLAG);
            ?READ -> 
                is_flag_set(O, ?READ_FLAG);
            ?WRITE -> 
                is_flag_set(O, ?WRITE_FLAG);
            ?FERTILE -> 
                is_flag_set(O, ?FERTILE_FLAG);
            ?PLAYER ->
                is_flag_set(O, ?PLAYER_FLAG);
            _Other ->
                case lists:keyfind(Name, 2, O#object.properties) of
                    false -> 'E_PROPNF';
                    #property{name = Name, value = Value} -> Value
                end
        end;
        [] -> 'E_INVARG'
    end.

%%%============================================================================
%%% Operations on player objects
%%%============================================================================
set_player_flag(O, Value) ->
    set_property(O, ?PLAYER, Value).

is_player(O) ->
    get_property(O, ?PLAYER).

players() ->
    Q = qlc:q([O#object.id || 
        O <- mnesia:table(object), is_flag_set(O, ?PLAYER_FLAG)]),
    F = fun() -> qlc:e(Q) end,
    {atomic, R} = mnesia:transaction(F),
    R.

new_connection(Object, Data) ->
    ctable:insert(Object, Data),
    ok.

get_connection(Object) ->
    ctable:lookup(Object).

notify(Conn, Msg) ->
    case get_connection(Conn) of
        {Socket, _Peer} -> 
            gen_tcp:send(Socket, io_lib:format("~s~n", [Msg])),
            ok;
        nothing -> ok
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================
set_flag(O, Flag, Value) ->
    OldFlags = O#object.flags,
    NewFlags = case Value of
        true -> OldFlags bor Flag;
        _Other -> OldFlags band (bnot Flag)
    end,
    O#object{flags = NewFlags}.

is_flag_set(O, Flag) ->
    O#object.flags band Flag =:= Flag.