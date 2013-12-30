-module(object).
-compile(export_all).

-include_lib("include/records.hrl").

init_db() ->
    mnesia:create_table(object, [{attributes, record_info(fields, object)}]).

create(Parent) ->
    {ok, Id} = id_gen:next(),
    O = #object{id = Id, name = "", location = nothing, parent = Parent},
    ok = mnesia:dirty_write(object, O),
    Id.

valid(Object) ->
    case mnesia:dirty_read(object, Object) of
        [_O|_] -> true;
        [] -> false
    end.

properties(Object) ->
    case mnesia:dirty_read(object, Object) of
        [O] -> lists:map(fun(P) -> P#property.name end, O#object.properties);
        [] -> 'E_INVARG'
    end.

add_property(Object, Name, Value) ->
    case mnesia:dirty_read(object, Object) of
        [O] ->
            case lists:keyfind(Name, 2, O#object.properties) of
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
    MapFun = fun(P) ->
        case P#property.name =:= Name of
            true -> P#property{value = Value};
            false -> P
        end
    end,
    case mnesia:dirty_read(object, Object) of
        [O] ->
            case lists:keyfind(Name, 2, O#object.properties) of
                false -> 'E_PROPNF';
                _Found ->
                    Props = lists:map(MapFun, O#object.properties),
                    mnesia:dirty_write(object, O#object{properties = Props}),
                    ok            
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
        [O] ->
            case lists:keyfind(Name, 2, O#object.properties) of
                false -> 'E_PROPNF';
                #property{name = Name, value = Value} -> Value
            end;
        [] -> 'E_INVARG'
    end.

%% Internal functions