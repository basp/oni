%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(test).
-compile(export_all).

-include_lib("include/builtins.hrl").

%% Basic match function
match(Needle, Stack) ->
    match(Needle, Stack, nothing).

match([], _Stack, _Found) -> nothing;
match(_Needle, [], Found) -> Found;
match(Needle, [Thing|Rest], Found) ->
    Pattern = io_lib:format("^~s", [Needle]),   
    case re:run(Thing, Pattern, [{capture, none}]) of
        match when Found =/= nothing -> ambiguous;
        match -> match(Needle, Rest, Thing);
        nomatch -> match(Needle, Rest, Found)
    end.

%% Simple verbs
emote(Player, Str) ->
    Name = object:get_property(Player, ?NAME),
    Msg = io_lib:format("~s ~s", [Name, Str]),
    Location = object:get_property(Player, ?LOCATION),
    Contents = object:contents(Location),
    Players = lists:filter(fun(X) -> object:is_player(X) end, Contents),
    lists:foreach(fun(X) -> object:notify(X, Msg) end, Players).

say(Player, Str) -> 
    Say = case string:sub_string(Str, string:len(Str)) of
        "!" -> "exclaims";
        "?" -> "asks";
        _ -> "says"
    end,
    Name = object:get_property(Player, ?NAME),
    Msg = io_lib:format("~s ~s, \"~s\"", [Name, Say, Str]),
    Location = object:get_property(Player, ?LOCATION),
    Contents = object:contents(Location),
    Players = lists:filter(fun(X) -> object:is_player(X) end, Contents),
    lists:foreach(fun(X) -> object:notify(X, Msg) end, Players).

%% Example action
start_bar() ->
    io:format("You start barring.~n"),
    MFA = {test, finish_bar, []},
    {continue, 2000, MFA}.

finish_bar() ->
    io:format("You finish barring.~n"),
    done.