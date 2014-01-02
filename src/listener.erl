%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(listener).

-include_lib("include/builtins.hrl").

%% API
-export([start/1, start_link/1]).

-define(INVALID_MSG, <<"That doesn't seem right.">>).
-define(SEPARATORS, " \t\r\n").

%%%============================================================================
%%% API
%%%============================================================================
start(Port) ->
    register(listener, spawn(fun() -> spawn_acceptor(Port) end)).

start_link(Port) ->
    register(listener, spawn_link(fun() -> spawn_acceptor(Port) end)).

%%%============================================================================
%%% Internal functions
%%%============================================================================
spawn_acceptor(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
    spawn(fun() -> acceptor(Listen) end),
    timer:sleep(infinity).

acceptor(ListenSocket) ->
    MOTD = <<"Oni [Little Nugget]\n\nWelcome! Please login.\n">>,
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, Peer} = inet:peername(Socket),
    error_logger:info_msg("Client ~p connected~n", [Peer]),
    spawn(fun() -> acceptor(ListenSocket) end),
    gen_tcp:send(Socket, MOTD),
    handle_login(Socket).

normalize(Str) when is_binary(Str) -> normalize(binary_to_list(Str));
normalize(Str) ->
    Tokens = string:tokens(Str, ?SEPARATORS),
    string:join(Tokens, " ").

tokenize(Request) ->
    Line = binary_to_list(Request),
    [Cmd|Args] = string:tokens(Line, ?SEPARATORS),
    Argstr = string:join(Args, " "),
    {ok, {Cmd, Args, Argstr}}.

handle_login(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Request} -> 
            {ok, Peer} = inet:peername(Socket),
            {ok, {Cmd, Args, Argstr}} = tokenize(Request),
            case {Cmd, Args} of 
                {"connect", [Username|_]} -> 
                    case authorize(Username) of
                        false ->
                            log_attempt(Cmd, Args, Argstr, Peer),                
                            gen_tcp:send(Socket, ?INVALID_MSG),
                            handle_login(Socket);
                        Player ->
                            login(Player, {Socket, Peer}),
                            handle(Player, {Socket, Peer})
                    end;
                _Other ->
                    log_attempt(Cmd, Args, Argstr, Peer),
                    gen_tcp:send(Socket, ?INVALID_MSG),
                    handle_login(Socket)
            end;
        {tcp_closed, Socket} ->
            error_logger:info_msg(
                "Client ~p disconnected (from login)~n", 
                [Socket])
    end.

handle(Player, {Socket, Peer}) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<";", Str/binary>>} ->
            gen_tcp:send(Socket, eval_to_str(binary_to_list(Str))),
            handle(Player, {Socket, Peer});
        {tcp, Socket, <<":", Str/binary>>} ->
            test:emote(Player, normalize(Str)),
            handle(Player, {Socket, Peer});
        {tcp, Socket, <<"'", Str/binary>>} ->
            test:say(Player, normalize(Str)),
            handle(Player, {Socket, Peer});
        {tcp, Socket, <<"@quit", _/binary>>} ->
            ctable:delete(Player),
            gen_tcp:send(Socket, <<"Bye!\n">>),
            gen_tcp:close(Socket),
            error_logger:info_msg("Client ~p disconnected.~n", [Peer]);
        {tcp, Socket, Request} ->
            case tokenize(Request) of
                {ok, {"say", _Args, Argstr}} -> test:say(Player, Argstr);
                {ok, {"emote", _Args, Argstr}} -> test:emote(Player, Argstr);
                _ -> object:notify(Player, ?INVALID_MSG)
            end,
            handle(Player, {Socket, Peer});
        {tcp_closed, Socket} ->
            ctable:delete(Player),
            error_logger:info_msg("Client ~p disconnected.~n", [Peer]);
        Junk ->
            error_logger:info_msg("Received junk: ~p~n", [Junk]),
            handle(Player, {Socket, Peer})
    end.

login(Player, {Socket, Peer}) ->
    ctable:insert(Player, {Socket, Peer}),
    Name = object:get_property(Player, ?NAME),
    error_logger:info_msg("~s logged in from ~p~n", [Name, Peer]),
    Msg = io_lib:format("*** Connected (~s) ***~n", [Name]),
    gen_tcp:send(Socket, Msg).

first(Pred, List) ->
    case lists:dropwhile(fun(X) -> not Pred(X) end, List) of
        [] -> false;
        [X|_] -> X
    end.

authorize(Username) ->
    Players = object:players(),
    Pred = fun(X) -> object:get_property(X, <<"name">>) =:= Username end,
    case first(Pred, Players) of
        false -> false;
        Player -> Player
    end.

eval_to_str(Str) ->
    try eval(Str) of
        [Value] -> io_lib:format("=> ~p~n", [Value]);
        [H|T] -> io_lib:format("=> ~p~n", [[H|T]])
    catch
        Exception:Reason -> io_lib:format("~p: ~p~n", [Exception, Reason])
    end.

eval(Str) ->
    {ok, Ts, _} = erl_scan:string(Str),
    {ok, ExprList} = erl_parse:parse_exprs(Ts),
    Bindings = [],
    {ValueList, _NewBindings} = erl_eval:expr_list(ExprList, Bindings),
    ValueList.

log_attempt(Cmd, Args, Argstr, Peer) ->
    error_logger:info_msg(
        "Login attempt with ~p from ~p~n", 
        [{Cmd, Args, Argstr}, Peer]).