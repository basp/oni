%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(tcp_server).

-include_lib("include/builtins.hrl").

%% API
-export([start_link/1]).

%%%============================================================================
%%% API
%%%============================================================================
start_link(Port) ->
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
        spawn(fun() -> acceptor(Listen) end),
        timer:sleep(infinity)
    end),
    {ok, Pid}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
acceptor(ListenSocket) ->
    MOTD = <<"Oni - Steampunk Dreams\n\nWelcome! Please login.\n">>,
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, Peer} = inet:peername(Socket),
    error_logger:info_msg("Client ~p connected~n", [Peer]),
    spawn(fun() -> acceptor(ListenSocket) end),
    gen_tcp:send(Socket, MOTD),
    handle_login(Socket).

tokenize(Request) ->
    Line = binary_to_list(Request),
    [Cmd|Args] = string:tokens(Line, " \r\n"), % Handle all WS here
    Argstr = string:join(Args, " "),
    {ok, {Cmd, Args, Argstr}}.

handle_login(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Request} -> 
            {ok, Peer} = inet:peername(Socket),
            {ok, {Cmd, Args, Argstr}} = tokenize(Request),
            log_attempt(Cmd, Args, Argstr, Peer),
            case {Cmd, Args} of 
                {"connect", [Username|_]} -> 
                    case authorize(Username) of
                        false ->
                            gen_tcp:send(
                                Socket,
                                <<"Mmmm. That doesn't seem right.\n">>),
                            handle_login(Socket);
                        Player ->
                            ets:insert(connections, {Player, {Socket, Peer}}),
                            Name = object:get_property(Player, ?NAME),
                            error_logger:info_msg("~s logged in from ~p~n", [Name, Peer]),
                            Msg = io_lib:format(
                                "*** Connected (~s) ***~n", 
                                [Name]),
                            gen_tcp:send(Socket, Msg),
                            handle({Socket, Peer})
                    end;
                _Other ->
                    log_attempt(Cmd, Args, Argstr, Peer),
                    gen_tcp:send(Socket, <<"That would never work.\n">>),
                    handle_login(Socket)
            end;
        {tcp_closed, Socket} ->
            error_logger:info_msg(
                "Client ~p disconnected (from login)~n", 
                [Socket])
    end.

handle({Socket, Peer}) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"@quit", _/binary>>} ->
            gen_tcp:send(Socket, <<"Bye!\n">>),
            gen_tcp:close(Socket);
        {tcp, Socket, Request} ->


            %% Echo
            gen_tcp:send(Socket, Request),
            handle({Socket, Peer});
        {tcp_closed, Socket} ->
            error_logger:info_msg("Client ~p disconnected.~n", [Peer]);
        Junk ->
            error_logger:info_msg("Received junk: ~p~n", [Junk])
    end.

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

log_attempt(Cmd, Args, Argstr, Peer) ->
    error_logger:info_msg(
        "Login attempt with ~p from ~p~n", 
        [{Cmd, Args, Argstr}, Peer]).