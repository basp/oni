%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013 TMG
%%% @end
%%%----------------------------------------------------------------------------
-module(tcp_server).

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

handle({Socket, Peer}) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        %%{tcp, Socket, <<"quit", _/binary>>} ->
        %%   gen_tcp:close(Socket);
        %%{tcp, Socket, <<"foo", _/binary>>} ->
        %%    spawn(fun() -> test:foo(Socket) end),
        %%    handle(Socket);
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, Msg),
            handle({Socket, Peer});
        {tcp_closed, Socket} ->
            error_logger:info_msg("Client ~p disconnected.~n", [Peer]);
        Junk ->
            error_logger:info_msg("Received junk: ~p~n", [Junk])
    end.

handle_login(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} -> 
            {ok, Peer} = inet:peername(Socket),
            Line = binary_to_list(Msg),
            [Cmd|Args] = string:tokens(Line, " \r\n"), % Handle all WS here
            Argstr = string:join(Args, " "),
            case Cmd of 
                "connect" -> 
                    handle({Socket, Peer});
                _Other ->
                    error_logger:info_msg(
                        "Login attempt with ~p from ~p~n", 
                        [{Cmd, Args, Argstr}, Peer]),
                    gen_tcp:send(Socket, <<"That would never work.\n">>),
                    handle_login(Socket)
            end;
        {tcp_closed, Socket} ->
            error_logger:info_msg(
                "Client ~p disconnected (from login)~n", 
                [Socket])
    end.



%% tcp_server:start_link(7777).