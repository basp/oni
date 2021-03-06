%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com/]
%%% @copyright 2013-2014 Bas Pennings (TMG)
%%% @end
%%%----------------------------------------------------------------------------
-module(aq).

%% API
-export([start_link/0, queue/2, clear/1]).

%%%============================================================================
%%% API
%%%============================================================================
start_link() ->
    spawn_link(fun() -> loop(queue:new()) end).

queue(Pid, MFA) ->
    Pid ! {self(), {queue, MFA}},
    receive
        Any -> Any
    end.

clear(Pid) -> Pid ! clear.

%%%============================================================================
%%% Internal functions
%%%============================================================================
loop(Queue) ->
    receive
        {From, {queue, MFA}} -> 
            NewQueue = queue:in(MFA, Queue),
            case queue:is_empty(Queue) of
                true -> execute(self(), MFA), From ! ok;
                false -> From ! queued
            end,
            loop(NewQueue);
        next ->
            {_Completed, NewQueue} = queue:out(Queue),
            case queue:peek(NewQueue) of
                {value, MFA} -> execute(self(), MFA), ok;
                empty -> ok
            end,
            loop(NewQueue);
        clear ->
            NewQueue = queue:new(),
            loop(NewQueue);
        _Junk -> 
            loop(Queue)
    end.

execute(Aq, MFA) ->
    F = fun() -> 
        R = rt:execute(MFA),
        case R of
            {continue, Time, CMFA} ->
                timer:sleep(Time),
                execute(Aq, CMFA),
                ok;
            _Other -> Aq ! next, ok
        end
    end,
    spawn(F).