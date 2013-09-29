-module(gusion_example).

-export([gc_start/1, gc_stop/0]).
-export([gc_twice/1, gc_sum/2]).

-define(GC_REGISTER, gusion_drv).

gc_start(SharedLib)->
    case erl_ddll:load_driver(".", SharedLib) of
        ok->ok;
        {error, already_loaded}->ok;
        R->
            io:format("~p~n", [R]),
            exit({error, could_not_load_driver})
    end,
    spawn(fun()->init(SharedLib) end).

init(SharedLib)->
    register(?GC_REGISTER, self()),
    Port=open_port({spawn, SharedLib}, []),
    gc_loop(Port).

gc_stop()->
    ?GC_REGISTER!stop.

gc_twice(X)->gc_call_port({twice, X}).
gc_sum(X, Y)->gc_call_port({sum, X, Y}).

gc_call_port(Msg)->
    ?GC_REGISTER!{call, self(), Msg},
    receive
        {?GC_REGISTER, Result}->
            Result
    end.

gc_loop(Port)->
    receive
        {call, Caller, Msg}->
            Port!{self(), {command, gc_encode(Msg)}},
            receive
                {Port, {data, Data}}->
                    Caller!{?GC_REGISTER, gc_decode(Data)}
            end,
            gc_loop(Port);
        stop->
            Port!{self(), close},
            receive
                {Port, closed}->
                    exit(normal)
            end;
        {'EXIT', Port, Reason}->
            exit({port_terminated, Reason})
    end.

gc_encode({twice, X})->[1, X];
gc_encode({sum, X, Y})->[2, X, Y].

gc_decode([Int])->Int.
