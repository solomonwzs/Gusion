-module(gusion_example).

-export([gc_start/0, gc_stop/0]).
-export([gc_twice/1, gc_sum/2]).

-define(GC_REGISTER, gc_example).

gc_start()->
    spawn(fun()->
                register(?GC_REGISTER, self()),
                process_flag(trap_exit, true),
                Port=open_port({spawn, "./c_bin/gc_example"}, [{packet, 2}]),
                gc_loop(Port)
        end).

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
