-module(gusion_example).

-export([gc_start/0, gc_stop/0]).
-export([gc_process/1]).

-define(GC_REGISTER, gusion_drv).
-define(term_to_list(X), binary_to_list(term_to_binary(X))).

gc_start()->
    case erl_ddll:load_driver("./priv", "gusion_example_drv") of
        ok->ok;
        {error, already_loaded}->ok;
        R->
            io:format("~p~n", [R]),
            exit({error, could_not_load_driver})
    end,
    spawn(fun()->init("gusion_example_drv") end).

init(SharedLib)->
    register(?GC_REGISTER, self()),
    Port=open_port({spawn, SharedLib}, []),
    gc_loop(Port).

gc_stop()->
    ?GC_REGISTER!stop.

gc_process(X)->gc_call_port({process, X}).

gc_call_port(Msg)->
    ?GC_REGISTER!{call, self(), Msg},
    receive
        {?GC_REGISTER, Result}->
            Result
    end.

gc_loop(Port)->
    receive
        {call, Caller, {process, X}}->
            Port!{self(), {command, ?term_to_list(X)}},
            receive
                {Port, {data, Data}}->
                    Caller!{?GC_REGISTER, Data}
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
