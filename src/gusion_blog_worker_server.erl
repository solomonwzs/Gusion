-module(gusion_blog_worker_server).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).

-record(state, {
        dir::string(),
        blog_state::record(gusion_blog_state),
        wfile_log::term()
    }).

init([Dir, Name])->
    process_flag(trap_exit, true),

    StateFileName=filename:absname_join(Dir, Name++'.bsta'),
    {ok, Bin}=file:read_file(StateFileName),
    BLogState=binary_to_term(Bin),
    #gusion_blog_state{name=Name, wfile=WFile, pfiles=PFiles}=BLogState,

    NewWFile=Name++"_"++integer_to_list(?timestamp),
    {ok, WFileLog}=disk_log:open([{name, NewWFile++".bdat"},
            {file, filename:absname_join(Dir, NewWFile++"bdat")}]),

    NewPFiles=PFiles++[WFile],
    {ok, Ret}=disk_log:open([{name, PFiles++".bpro"},
            {file, filename:absname_join(Dir, PFiles++".bpro")}]),
    ok=disk_log:blog(Ret, integer_to_binary(0)),
    ok=disk_log:close(Ret),

    NewBLogState=BLogState#gusion_blog_state{wfile=NewWFile, pfiles=NewPFiles},
    file:write_file(StateFileName, term_to_binary(NewBLogState)),

    {ok, #state{dir=Dir, blog_state=NewBLogState, wfile_log=WFileLog}}.

handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, State)->
    disk_log:close(State#state.wfile_log),
    ok.
