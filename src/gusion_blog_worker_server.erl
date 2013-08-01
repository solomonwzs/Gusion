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
    {WFileLog, NewBLogState}=swap_wfile(Dir, binary_to_term(Bin)),
    {ok, #state{dir=Dir, blog_state=NewBLogState, wfile_log=WFileLog}}.

handle_call({write_blog, Term}, _From, State)->
    Reply=case disk_log:blog(State#state.wfile_log, term_to_binary(Term)) of
        ok->ok;
        {error, Reason}->{aborted, Reason}
    end,
    {ok, Reply, State};
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(swap_wfile, State)->
    #state{dir=Dir, blog_state=BLogState, wfile_log=WFileLog}=State,
    disk_log:close(WFileLog),
    {NewWFileLog, NewBLogState}=swap_wfile(Dir, BLogState),
    {noreply, State#state{wfile_log=NewWFileLog, blog_state=NewBLogState}};
handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, State)->
    disk_log:close(State#state.wfile_log),
    ok.

swap_wfile(Dir, BLogState)->
    #gusion_blog_state{name=Name, wfile=WFile, pfiles=PFiles}=BLogState,
    StateFileName=filename:absname_join(Dir, Name++'.bsta'),

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

    {WFileLog, NewBLogState}.
