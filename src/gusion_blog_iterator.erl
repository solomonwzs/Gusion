-module(gusion_blog_iterator).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).

-define(apply(Func, Arg),
    if
        is_function(Func)->Func(Arg);
        is_tuple(Func)->
            {M, F}=Func,
            apply(M, F, [Arg])
    end).

-record(state, {
        pfile::string(),
        worker_server::pid(),
        data_log::log(),
        data_con::continuation()|start,
        process_log::log(),
        chunk_size::integer()|infinity,
        func::{atom(), atom()}|'fun'(),
        process::busy|compiled
    }).

init([WorkerServer, Dir, PFile, Func, ChunkSize])->
    process_flag(trap_exit, true),
    try
        DataLog=gusion_util:disk_log_open([{name, PFile++".bdat"},
                {file, filename:absname_join(Dir, PFile++".bdat")}]),
        ProcessLog=gusion_util:disk_log_open([{name, PFile++"bpro"},
                {file, filename:absname_join(Dir, PFile++".bpro")}]),
        {ok, #state{
                pfile=PFile,
                worker_server=WorkerServer,
                data_log=DataLog,
                data_con=get_data_con(ProcessLog, start, start),
                process_log=ProcessLog,
                chunk_size=ChunkSize,
                func=Func,
                process=busy
            }}
    catch
        _:Reason->{stop, Reason}
    end.

handle_call(get_bchunk, _From, State=#state{
        data_log=DataLog,
        data_con=DataCon,
        chunk_size=ChunkSize
    })->
    Chunk=case disk_log:bchunk(DataLog, DataCon, ChunkSize) of
        eof->eof;
        {error, Reason}->error(Reason);
        BchunkRet->element(2, BchunkRet)
    end,
    {reply, {ok, Chunk}, State};
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(run, State=#state{
        data_log=DataLog,
        data_con=DataCon,
        chunk_size=ChunkSize,
        func=Func,
        process_log=ProcessLog
    })->
    {NextDataCon, Chunk}=case disk_log:bchunk(DataLog, DataCon, ChunkSize) of
        eof->{nil, eof};
        {error, _Reason}->{DataCon, nil};
        BchunkRet->{element(1, BchunkRet), element(2, BchunkRet)}
    end,
    NewState=case ?apply(Func, Chunk) of
        {ok, Cmd}->
            ok=disk_log:blog(ProcessLog, term_to_binary(NextDataCon)),
            if
                Cmd=:=continue->gen_server:cast(self(), run);
                Cmd=:=finish->gen_server:cast(self(), finish);
                true->ok
            end,
            State#state{data_con=DataCon};
        _->State
    end,
    {noreply, NewState};
%handle_cast(finish, State)->
handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, State)->
    #state{
        pfile=PFile,
        worker_server=WorkerServer,
        data_log=DataLog,
        process_log=ProcessLog,
        process=Process
    }=State,
    disk_log:close(DataLog),
    disk_log:close(ProcessLog),
    if
        Process=:=compiled->
            gen_server:call(WorkerServer, {del_process_blog, PFile});
        true->ok
    end.

get_data_con(ProcessLog, ProcessCon, DataCon)->
    case disk_log:bchunk(ProcessLog, ProcessCon) of
        eof->{ok, DataCon};
        {error, Reason}->error(Reason);
        BchunkRet->
            NextCon=element(1, BchunkRet),
            BinList=element(2, BchunkRet),
            get_data_con(ProcessLog, NextCon,
                binary_to_term(lists:last(BinList)))
    end.
