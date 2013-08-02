-module(gusion_blog_iterator).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).

-record(state, {
        pfile::string(),
        worker_server::pid(),
        data_log::log(),
        data_con::continuation()|start,
        process_log::log(),
        chunk_num::integer()|infinity,
        wf::{atom(), atom()},
        process::busy|compiled
    }).

init([WorkerServer, Dir, PFile, WF, ChunkNum])->
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
                chunk_num=ChunkNum,
                wf=WF,
                process=busy
            }}
    catch
        _:Reason->{stop, Reason}
    end.

handle_call(_Msg, _From, State)->
    {reply, reply, State}.

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
