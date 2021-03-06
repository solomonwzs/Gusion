-module(gusion_blog_iterator).
-behaviour(gen_server).
-include("gusion.hrl").
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).

-record(state, {
        pfile::string()|nil,
        worker_server::pid(),
        data_log::log()|nil,
        data_con::continuation()|start|nil,
        process_log::log()|nil,
        chunk_size::integer()|infinity|nil,
        pro_func::{atom(), atom()}|nil,
        progress::busy|idle|stop
    }).

start_link(WorkerServer)->
    gen_server:start_link(?MODULE, [WorkerServer], []).

init([WorkerServer])->
    process_flag(trap_exit, true),
    {ok, #state{worker_server=WorkerServer, progress=idle, _=nil}}.

handle_call({new_task, Dir, PFile, Func, ChunkSize}, _From, State=#state{
        worker_server=_WorkerServer,
        progress=idle
    })->
    DataLog=gusion_util:disk_log_open([{name, ?data_file_name(PFile)},
            {file, filename:absname_join(Dir, ?data_file_name(PFile))}]),
    ProcessLog=gusion_util:disk_log_open([{name, ?process_file_name(PFile)},
            {file, filename:absname_join(Dir, ?process_file_name(PFile))}]),
    {reply, ok, State#state{
            pfile=PFile,
            data_log=DataLog,
            data_con=get_data_con(ProcessLog, start, start),
            process_log=ProcessLog,
            chunk_size=ChunkSize,
            pro_func=Func,
            progress=busy
        }};
handle_call(get_bchunk, _From, State=#state{
        data_log=DataLog,
        data_con=DataCon,
        chunk_size=ChunkSize,
        progress=busy
    })->
    Chunk=case disk_log:bchunk(DataLog, DataCon, ChunkSize) of
        eof->eof;
        {error, Reason}->error(Reason);
        BchunkRet->element(2, BchunkRet)
    end,
    {reply, {ok, Chunk}, State};
handle_call(get_progress, _From, State)->
    {reply, State#state.progress, State};
handle_call(stop, _From, State)->
    {reply, {ok, State#state.pfile}, stop_iterator(State)};
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(stop, State)->
    {noreply, stop_iterator(State)};
handle_cast(run, State=#state{
        pfile=PFile,
        worker_server=WorkerServer,
        data_log=DataLog,
        data_con=DataCon,
        chunk_size=ChunkSize,
        pro_func={PModule, PFunc},
        process_log=ProcessLog,
        progress=busy
    })->
    {NextDataCon, Chunk}=case disk_log:bchunk(DataLog, DataCon, ChunkSize) of
        eof->{nil, eof};
        {error, _Reason}->{DataCon, nil};
        BchunkRet->{element(1, BchunkRet), element(2, BchunkRet)}
    end,
    NewState=case apply(PModule, PFunc, [Chunk]) of
        {ok, Cmd}->
            ok=disk_log:blog(ProcessLog, term_to_binary(NextDataCon)),
            if
                Cmd=:=continue->
                    gen_server:cast(self(), run),
                    State#state{data_con=DataCon, progress=busy};
                Cmd=:=compiled->
                    disk_log:close(DataLog),
                    disk_log:close(ProcessLog),
                    gen_server:call(WorkerServer, {remove_process_blog, PFile}),
                    State#state{worker_server=WorkerServer, progress=idle};
                true->State#state{progress=busy}
            end;
        _->State
    end,
    {noreply, NewState};
handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, #state{
        data_log=DataLog,
        process_log=ProcessLog
    })->
    disk_log:close(DataLog),
    disk_log:close(ProcessLog),
    ok.

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

stop_iterator(#state{
        worker_server=WorkerServer,
        data_log=DataLog,
        process_log=ProcessLog
    })->
    disk_log:close(DataLog),
    disk_log:close(ProcessLog),
    #state{worker_server=WorkerServer, progress=stop, _=nil}.
