-module(gusion_worker_server).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).
-export([start_link/1]).

start_link(Config)->
    gen_server:start_link(?MODULE, [Config], []).

init([Config])->
    #gusion_config{
        names=Names,
        buffer_size=BufferSize,
        delay=Delay,
        tag_size=TagSize,
        pos_size=PosSize,
        index_size=IndexSize}=Config,
    [DataFile, IndexFile, ConfigFile]=Names,
    {ok, TRef}=if
        Delay>0->
            timer:send_interval(Delay, clear_buffer);
        true->
            {ok, nil}
    end,
    DataPos=gusion_util:get_file_size(DataFile),
    {ok, #gusion_worker_server_state{
            data_file=DataFile,
            index_file=IndexFile,
            config_file=ConfigFile,
            delay=Delay,
            max_buffer_size=BufferSize,
            tag_size=TagSize,
            pos_size=PosSize,
            data_buffer= <<>>,
            index_buffer= <<>>,
            data_pos=DataPos,
            index_size=IndexSize,
            timer_ref=TRef
        }}.

handle_call({write, Data}, _From, State=#gusion_worker_server_state{
    data_file=DataFile,
    index_file=IndexFile,
    max_buffer_size=MaxBufferSize,
    data_buffer=DataBuf,
    index_buffer=IndexBuf,
    data_pos=DataPos,
    tag_size=TagSize,
    pos_size=PosSize
})->
    Bin=term_to_binary(Data),
    NewDataBuf= <<DataBuf/binary, Bin/binary>>,
    Seconds=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    IndexBin= <<Seconds:(?TIMESTAMP_SIZE*8), 0:(TagSize*8),
        DataPos:(PosSize*8), (size(Bin)):(PosSize*8)>>,
    NewIndexBuf= <<IndexBuf/binary, IndexBin/binary>>,
    NewDataPos=DataPos+size(Bin),
    BufferSize=size(NewDataBuf),
    if
        BufferSize>=MaxBufferSize->
            write_to_file(NewDataBuf, NewIndexBuf, DataFile, IndexFile),
            %write_to_file(NewDataBuf, NewIndexBuf, DataFd, IndexFd),
            {reply, ok, State#gusion_worker_server_state{
                    data_buffer= <<>>,
                    index_buffer= <<>>,
                    data_pos=NewDataPos
                }};
        true->
            {reply, ok, State#gusion_worker_server_state{
                    data_buffer=NewDataBuf,
                    index_buffer=NewIndexBuf,
                    data_pos=NewDataPos
                }}
    end;
handle_call({read, Index}, _From, State=#gusion_worker_server_state{
    data_file=DataFile,
    index_file=IndexFile,
    tag_size=TagSize,
    pos_size=PosSize,
    index_size=IndexSize
})->
    try
        Reply=gusion_util:get_data(DataFile, IndexFile, TagSize, PosSize,
            IndexSize, Index),
        {reply, {ok, Reply}, State}
    catch
        _:Reason->{reply, {error, Reason}, State}
    end;
handle_call({read_by_time, Start, End}, _From, State=#gusion_worker_server_state{
    data_file=DataFile,
    index_file=IndexFile,
    tag_size=TagSize,
    pos_size=PosSize,
    index_size=IndexSize
})->
    try
        Ret=gusion_util:get_data_by_time(DataFile, IndexFile, TagSize, PosSize,
            IndexSize, Start, End),
        {reply, {ok, Ret}, State}
    catch
        _:Reason->{reply, {error, Reason}, State}
    end;
handle_call(clear_buffer, _From, State=#gusion_worker_server_state{
    data_buffer=DataBuf,
    index_buffer=IndexBuf,
    data_file=DataFile,
    index_file=IndexFile
})->
    try
        ok=clear_buffer(DataBuf, IndexBuf, DataFile, IndexFile),
        {reply, ok, State#gusion_worker_server_state{
                data_buffer= <<>>,
                index_buffer= <<>>
            }}
    catch
        _:Reason->{reply, {error, Reason}, State}
    end;
handle_call(cancel_tref, _From, State)->
    try
        case State#gusion_worker_server_state.timer_ref of
            nil->
                {reply, ok, State};
            TRef->
                {ok, cancel}=timer:cancel(TRef),
                {reply, ok, State#gusion_worker_server_state{timer_ref=nil}}
        end
    catch
        _:_->{reply, failed, State}
    end;
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(clear_buffer, State=#gusion_worker_server_state{
    data_buffer=DataBuf,
    index_buffer=IndexBuf,
    data_file=DataFile,
    index_file=IndexFile
})->
    try
        ok=clear_buffer(DataBuf, IndexBuf, DataFile, IndexFile),
        {noreply, State#gusion_worker_server_state{
                data_buffer= <<>>,
                index_buffer= <<>>
            }}
    catch
        _:_->{noreply, State}
    end;
handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, #gusion_worker_server_state{
    data_buffer=DataBuf,
    index_buffer=IndexBuf,
    data_file=DataFile,
    index_file=IndexFile
})->
    clear_buffer(DataBuf, IndexBuf, DataFile, IndexFile),
    ok.

clear_buffer(DataBuf, IndexBuf, DataFile, IndexFile)->
    try
        if
            DataBuf=/= <<>> andalso IndexBuf=/= <<>> ->
                {ok, _, _}=write_to_file(DataBuf, IndexBuf, DataFile,
                    IndexFile);
            true->
                ok
        end,
        ok
    catch
        _:_->error(clear_buffer_failed)
    end.

write_to_file(DataBuf, IndexBuf, DataFile, IndexFile)->
    try
        {ok, Fd1}=file:open(DataFile, [raw, binary, write, append]),
        {ok, Fd2}=file:open(IndexFile, [raw, binary, write, append]),
        ok=file:pwrite(Fd1, 0, DataBuf),
        ok=file:pwrite(Fd2, 0, IndexBuf),
        file:close(Fd1),
        file:close(Fd2),
        {ok, gusion_util:get_file_size(DataFile),
            gusion_util:get_file_size(IndexFile)}
    catch
        _:Reason->
            {error, Reason}
    end.
