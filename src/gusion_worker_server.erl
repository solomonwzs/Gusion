-module(gusion_worker_server).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).
-export([start_link/2]).

start_link(new, Config)->
    gen_server:start_link(?MODULE, [new, Config], []).

init([new, Config])->
    #gusion_config{
        names=Names,
        buffer_size=BufferSize,
        delay=Delay,
        tag_size=TagSize,
        pos_size=PosSize,
        index_size=IndexSize}=Config,
    [DataFile, IndexFile, ConfigFile]=Names,
    {ok, #gusion_writer_server_state{
            data_file=DataFile,
            index_file=IndexFile,
            config_file=ConfigFile,
            delay=Delay,
            max_buffer_size=BufferSize,
            tag_size=TagSize,
            pos_size=PosSize,
            data_buffer= <<>>,
            index_buffer= <<>>,
            data_pos=0,
            index_size=IndexSize
        }}.

handle_call({write, Data}, _From, State=#gusion_writer_server_state{
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
    TimeStamp=gusion_util:timestamp(),
    IndexBin= <<TimeStamp:(?TIMESTAMP_SIZE*8), 0:(TagSize*8),
        DataPos:(PosSize*8), (size(Bin)):(PosSize*8)>>,
    NewIndexBuf= <<IndexBuf/binary, IndexBin/binary>>,
    NewDataPos=DataPos+size(Bin),
    BufferSize=size(NewDataBuf),
    if
        BufferSize>=MaxBufferSize->
            write_to_file(NewDataBuf, NewIndexBuf, DataFile, IndexFile),
            {reply, ok, State#gusion_writer_server_state{
                    data_buffer= <<>>,
                    index_buffer= <<>>,
                    data_pos=NewDataPos
                }};
        true->
            {reply, ok, State#gusion_writer_server_state{
                    data_buffer=NewDataBuf,
                    index_buffer=NewIndexBuf,
                    data_pos=NewDataPos
                }}
    end;
handle_call({read, Index}, _From, State=#gusion_writer_server_state{
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
        _:Reason->
            %io:format("~p~n", [erlang:get_stacktrace()]),
            {reply, {error, Reason}, State}
    end;
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, _State)->
    ok.

write_to_file(DataBuf, IndexBuf, DataFile, IndexFile)->
    try
        {ok, Fd1}=file:open(DataFile, [append]),
        {ok, Fd2}=file:open(IndexFile, [append]),
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
