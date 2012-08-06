-module(gusion_util).
-include("gusion.hrl").
-export([cell/1, int_byte_len/1, int_bit_len/1, get_config/1,
        get_file_size/1, timestamp/0, get_data/6, error_msg/2]).

cell(F)->
    I=round(F),
    if
        F>I->I+1;
        true->I
    end.

int_byte_len(I)->
    F=math:log(I)/math:log(2)/8,
    cell(F).

int_bit_len(I)->
    int_byte_len(I)*8.

get_config(Config)->
    Name=proplists:get_value(name, Config),
    BufferSize=proplists:get_value(buffer, Config, 0),
    Delay=proplists:get_value(delay, Config, 1000),
    Dir=proplists:get_value(dir, Config, "./data"),
    TagSize=proplists:get_value(tag_size, Config, 3),
    MaxSize=proplists:get_value(max_size, Config, 10*1024*1024*1024),
    PosSize=gusion_util:int_byte_len(MaxSize),
    DataAbsName=filename:absname_join(Dir, lists:concat([Name, ".dat"])),
    IndexAbsName=filename:absname_join(Dir, lists:concat([Name, ".ind"])),
    ConfigAbsName=filename:absname_join(Dir, lists:concat([Name, ".con"])),
    #gusion_config{
        name=Name,
        names=[DataAbsName, IndexAbsName, ConfigAbsName],
        buffer_size=BufferSize,
        delay=Delay,
        dir=Dir,
        tag_size=TagSize,
        pos_size=PosSize,
        index_size=?TIMESTAMP_SIZE+TagSize+PosSize*2
    }.

get_file_size(File)->
    {ok, Info}=file:read_file_info(File),
    element(2, Info).

timestamp()->
    {MegaSecs, Secs, MicroSecs}=now(),
    MegaSecs*100000000000+Secs*1000000+MicroSecs.

get_data(DataFile, IndexFile, TagSize, PosSize, IndexSize, Index)->
    IndexBin=get_index(IndexFile, IndexSize, Index),
    TagBitsSize=TagSize*8,
    PosBitsSize=PosSize*8,
    <<_TimeStamp:(?TIMESTAMP_SIZE*8), _Tag:TagBitsSize,
        DataPos:PosBitsSize, Bytes:PosBitsSize>> =IndexBin,
    get_data(DataFile, DataPos, Bytes).

get_data(DataFile, DataPos, Bytes)->
    {ok, Fd}=file:open(DataFile, [raw, binary]),
    Ret=file:pread(Fd, DataPos, Bytes),
    file:close(Fd),
    case Ret of
        {ok, Bin}->
            binary_to_term(Bin);
        {error, Reason}->
            error(Reason)
    end.

get_index(IndexFile, Size, Index) when is_integer(Index) andalso Index>0 ->
    FileSize=gusion_util:get_file_size(IndexFile),
    Offset=(Index-1)*Size,
    if
        Offset>=FileSize->
            error(index_out_of_bound);
        true->
            {ok, Fd}=file:open(IndexFile, [raw, binary]),
            {ok, Bin}=file:pread(Fd, Offset, Size),
            file:close(Fd),
            Bin
    end.

error_msg(Tag, {Type, Reason})->
    case Tag of
        open->
            case {Type, Reason} of
                {_, {already_started, _}}->
                    {error, data_file_has_opened};
                _->
                    {error, Reason}
            end;
        _->
            {error, Reason}
    end.
