-module(gusion_util).
-include("gusion.hrl").
-export([get_config/1, get_file_size/1, get_data_by_index/6, error_msg/2]).

-define(int_byte_len(Int), ceil(math:log(Int)/math:log(2)/8)).
-define(int_bit_len(Int), ?int_byte_len(Int)*8).

ceil(F)->
    I=round(F),
    if
        F>I->I+1;
        true->I
    end.

get_config(Config)->
    Name=proplists:get_value(name, Config),
    BufferSize=proplists:get_value(buffer, Config, 0),
    Delay=proplists:get_value(delay, Config, 1000),
    Dir=proplists:get_value(dir, Config, "./data"),
    TagSize=proplists:get_value(tag_size, Config, 3),
    MaxSize=proplists:get_value(max_size, Config, 1024*1024*1024*1024),
    PosSize=?int_byte_len(MaxSize),
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

get_data_by_index(DataFile, IndexFile, TagSize, PosSize, IndexSize, Index)->
    IndexBin=get_index(IndexFile, IndexSize, Index),
    TagBitsSize=TagSize*8,
    PosBitsSize=PosSize*8,
    <<_Seconds:(?TIMESTAMP_SIZE*8), _Tag:TagBitsSize,
        DataPos:PosBitsSize, Bytes:PosBitsSize>> =IndexBin,
    get_data(DataFile, DataPos, Bytes).

get_data(DataFile, DataPos, Bytes) when is_integer(DataPos)->
    {ok, Fd}=file:open(DataFile, [raw, binary]),
    Ret=file:pread(Fd, DataPos, Bytes),
    file:close(Fd),
    case Ret of
        {ok, Bin}->
            binary_to_term(Bin);
        {error, Reason}->
            error(Reason)
    end.

get_index(IndexFile, IndexSize, Index) when is_integer(Index) andalso Index>0 ->
    FileSize=get_file_size(IndexFile),
    Offset=(Index-1)*IndexSize,
    if
        Offset>=FileSize->
            error(index_out_of_bound);
        true->
            {ok, Fd}=file:open(IndexFile, [raw, binary]),
            {ok, Bin}=file:pread(Fd, Offset, IndexSize),
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
