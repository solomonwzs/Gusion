-module(gusion_util).
-include("gusion.hrl").
-export([cell/1, int_byte_len/1, int_bit_len/1, get_config/1,
        get_file_size/1, get_data/6, error_msg/2, get_data_by_time/7]).

-define(K_DATA_DF, gusion_util_dict_key_1).
-define(K_INDEX_DF, gusion_util_dict_key_2).
-define(K_INDEX_SIZE, gusion_util_dict_key_3).
-define(K_TAG_BITS_SIZE, gusion_util_dict_key_4).
-define(K_POS_BITS_SIZE, gusion_util_dict_key_5).
-define(K_START_SECS, gusion_util_dict_key_6).
-define(K_END_SECS, gusion_util_dict_key_7).

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
    MaxSize=proplists:get_value(max_size, Config, 1024*1024*1024*1024),
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

get_data(DataFile, IndexFile, TagSize, PosSize, IndexSize, Index)->
    IndexBin=get_index(IndexFile, IndexSize, Index),
    TagBitsSize=TagSize*8,
    PosBitsSize=PosSize*8,
    <<_Seconds:(?TIMESTAMP_SIZE*8), _Tag:TagBitsSize,
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

get_index(IndexFile, IndexSize, Index) when is_integer(Index) andalso Index>0 ->
    FileSize=gusion_util:get_file_size(IndexFile),
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

get_data_by_time(DataFile, IndexFile, TagSize, PosSize, IndexSize, Start, End)->
    put(?K_INDEX_SIZE, IndexSize),
    put(?K_START_SECS, calendar:datetime_to_gregorian_seconds(Start)),
    put(?K_END_SECS, calendar:datetime_to_gregorian_seconds(End)),
    put(?K_TAG_BITS_SIZE, TagSize*8),
    put(?K_POS_BITS_SIZE, PosSize*8),
    {ok, DataFd}=file:open(DataFile, [raw, binary]),
    {ok, IndexFd}=file:open(IndexFile, [raw, binary]),
    file:position(DataFd, 0),
    file:position(IndexFd, 0),
    put(?K_DATA_DF, DataFd),
    put(?K_INDEX_DF, IndexFd),
    Ret=get_data_by_time(),
    file:close(DataFd),
    file:close(IndexFd),
    Ret.

get_data_by_time()->
    case file:read(get(?K_INDEX_DF), get(?K_INDEX_SIZE)) of
        eof->
            [];
        {ok, IndexBin}->
            TagBitsSize=get(?K_TAG_BITS_SIZE),
            PosBitsSize=get(?K_POS_BITS_SIZE),
            <<Seconds:(?TIMESTAMP_SIZE*8), _Tag:TagBitsSize,
                _DataPos:PosBitsSize, Bytes:PosBitsSize>> =IndexBin,
            {ok, DataBin}=file:read(get(?K_DATA_DF), Bytes),
            StartSecs=get(?K_START_SECS),
            EndSecs=get(?K_END_SECS),
            if
                StartSecs =< Seconds andalso Seconds =<EndSecs->
                    [binary_to_term(DataBin)|get_data_by_time()];
                Seconds =< StartSecs->
                    get_data_by_time();
                true->
                    []
            end
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
