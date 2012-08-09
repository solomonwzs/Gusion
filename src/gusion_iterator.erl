-module(gusion_iterator).
-include("gusion.hrl").
-export([new/2, next/1, close/1]).

-spec new(record(), list(tuple()))->{ok, record()}|{error, term()}.
new(Dev, MatchSpec)->
    try
        [DataFd, IndexFd, ConfigFd]=open_file(Dev#gusion_dev.files),
        {ok, Config}=io:read(ConfigFd, ''),
        file:close(ConfigFd),
        Tags=proplists:get_value(tags, MatchSpec, '_'),
        {StartSecs, EndSecs}=case proplists:get_value(time, MatchSpec) of
            undefined->
                {'_', '_'};
            [Start, End]->{
                if 
                    Start=:='_'->'_';
                    true->calendar:datetime_to_gregorian_seconds(Start)
                end,
                if
                    End=:='_'->'_';
                    true->calendar:datetime_to_gregorian_seconds(End)
                end}
        end,
        {ok, #gusion_iterator{
                data_fd=DataFd,
                index_fd=IndexFd,
                start_sec=StartSecs,
                end_sec=EndSecs,
                tags=Tags,
                index_size=Config#gusion_config.index_size,
                tag_size=Config#gusion_config.tag_size,
                pos_size=Config#gusion_config.pos_size,
                index=0
            }}
    catch
        _:Reason->{error, Reason}
    end.

-spec next(record())->{ok, record()}|eof|{error, term()}.
next(#gusion_iterator{
    data_fd=DataFd,
    index_fd=IndexFd,
    start_sec=StartSecs,
    end_sec=EndSecs,
    tags=Tags,
    index_size=IndexSize,
    tag_size=TagSize,
    pos_size=PosSize
})->
    try
        Index=get_index(IndexFd, IndexSize, TagSize*8, PosSize*8, Tags,
            StartSecs, EndSecs),
        case Index of
            eof->
                eof;
            _->
                {ok, Bin}=file:pread(DataFd, Index#gusion_index.data_pos,
                    Index#gusion_index.bytes),
                binary_to_term(Bin)
        end
    catch
        _:Reason->
            io:format("~p~n", [erlang:get_stacktrace()]),
            {error, Reason}
    end.

-spec close(record())->ok.
close(#gusion_iterator{
    data_fd=DataFd,
    index_fd=IndexFd
})->
    file:close(DataFd),
    file:close(IndexFd),
    ok.

get_index(IndexFd, IndexSize, TagBitsSize, PosBitsSize, Tags, StartSecs,
EndSecs)->
    case file:read(IndexFd, IndexSize) of
        eof->
            eof;
        {ok, IndexBin}->
            <<Seconds:(?TIMESTAMP_SIZE*8), Tag:TagBitsSize,
                DataPos:PosBitsSize, Bytes:PosBitsSize>>=IndexBin,
            case check_time(Seconds, StartSecs, EndSecs) andalso
            check_tag(Tag, Tags) of
                true->
                    #gusion_index{
                        timestamp=Seconds,
                        tag=Tag,
                        data_pos=DataPos,
                        bytes=Bytes
                    };
                false->
                    get_index(IndexFd, IndexSize, TagBitsSize, PosBitsSize,
                        Tags, StartSecs, EndSecs)
            end
    end.

check_time(Seconds, StartSecs, EndSecs)->
    case {StartSecs, EndSecs} of
        {'_', '_'}->
            true;
        {'_', EndSecs}->
            Seconds=<EndSecs;
        {StartSecs, '_'}->
            StartSecs=<Seconds;
        {StartSecs, EndSecs}->
            StartSecs=<Seconds andalso Seconds=<StartSecs
    end.

check_tag(Tag, Tags)->
    case Tags of
        '_'->
            true;
        _->
            lists:member(Tag, Tags)
    end.

open_file([DataFile, IndexFile, ConfigFile])->
    try
        {ok, DataFd}=file:open(DataFile, [raw, binary]),
        [IndexFd, ConfigFd]=try
            {ok, IndexFd1}=file:open(IndexFile, [raw, binary]),
            ConfigFd1=try
                {ok, ConfigFd2}=file:open(ConfigFile, [read]),
                ConfigFd2
            catch
                _:_->
                    file:close(DataFd),
                    file:close(IndexFd1),
                    error(open_config_file_failed)
            end,
            [IndexFd1, ConfigFd1]
        catch
            _:_->
                file:close(DataFd),
                error(open_index_file_failed)
        end,
        [DataFd, IndexFd, ConfigFd]
    catch
        _:_->error(open_data_file_failed)
    end.
