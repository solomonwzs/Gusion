-module(gusion_util).
-include("gusion.hrl").
-export([cell/1, int_byte_len/1, int_bit_len/1, get_config/1,
        get_file_size/1, timestamp/0]).

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
    #gusion_config{
        name=Name,
        buffer_size=BufferSize,
        delay=Delay,
        dir=Dir,
        tag_size=TagSize,
        max_size=MaxSize
    }.

get_file_size(File)->
    {ok, Info}=file:read_file_info(File),
    element(2, Info).

timestamp()->
    {MegaSecs, Secs, MicroSecs}=now(),
    MegaSecs*100000000000+Secs*1000000+MicroSecs.
