-module(gusion).
-include("gusion.hrl").
-export([new/1, open/2, close/1, write/2, read/2, dirty_read/2]).

-spec new(list())->{ok, record()}|{error, term()}.
new(Config)->
    try
        R=gusion_util:get_config(Config),
        case R#gusion_config.name of
            undefined->error(undefined_name);
            _->ok
        end,
        ok=create_data_file(R),
        get_dev(new, R)
    catch
        Type:Reason->
            gusion_util:error_msg(open, {Type, Reason})
    end.

-spec open(string(), string()|term())->{ok, record()}|{error, term()}.
open(Dir, Name)->
    try
        ConfigAbsName=filename:absname_join(Dir, lists:concat([Name, ".con"])),
        {ok, Fd}=file:open(ConfigAbsName, read),
        {ok, Config}=io:read(Fd, ''),
        file:close(Fd),
        get_dev(open, Config)
    catch
        Type:Reason->
            gusion_util:error_msg(open, {Type, Reason})
    end.

-spec close(record())->ok|{error, term()}.
close(Dev)->
    Id=Dev#gusion_dev.id,
    supervisor:terminate_child(gusion_sup, Id),
    supervisor:delete_child(gusion_sup, Id).

-spec write(record(), term())->ok|{error, term()}.
write(Dev, Data)->
    Pid=Dev#gusion_dev.pid,
    gen_server:call(Pid, {write, Data}).

-spec read(record(), integer())->ok|{error, term()}.
read(Dev, Index)->
    Pid=Dev#gusion_dev.pid,
    gen_server:call(Pid, {read, Index}).

-spec dirty_read(record(), integer())->ok|{error, term()}.
dirty_read(Dev, Index)->
    try
        [DataFile, IndexFile, ConfigFile]=Dev#gusion_dev.files,
        {ok, Fd}=file:open(ConfigFile, read),
        {ok, Config}=io:read(Fd, ''),
        file:close(Fd),
        TagSize=Config#gusion_config.tag_size,
        PosSize=Config#gusion_config.pos_size,
        IndexSize=Config#gusion_config.index_size,
        Ret=gusion_util:get_data(DataFile, IndexFile, TagSize, PosSize,
            IndexSize, Index),
        {ok, Ret}
    catch
        _:Reason->{error, Reason}
    end.

create_data_file(Config=#gusion_config{
    dir=Dir,
    name=Name,
    names=[DataAbsName, IndexAbsName, ConfigAbsName]
})->
    file:make_dir(Dir),
    DataAbsName=filename:absname_join(Dir, lists:concat([Name, ".dat"])),
    IndexAbsName=filename:absname_join(Dir, lists:concat([Name, ".ind"])),
    ConfigAbsName=filename:absname_join(Dir, lists:concat([Name, ".con"])),
    case [file:open(DataAbsName, []), file:open(IndexAbsName, []),
     file:open(ConfigAbsName, [])] of
        [{error, enoent}, {error, enoent}, {error, enoent}]->
            {ok, Fd1}=file:open(DataAbsName, [append]),
            {ok, Fd2}=file:open(IndexAbsName, [append]),
            {ok, Fd3}=file:open(ConfigAbsName, [append]),
            Names=[DataAbsName, IndexAbsName, ConfigAbsName],
            io:format(Fd3, "~w.", [Config#gusion_config{names=Names}]),
            file:close(Fd1),
            file:close(Fd2),
            file:close(Fd3),
            ok;
        [Ret1, Ret2, Ret3]->
            case Ret1 of
                {ok, Fd1}->file:close(Fd1);
                _->do_thing
            end,
            case Ret2 of
                {ok, Fd2}->file:close(Fd2);
                _->do_thing
            end,
            case Ret3 of
                {ok, Fd3}->file:close(Fd3);
                _->do_thing
            end,
            error(create_data_file_failure)
    end.

get_dev(Action, Config=#gusion_config{
    name=Name,
    names=Names
})->
    Id=lists:concat(["_", Name]),
    ChildSpec={Id, 
        {gusion_worker_server, start_link, [Action, Config]},
        permanent, 5000, worker, [gusion_worker_server]},
    case supervisor:start_child(gusion_sup, ChildSpec) of
        {ok, Child}->
            {ok, #gusion_dev{pid=Child, id=Id, files=Names}};
        {ok, Child, _Info}->
            {ok, #gusion_dev{pid=Child, id=Id, files=Names}};
        {error, Err}->
            error(Err)
    end.
