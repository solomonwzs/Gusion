-module(gusion).
-include("gusion.hrl").
-export([new/1, close/1, write/2, read/2]).
%-compile(export_all).

-spec new(list())->{ok, pid()}|{error, term()}.
new(Config)->
    try
        R=gusion_util:get_config(Config),
        case R#gusion_config.name of
            undefined->error(undefined_name);
            _->ok
        end,
        {ok, Names}=create_data_file(R#gusion_config.dir, R#gusion_config.name,
            Config),
        Id=lists:concat(["_", R#gusion_config.name]),
        ChildSpec={Id, 
            {gusion_worker_server, start_link,
                [new, R#gusion_config{names=Names}]}, 
            permanent, 5000, worker, [gusion_worker_server]},
        case supervisor:start_child(gusion_sup, ChildSpec) of
            {ok, Child}->
                {ok, #gusion_dev{pid=Child, id=Id}};
            {ok, Child, _Info}->
                {ok, #gusion_dev{pid=Child, id=Id}};
            {error, Err}->
                error(Err)
        end
    catch
        _:Reason->
            io:format("~p~n", [erlang:get_stacktrace()]),
            {error, Reason}
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

create_data_file(Dir, Name, Config)->
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
            io:format(Fd3, "~w.", [Config]),
            file:close(Fd1),
            file:close(Fd2),
            file:close(Fd3),
            {ok, [DataAbsName, IndexAbsName, ConfigAbsName]};
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
