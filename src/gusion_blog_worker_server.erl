-module(gusion_blog_worker_server).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).

-record(state, {
        dir::string(),
        blog_state::record(gusion_blog_state),
        wfile_log::log(),
        finding_idle_iter::pid()|nil,
        swap_wfile_timer::ref()
    }).
-define(GET_ITER_PROCESS_TIMEOUT, 500).

-define(write_state_file(Dir, BLogState),
    file:write_file(filename:absname_join(Dir,
            ?state_file_name(BLogState#gusion_blog_state.name)),
        BLogState)).
-define(sup_children(Name), supervisor:which_children(list_to_atom(
            ?iter_sup_name(Name)))).

init([Dir, Name])->
    try
        process_flag(trap_exit, true),
        StateFileName=filename:absname_join(Dir, ?state_file_name(Name)),
        {ok, Bin}=file:read_file(StateFileName),
        {WFileLog, NewBLogState}=swap_wfile(Dir, binary_to_term(Bin)),
        SWTimer=timer:send_interval(
            NewBLogState#gusion_blog_state.swap_wfile_interval, swap_wfile),
        {ok, _Sup}=gusion_blog_iter_sup:start_link(Name, self(),
            NewBLogState#gusion_blog_state.iter_num),
        {ok, #state{
                finding_idle_iter=nil,
                dir=Dir,
                swap_wfile_timer=SWTimer,
                blog_state=NewBLogState,
                wfile_log=WFileLog}}
    catch
        Type:What->{stop, {Type, What}}
    end.

handle_call({set_swap_wfile_interval, Time}, _From, State=#state{
        dir=Dir,
        swap_wfile_timer=SWTimer,
        blog_state=BLogState
    })->
    {ok, cancel}=timer:cancel(SWTimer),
    NewBLogState=BLogState#gusion_blog_state{swap_wfile_interval=Time},
    ok=?write_state_file(Dir, NewBLogState),
    NewSWTimer=timer:send_interval(Time, swap_wfile),
    {reply, ok, State#state{
            blog_state=NewBLogState,
            swap_wfile_timer=NewSWTimer}};
handle_call({write_blog, Term}, _From, State)->
    Reply=case disk_log:blog(State#state.wfile_log, term_to_binary(Term)) of
        ok->ok;
        {error, Reason}->{error, Reason}
    end,
    {ok, Reply, State};
handle_call({remove_process_blog, PFile}, _From, State)->
    #state{
        dir=Dir,
        blog_state=BLogState
    }=State,
    #gusion_blog_state{
        pfiles=PFiles,
        rm_func={RModule, RFunc}
    }=BLogState,
    ok=apply(RModule, RFunc, [Dir, PFile]),
    NewBLogState=BLogState#gusion_blog_state{
        pfiles=?set_del_element(PFile, PFiles)},
    ok=?write_state_file(Dir, BLogState),
    {reply, ok, State#state{blog_state=NewBLogState}};
handle_call({new_process_task, nil}, {Finding, _}, State=#state{
        blog_state=BLogState,
        finding_idle_iter=Finding
    })->
    if
        BLogState#gusion_blog_state.ifiles=/=[]->
            gen_server:cast(self(), get_idle_iterator);
        true->ok
    end,
    {reply, ok, State#state{finding_idle_iter=nil}};
handle_call({new_process_task, Iterator}, {Finding, _}, State=#state{
        dir=Dir,
        blog_state=BLogState,
        finding_idle_iter=Finding
    })->
    #gusion_blog_state{
        ifiles=IFiles,
        pfiles=PFiles,
        pro_func=ProFunc,
        chunk_size=ChunkSize
    }=BLogState,
    {NewPFile, NewIFiles}=?set_pull_element(IFiles),
    if
        NewPFile=:=nil->
            {reply, ok, State#state{finding_idle_iter=nil}};
        true->
            ok=gen_server:call(Iterator,
                {new_task, Dir, NewPFile, ProFunc, ChunkSize}),
            NewBLogState=BLogState#gusion_blog_state{
                ifiles=NewIFiles,
                pfiles=?set_add_element(NewPFile, PFiles)
            },
            ok=?write_state_file(Dir, NewBLogState),
            {reply, ok, State#state{
                    blog_state=BLogState,
                    finding_idle_iter=nil}}
    end;
handle_call({new_process_task, _}, _From, State)->
    Finding=State#state.finding_idle_iter,
    if
        is_pid(Finding)->
            case process_info(Finding) of
                undefined->{reply, ok, State#state{finding_idle_iter=nil}};
                _->{reply, ok, State}
            end;
        true->{reply, ok, State#state{finding_idle_iter=nil}}
    end;
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(get_idle_iterator, State=#state{
        finding_idle_iter=nil,
        blog_state=BLogState
    })->
    Self=self(),
    Pid=spawn(fun()->
                Iterator=get_idle_iterator(
                    ?sup_children(BLogState#gusion_blog_state.name)),
                gen_server:call(Self, {new_process_task, Iterator})
        end),
    {noreply, State#state{finding_idle_iter=Pid}};
handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(swap_wfile, State)->
    #state{
        dir=Dir,
        blog_state=BLogState,
        wfile_log=WFileLog
    }=State,
    disk_log:close(WFileLog),
    {NewWFileLog, NewBLogState}=swap_wfile(Dir, BLogState),
    {noreply, State#state{
            wfile_log=NewWFileLog,
            blog_state=NewBLogState}};
handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, State)->
    disk_log:close(State#state.wfile_log),
    ok.

swap_wfile(Dir, BLogState)->
    #gusion_blog_state{
        name=Name,
        wfile=WFile,
        ifiles=IFiles
    }=BLogState,

    NewWFile=Name++"_"++integer_to_list(?timestamp),
    {ok, WFileLog}=disk_log:open([{name, ?data_file_name(NewWFile)},
            {file, filename:absname_join(Dir, ?data_file_name(NewWFile))}]),

    NewIFiles=?set_add_element(WFile, IFiles),
    {ok, Ret}=disk_log:open([{name, ?process_file_name(WFile)},
            {file, filename:absname_join(Dir, ?process_file_name(WFile))}]),
    ok=disk_log:blog(Ret, term_to_binary(start)),
    ok=disk_log:close(Ret),

    NewBLogState=BLogState#gusion_blog_state{wfile=NewWFile, ifiles=NewIFiles},
    ok=?write_state_file(Dir, NewBLogState),

    {WFileLog, NewBLogState}.

get_idle_iterator([])->nil;
get_idle_iterator([{_Id, Child, _Type, _Module}|Tail])->
    try
        case gen_server:call(Child, get_progress, ?GET_ITER_PROCESS_TIMEOUT) of
            idle->{ok, Child};
            _->get_idle_iterator(Tail)
        end
    catch
        _:_->get_idle_iterator(Tail)
    end.
