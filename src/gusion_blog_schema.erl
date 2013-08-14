-module(gusion_blog_schema).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).

init([Dir])->
    process_flag(trap_exit, true),
    SchemaFile=filename:absname_join(Dir, "schema"),
    BLogSet=case file:read_file(SchemaFile) of
        {ok, Bin}->binary_to_term(Bin);
        {error, enoent}->
            file:make_dir(Dir),
            ok=file:write_file(SchemaFile, term_to_binary(sets:new())),
            sets:new()
    end,
    {ok, #gusion_blog_schema{dir=Dir, blog_set=BLogSet}}.

handle_call({add_blog, BLogName}, _From, State)->
    #gusion_blog_schema{dir=Dir, blog_set=BLogSet}=State,
    case ?set_is_element(BLogName, BLogSet) of
        false->
            SchemaFile=filename:absname_join(Dir, "schema"),
            NewBLogSet=?set_add_element(BLogName, BLogSet),
            ok=file:write_file(SchemaFile, term_to_binary(NewBLogSet, [write])),
            {reply, ok, State#gusion_blog_schema{blog_set=NewBLogSet}};
        true->
            {reply, {aborted, {exists, BLogName}}, State}
    end;
handle_call({del_blog, BLogName}, _From, State)->
    #gusion_blog_schema{dir=Dir, blog_set=BLogSet}=State,
    case ?set_is_element(BLogName, BLogSet) of
        true->
            SchemaFile=filename:absname_join(Dir, "schema"),
            NewBLogSet=?set_del_element(BLogName, BLogSet),
            ok=file:write_file(SchemaFile, term_to_binary(NewBLogSet, [write])),
            {reply, ok, State#gusion_blog_schema{blog_set=NewBLogSet}};
        false->
            {reply, {aborted, {no_exists, BLogName}}, State}
    end;
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, _State)->ok.
