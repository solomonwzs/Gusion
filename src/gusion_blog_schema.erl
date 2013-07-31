
-module(gusion_blog_schema).
-behaviour(gen_server).
-include("gusion.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
        terminate/2]).

init([Dir])->
    process_flag(trap_exit, true),
    SchemaFile=filename:absname_join(Dir, "schema"),
    {ok, Bin}=file:read_file(SchemaFile),
    {ok, #gusion_blog_schema{dir=Dir, blog_dict=binary_to_term(Bin)}}.

handle_call({update_blog, BLogName, BLogState}, _From, State)->
    #gusion_blog_schema{dir=Dir, blog_dict=BLogDict}=State,
    NewBLogDict=dict:store(BLogName, BLogState, BLogDict),
    SchemaFile=filename:absname_join(Dir, "schema"),
    file:write_file(SchemaFile, term_to_binary(NewBLogDict), [write]),
    {reply, ok,  State#gusion_blog_schema{blog_dict=NewBLogDict}};
handle_call({delete_blog, BLogName}, _From, State)->
    #gusion_blog_schema{dir=Dir, blog_dict=BLogDict}=State,
    NewBLogDict=dict:erase(BLogName, BLogDict),
    SchemaFile=filename:absname_join(Dir, "schema"),
    file:write_file(SchemaFile, term_to_binary(NewBLogDict), [write]),
    {reply, ok,  State#gusion_blog_schema{blog_dict=NewBLogDict}};
handle_call(_Msg, _From, State)->
    {reply, reply, State}.

handle_cast(_Msg, State)->
    {noreply, State}.

handle_info(_Msg, State)->
    {noreply, State}.

code_change(_Vsn, State, _Extra)->
    {ok, State}.

terminate(_Reason, _State)->ok.
