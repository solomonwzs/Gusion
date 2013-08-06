-module(gusion_blog_iter_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
-export([new_iterator/6]).

start_link(Name) when is_list(Name)->
    supervisor:start_link({local, list_to_atom(Name)}, ?MODULE, []).

init([])->
    {ok, {{one_for_one, 5, 10}, []}}.

new_iterator(Name, WorkerServer, Dir, PFile, Func, ChunkSize)->
    ChildSpec={list_to_atom(PFile),
        {gusion_blog_iterator, start_link, 
            [WorkerServer, Dir, PFile, Func, ChunkSize]},
        permanent, 5000, worker, [gusion_blog_iterator]},
    supervisor:start_child(list_to_atom(Name), ChildSpec).
