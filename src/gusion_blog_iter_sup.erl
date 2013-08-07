-module(gusion_blog_iter_sup).
-behaviour(supervisor).
-include("gusion.hrl").
-export([start_link/3]).
-export([init/1]).

start_link(Name, WorkerServer, N) when is_list(Name)->
    supervisor:start_link({local, list_to_atom(?iter_sup_name(Name))},
        ?MODULE, [WorkerServer, N]).

init([WorkerServer, N])->
    {ok, {{one_for_one, 5, 10},
            [{list_to_atom(X), 
                    {gen_server, start_link,
                        [gusion_blog_iterator, [WorkerServer], []]},
                    permanent, 5000, worker, [gusion_blog_iterator]}||
                X<-lists:seq(1, N)]
        }}.
