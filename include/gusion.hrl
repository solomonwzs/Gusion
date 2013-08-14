-ifndef(GUSION_INCLUDED).
-define(GUSION_INCLUDED, 1).

-type set()::tuple().
-type gb_set()::tuple().

-type log()::term().
-type continuation()::term().
-type ref()::term().

-define(TIMESTAMP_SIZE, 7).
-define(SCHEMA_SERVER_NAME, 'gb:schema').

-define(timestamp, calendar:datetime_to_gregorian_seconds(
        calendar:universal_time())).
-define(set_add_element(Element, Set), [Set|Element]).
-define(set_del_element(Element, Set), lists:delete(Element, Set)).
-define(set_is_element(Element, Set), lists:member(Element, Set)).
-define(set_get_element(Set), lists:last(Set)).
-define(set_pull_element(Set),
    if
        length(Set)=:=0->{nil, []};
        true->{hd(Set), tl(Set)}
    end).

-define(iter_sup_name(Name), "gbis:"++Name).
-define(worker_server_name(Name), "gbws:"++Name).

-define(state_file_name(Name), Name++".bsta").
-define(data_file_name(Name), Name++".bdat").
-define(process_file_name(Name), Name++".bpro").

-define(sup_children(Name), supervisor:which_children(list_to_atom(
            ?iter_sup_name(Name)))).

-record(gusion_config, {
        name::string(),
        names::list(string()),
        buffer_size::integer(),
        delay::integer(),
        dir::string(),
        tag_size::integer(),
        pos_size::integer(),
        index_size::integer()
    }).

-record(gusion_dev, {
        pid::pid(),
        id::term(),
        files::list(string())
    }).

-record(gusion_worker_server_state, {
        data_file::string(),
        index_file::string(),
        config_file::string(),
        delay::integer(),
        max_buffer_size::integer(),
        data_buffer::binary(),
        index_buffer::binary(),
        tag_size::integer(),
        pos_size::integer(),
        index_size::integer(),
        data_pos::integer(),
        timer_ref::term()
    }).

-record(gusion_index, {
        timestamp::integer(),
        tag::integer(),
        data_pos::integer(),
        bytes::integer()
    }).

-record(gusion_iterator, {
        data_fd::term(),
        index_fd::term(),
        start_sec::'_'|integer(),
        end_sec::'_'|integer(),
        tags::'_'|list(),
        index_size::integer(),
        tag_size::integer(),
        pos_size::integer(),
        index::integer()
    }).

-record(gusion_blog_state, {
        name::string(),
        wfile::string(),
        ifiles::set(),
        pfiles::set(),
        pro_func::{atom(), atom()},
        rm_func::{atom(), atom()},
        chunk_size::integer()|infinity,
        swap_wfile_interval::integer(),
        iter_num::integer()
    }).

-record(gusion_blog_schema, {
        dir::string(),
        blog_set::set()
    }).

-endif.
