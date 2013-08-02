-ifndef(GUSION_INCLUDED).
-define(GUSION_INCLUDED, 1).

-type set()::tuple().
-type gb_set()::tuple().

-type log()::term().
-type continuation()::term().

-define(TIMESTAMP_SIZE, 7).

-define(timestamp, calendar:datetime_to_gregorian_seconds(
        calendar:universal_time())).
-define(set_add_element(Element, Set), sets:add_element(Element, Set)).
-define(set_del_element(Element, Set), sets:del_element(Element, Set)).
-define(set_is_element(Element, Set), sets:is_element(Element, Set)).

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
        pfiles::set(),
        mf::tuple(atom(), atom()),
        chunk_num::integer()|infinity,
        process_interval::integer()
    }).

-record(gusion_blog_schema, {
        dir::string(),
        blog_set::set()
    }).

-endif.
