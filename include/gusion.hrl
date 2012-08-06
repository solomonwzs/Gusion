-ifndef(GUSION_INCLUDED).
-define(GUSION_INCLUDED, 1).

-define(TIMESTAMP_SIZE, 7).

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

-record(gusion_writer_server_state, {
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
        data_pos::integer()
    }).

-record(gusion_index, {
        timestamp::integer(),
        tag::term(),
        data_pos::integer(),
        bytes::integer()
    }).

-endif.
