-module(gusion_blog).
-export([create_schema/0]).

-define(DEFAULT_DIR, "Gusion_BLog."++atom_to_list(node())).

-spec create_schema()->ok|{error, term()}.
create_schema()->ok.
