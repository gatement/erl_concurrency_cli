-module(erl_concurrency_cli_db).
-include("erl_concurrency_cli.hrl").
-export([
		init/0
	]).

%% ===================================================================
%% API functions
%% ===================================================================
init() ->
    case  mnesia:system_info(tables) of
        [schema] ->
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            mnesia:create_table(log, [{attributes, record_info(fields, log)}, {disc_copies, [erlang:node()]}]);
        _ -> 
            no_schema_update
    end.
