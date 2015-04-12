-module(erl_concurrency_cli).
-include("erl_concurrency_cli.hrl").

-export([start/0]).

%% ===================================================================
%% API callbacks
%% ===================================================================
start() ->
    mnesia:start(),
    erl_concurrency_cli_db:init(),

    lager:start(),
    application:start(erl_concurrency_cli),

    erl_concurrency_cli_server:start_test(),

    ok.
