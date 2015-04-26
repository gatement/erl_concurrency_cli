-module(erl_concurrency_cli).
-include("erl_concurrency_cli.hrl").

-export([start/0]).

%% ===================================================================
%% API callbacks
%% ===================================================================
start() ->
    mnesia:start(),
    erl_concurrency_cli_db:init(),
    timer:sleep(5000), %% waiting for mnesia table ready
    model_log:clear(),

    lager:start(),
    application:start(erl_concurrency_cli),

    erl_concurrency_cli_server:start_test(),

    ok.
