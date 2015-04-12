-module(erl_concurrency_ser).
-include("erl_concurrency_ser.hrl").

-export([start/0]).

%% ===================================================================
%% API callbacks
%% ===================================================================
start() ->
    lager:start(),
    application:start(erl_concurrency_ser),
    ok.
