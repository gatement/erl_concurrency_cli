-module(erl_concurrency_cli_server).
-include("erl_concurrency_cli.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start_test/0, write_log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_test() ->
    gen_server:cast(?MODULE, start_test).

write_log(RespTime) ->
    gen_server:cast(?MODULE, {write_log, RespTime}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
    State = #state{},
    {ok, State}.

handle_call(Msg, From, State) ->
    ?LOG_DEBUG("handle_call: msg=~p, from=~p", [Msg, From]),
    {reply, error, State}.

handle_cast(start_test, State) ->
    start_test_inner(),
    {noreply, State};

handle_cast({write_log, RespTime}, State) ->
    write_log_inner(RespTime),
    {noreply, State};

handle_cast(Msg, State) ->
    ?LOG_DEBUG("handle_cast: msg=~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    ?LOG_DEBUG("handle_info: msg=~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Functions
%% ===================================================================
start_test_inner() ->
	{ok, OnlineInterval} = application:get_env(interval_online),
	{ok, Count} = application:get_env(client_count),

    start_client(Count, OnlineInterval).

start_client(0, _) ->
    ok;
start_client(Count, OnlineInterval) ->
    timer:sleep(OnlineInterval),
    erl_concurrency_cli_tcp_sup:start_child(),
    start_client(Count - 1, OnlineInterval).

write_log_inner(RespTime) ->
    At = calendar:local_time(),
	{ok, Verbose} = application:get_env(verbose),
	case Verbose of
		true ->
            case RespTime of
                timeout ->
                    ?LOG_DEBUG("log [~s]: ~p", [At, RespTime]);
                _ ->
                    ?LOG_DEBUG("log [~s]: ~p macrosecs", [At, RespTime])
            end;
        _ ->
            continue
    end,
    model_log:create(#log{id = erlang:make_ref(), at = At, resp_time = RespTime}).
