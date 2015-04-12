-module(erl_concurrency_cli_tcp_server).
-include("erl_concurrency_cli.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% type: active | inactive
-record(state, {heartbeat_interval, 
                socket, 
                is_waiting_resp = false, 
                cmd_sent_time}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
	{ok, ReconnectInterval} = application:get_env(interval_reconnect),
	{ok, HeartbeatInterval} = application:get_env(interval_heartbeat),
    State = #state{heartbeat_interval = HeartbeatInterval},
    {ok, State, ReconnectInterval}.

handle_call(Msg, From, #state{heartbeat_interval = HeartbeatInterval} = State) ->
    ?LOG_WARNING("handle_call: msg=~p, from=~p", [Msg, From]),
    {noreply, State, HeartbeatInterval}.

handle_cast(Msg, #state{heartbeat_interval = HeartbeatInterval} = State) ->
    ?LOG_WARNING("handle_cast: msg=~p", [Msg]),
    {noreply, State, HeartbeatInterval}.

handle_info({tcp, _Socket, Data}, #state{heartbeat_interval = HeartbeatInterval} = State) ->
	State2 = handle_received_data(Data, State),
    {noreply, State2, HeartbeatInterval};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, socket_closed, State};

%% connect server
handle_info(timeout, #state{socket = undefined, heartbeat_interval = HeartbeatInterval} = State) ->
	{ok, Host} = application:get_env(host),
	{ok, Port} = application:get_env(port),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {reuseaddr, true}]),

    {noreply, State#state{socket = Socket}, HeartbeatInterval};

%% receive response timeout
handle_info(timeout, #state{is_waiting_resp = true, heartbeat_interval = HeartbeatInterval} = State) ->
    ?LOG_INFO("receive timeout", []),
    RespTime = timeout,
    erl_concurrency_cli_server:write_log(RespTime),
    State2 = State#state{is_waiting_resp = false},
    {noreply, State2, HeartbeatInterval};

%% send heartbeat
handle_info(timeout, #state{is_waiting_resp = false, socket = Socket, heartbeat_interval = HeartbeatInterval} = State) ->
    %?LOG_DEBUG("sending ping", []),
    ok = gen_tcp:send(Socket, <<1>>),

    State2 = State#state{is_waiting_resp = true, cmd_sent_time = epoch_microseconds()},
    {noreply, State2, HeartbeatInterval};

handle_info(Msg, #state{heartbeat_interval = HeartbeatInterval} = State) ->
    ?LOG_WARNING("handle_info: msg=~p", [Msg]),
    {noreply, State, HeartbeatInterval}.

terminate(Reason, #state{socket = Socket}) ->
	case Socket of
		undefined -> do_nothing;
		_ -> gen_tcp:close(Socket)
	end,
	?LOG_INFO("exit: reason=~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Functions
%% ===================================================================
handle_received_data(<<>>, State) ->
    State;

handle_received_data(<<_Byte1:1/binary, Rest/binary>>, #state{is_waiting_resp = true, cmd_sent_time = CmdSentTime} = State) ->
    %?LOG_DEBUG("receive response", []),
    %% log response time
    Now = epoch_microseconds(),
    RespTime = Now - CmdSentTime,
    erl_concurrency_cli_server:write_log(RespTime),
    handle_received_data(Rest, State#state{is_waiting_resp = false});

handle_received_data(<<_Byte1:1/binary, Rest/binary>>, #state{is_waiting_resp = false} = State) ->
    %% ignore the data
    %?LOG_DEBUG("receive unexpected data", []),
    handle_received_data(Rest, State).

epoch_microseconds() ->
    {A1,A2,A3} = erlang:now(),
    A1*1000000000000 + A2*1000000 + A3.
