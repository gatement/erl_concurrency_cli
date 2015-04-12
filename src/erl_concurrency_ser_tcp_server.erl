-module(erl_concurrency_ser_tcp_server).
-behaviour(gen_server).
-include("erl_concurrency_ser.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(HEARTBEAT_TIMEOUT, 600000). % in milliseconds

-record(state, {lsocket, socket}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([LSocket]) ->
    ?LOG_INFO("started (pid=~p)", [erlang:self()]),
    erlang:process_flag(trap_exit, true),
    State = #state{lsocket = LSocket},
    {ok, State, 0}.

handle_call(Msg, From, State) ->
    ?LOG_DEBUG("handle_call: msg=~p, from=~p", [Msg, From]),
    {reply, error, State, ?HEARTBEAT_TIMEOUT}.

handle_cast(Msg, State) ->
    ?LOG_DEBUG("handle_cast: msg=~p", [Msg]),
    {noreply, State, ?HEARTBEAT_TIMEOUT}.

handle_info({tcp, _Socket, Data}, State) ->
    process_received_bytes(Data, State),
    {noreply, State, ?HEARTBEAT_TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, {shutdown, tcp_closed}, State};

handle_info(timeout, #state{socket = undefined, lsocket = LSocket} = State) ->
    %% accept new connection
    {ok, Socket} = try
        gen_tcp:accept(LSocket)
    after
        erl_concurrency_ser_tcp_sup:start_child()
    end,
    ?LOG_INFO("accepted conn: ~p ", [Socket]),
    erl_concurrency_ser_server:increase(),
    {noreply, State#state{socket = Socket}, ?HEARTBEAT_TIMEOUT};

handle_info(timeout, State) ->
    {stop, {shutdown, no_heartbeat}, State};

handle_info(Msg, State) ->
    ?LOG_DEBUG("handle_info: msg=~p", [Msg]),
    {noreply, State, ?HEARTBEAT_TIMEOUT}.

terminate(Reason, #state{socket = Socket}) ->
    case Socket of
        undefined -> do_nothing;
        _ -> gen_tcp:close(Socket)
    end,
    erl_concurrency_ser_server:decrease(),
    ?LOG_DEBUG("exit: reason=~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local Functions
%% ===================================================================
process_received_bytes(<<>>, _) ->
    ok;
process_received_bytes(<<Byte1:1/binary, Rest/binary>>, #state{socket = Socket} = State) ->
    ok = gen_tcp:send(Socket, Byte1),
    process_received_bytes(Rest, State).
