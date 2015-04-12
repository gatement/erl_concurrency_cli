-module(erl_concurrency_ser_tcp_sup).
-include("erl_concurrency_ser.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Params), {I, {I, start_link, Params}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    {ok, SupervisorPid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

    %% start child servers for connection listening
    {ok, WorkerCount} = application:get_env(init_tcp_listener_count),
    start_childs(WorkerCount),

    {ok, SupervisorPid}.

start_child() ->
	{ok, _} = supervisor:start_child(?MODULE, []).

init([]) ->
    Port = 10000,
    ?LOG_INFO("starting TCP Listener on ~p:~p", [{0,0,0,0}, Port]),

    SocketOpts = [binary, {active, true}, {reuseaddr, true}, {ip, {0,0,0,0}}],
    {ok, LSocket} = gen_tcp:listen(Port, SocketOpts),

    Child = ?CHILD(erl_concurrency_ser_tcp_server, worker, [LSocket]),  
    {ok, {{simple_one_for_one, 100000000, 1}, [Child]}}.

%% ===================================================================
%% Local Functions
%% ===================================================================
start_childs(0) ->
    ok;
start_childs(WorkerCount) ->
    supervisor:start_child(?MODULE, []),
    start_childs(WorkerCount - 1).
