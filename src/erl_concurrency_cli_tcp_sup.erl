-module(erl_concurrency_cli_tcp_sup).
-include("erl_concurrency_cli.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
	{ok, _} = supervisor:start_child(?MODULE, []).

init([]) ->
    Child = ?CHILD(erl_concurrency_cli_tcp_server, worker),  
    {ok, {{simple_one_for_one, 100000000, 1}, [Child]}}.

%% ===================================================================
%% Local Functions
%% ===================================================================
