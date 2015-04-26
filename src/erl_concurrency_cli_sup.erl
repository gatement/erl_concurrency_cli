-module(erl_concurrency_cli_sup).
-include("erl_concurrency_cli.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Server = ?CHILD(erl_concurrency_cli_server, worker),
    TcpSup = ?CHILD(erl_concurrency_cli_tcp_sup, supervisor),  
    {ok, {{one_for_one, 1000000, 1}, [Server, TcpSup]}}.
