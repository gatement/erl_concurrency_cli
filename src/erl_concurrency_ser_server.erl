-module(erl_concurrency_ser_server).
-include("erl_concurrency_ser.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, count/0, increase/0, decrease/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {count = 0}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

count() ->
    gen_server:call(?MODULE, count).

increase() ->
    gen_server:cast(?MODULE, increase).

decrease() ->
    gen_server:cast(?MODULE, decrease).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
    State = #state{},
    {ok, State}.

handle_call(count, _From, #state{count = Count} = State) ->
    {reply, Count, State};

handle_call(Msg, From, State) ->
    ?LOG_DEBUG("handle_call: msg=~p, from=~p", [Msg, From]),
    {reply, error, State}.

handle_cast(increase, #state{count = Count} = State) ->
    ?LOG_DEBUG("increase(+): count=~p", [Count+1]),
    {noreply, State#state{count = Count+1}};

handle_cast(decrease, #state{count = Count} = State) ->
    ?LOG_DEBUG("decrease(-): count=~P", [Count-1]),
    {noreply, State#state{count = Count-1}};

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
