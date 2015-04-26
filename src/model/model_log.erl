-module(model_log).
-include("erl_concurrency_cli.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
         create/1,
         clear/0,
         min/0,
         max/0,
         avg/0,
         count/0,
         timeout_count/0,
         info/0
        ]).

-define(TABLE_NAME, log).

%% ===================================================================
%% API Functions
%% ===================================================================
create(Model) ->
    Fun = fun() ->
        mnesia:write(Model)
    end,

    {atomic, ok} = mnesia:sync_transaction(Fun).

clear() ->
    {atomic, ok} = mnesia:clear_table(?TABLE_NAME).

min() ->
    Items = get_none_timeout_items(),
    case Items of
        [] -> 0;
        _ -> lists:min(Items)
    end.

max() ->
    Items = get_none_timeout_items(),
    case Items of
        [] -> 0;
        _ -> lists:max(Items)
    end.

avg() ->
    Items = get_none_timeout_items(),
    case Items of
        [] -> 0;
        _ -> lists:sum(Items) div erlang:length(Items)
    end.

count() ->
    Items = get_none_timeout_items(),
    erlang:length(Items).

timeout_count() ->
    Fun = fun() ->
        Items = qlc:e(qlc:q([X#log.resp_time || X <- mnesia:table(?TABLE_NAME), X#log.resp_time == timeout])),
        erlang:length(Items)
    end,
    mnesia:async_dirty(Fun).

info() ->
    [
     {none_timeout_count, ?MODULE:count()},
     {min, ?MODULE:min()},
     {max, ?MODULE:max()},
     {avg, ?MODULE:avg()},
     {timeout_count, ?MODULE:timeout_count()}
    ].

%% ===================================================================
%% Local Functions
%% ===================================================================
get_none_timeout_items() ->
    Fun = fun() ->
        qlc:e(qlc:q([X#log.resp_time || X <- mnesia:table(?TABLE_NAME), X#log.resp_time /= timeout]))
    end,
    mnesia:async_dirty(Fun).
