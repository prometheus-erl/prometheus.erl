-module(prometheus_mnesia_collector).
-compile({parse_transform, prometheus_pt}).
-moduledoc """
Collects Mnesia metrics mainly using `mnesia:system_info/1`.

### Exported metrics

* `erlang_mnesia_held_locks`
  Type: gauge.
  Number of held locks.
* `erlang_mnesia_lock_queue`
  Type: gauge.
  Number of transactions waiting for a lock.
* `erlang_mnesia_transaction_participants`
  Type: gauge.
  Number of participant transactions.
* `erlang_mnesia_transaction_coordinators`
  Type: gauge.
  Number of coordinator transactions.
* `erlang_mnesia_failed_transactions`
  Type: counter.
  Number of failed (i.e. aborted) transactions.
* `erlang_mnesia_committed_transactions`
  Type: gauge.
  Number of committed transactions.
* `erlang_mnesia_logged_transactions`
  Type: counter.
  Number of transactions logged.
* `erlang_mnesia_restarted_transactions`
  Type: counter.
  Total number of transaction restarts.
* `erlang_mnesia_memory_usage_bytes`
  Type: gauge.
  Total number of bytes allocated by all mnesia tables.
* `erlang_mnesia_tablewise_memory_usage_bytes'
  Type: gauge.
  Number of bytes allocated per mnesia table
* `erlang_mnesia_tablewise_size'
  Type: gauge.
  Number of rows present per table

### Configuration

Metrics exported by this collector can be configured via `mnesia_collector_metrics` key
of the `prometheus` app environment.

Available options:
- `held_locks` for `erlang_mnesia_held_locks`;
- `lock_queue` for `erlang_mnesia_lock_queue`;
- `transaction_participants` for `erlang_mnesia_transaction_participants`;
- `transaction_coordinators` for `erlang_mnesia_transaction_coordinators`;
- `transaction_failures` for `erlang_mnesia_failed_transactions`;
- `transaction_commits` for `erlang_mnesia_committed_transactions`;
- `transaction_log_writes` for `erlang_mnesia_logged_transactions`;
- `transaction_restarts` for `erlang_mnesia_restarted_transactions`;
- `memory_usage_bytes` for `erlang_mnesia_memory_usage_bytes`.

By default all metrics are enabled.
""".

-export([deregister_cleanup/1, collect_mf/2]).

-import(prometheus_model_helpers, [create_mf/4]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

-define(METRIC_NAME_PREFIX, "erlang_mnesia_").

-doc false.
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(_) ->
    ok.

-doc false.
-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:collect_mf_callback().
collect_mf(_Registry, Callback) ->
    case mnesia_running() of
        true ->
            EnabledMetrics = enabled_metrics(),
            Metrics = metrics(EnabledMetrics),
            [
                add_metric_family(Metric, Callback)
             || {Name, _, _, _} = Metric <- Metrics, metric_enabled(Name, EnabledMetrics)
            ];
        false ->
            ok
    end,
    ok.

add_metric_family({Name, Type, Help, Metrics}, Callback) ->
    Callback(create_mf(?METRIC_NAME(Name), Help, Type, catch_all(Metrics))).

%%====================================================================
%% Private Parts
%%====================================================================

metrics(EnabledMetrics) ->
    {Participants, Coordinators} = get_tm_info(EnabledMetrics),
    [
        {held_locks, gauge, "Number of held locks.", fun() -> ets:info(mnesia_held_locks, size) end},
        {lock_queue, gauge, "Number of transactions waiting for a lock.", fun() ->
            ets:info(mnesia_lock_queue, size)
        end},
        {transaction_participants, gauge, "Number of participant transactions.", fun() ->
            Participants
        end},
        {transaction_coordinators, gauge, "Number of coordinator transactions.", fun() ->
            Coordinators
        end},
        {failed_transactions, counter, "Number of failed (i.e. aborted) transactions.", fun() ->
            mnesia:system_info(transaction_failures)
        end},
        {committed_transactions, counter, "Number of committed transactions.", fun() ->
            mnesia:system_info(transaction_commits)
        end},
        {logged_transactions, counter, "Number of transactions logged.", fun() ->
            mnesia:system_info(transaction_log_writes)
        end},
        {restarted_transactions, counter, "Total number of transaction restarts.", fun() ->
            mnesia:system_info(transaction_restarts)
        end},
        {memory_usage_bytes, gauge, "Total number of bytes allocated by all mnesia tables", fun() ->
                get_memory_usage()
            end},
        {tablewise_memory_usage_bytes, gauge, "Number of bytes allocated per mnesia table", fun() ->
                get_tablewise_memory_usage()
            end},
        {tablewise_size, gauge, "Number of rows present per table", fun() ->
            get_tablewise_size()
        end}
    ].

%%====================================================================
%% Private Parts
%%====================================================================

get_tm_info(SetMetrics) ->
    case tm_metrics_enabled(SetMetrics) of
        true ->
            prometheus_mnesia:tm_info();
        _ ->
            {undefined, undefined}
    end.

tm_metrics_enabled(SetMetrics) ->
    metric_enabled(transaction_participants, SetMetrics) orelse
        metric_enabled(transaction_coordinators, SetMetrics).

catch_all(DataFun) ->
    try
        DataFun()
    catch
        _:_ -> undefined
    end.

mnesia_running() ->
    erlang:function_exported(mnesia, system_info, 1) andalso
        mnesia:system_info(is_running) == yes.

enabled_metrics() ->
    application:get_env(prometheus, mnesia_collector_metrics, all).

metric_enabled(Name, Metrics) ->
    Metrics =:= all orelse lists:member(Name, Metrics).

get_memory_usage() ->
    WordSize = erlang:system_info(wordsize),
    Calculator = fun(Tab, Sum) ->
        table_info(Tab, memory) + Sum
    end,
    lists:foldl(Calculator, 0, mnesia:system_info(tables)) * WordSize.

get_tablewise_memory_usage() ->
    WordSize = erlang:system_info(wordsize),
    Calculator =
        fun(Tab, Acc) ->
            [{[{table, Tab}], table_info(Tab, memory) * WordSize} | Acc]
        end,
    lists:foldl(Calculator, [], mnesia:system_info(tables)).

get_tablewise_size() ->
    Calculator =
        fun(Tab, Acc) ->
            [{[{table, Tab}], table_info(Tab, size)} | Acc]
        end,
    lists:foldl(Calculator, [], mnesia:system_info(tables)).

%% mnesia:table_info/2 may return 'undefined' when the table should
%% be loaded on the local node but hasn't been loaded yet.
%%
%% https://github.com/erlang/otp/issues/5830
table_info(Tab, Item) ->
    case mnesia:table_info(Tab, Item) of
        undefined -> 0;
        Val -> Val
    end.
