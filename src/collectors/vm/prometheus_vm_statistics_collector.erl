-module(prometheus_vm_statistics_collector).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Collects Erlang VM metrics using `erlang:statistics/1`.

### Exported metrics

* `erlang_vm_statistics_bytes_output_total`
  Type: counter.
  The total number of bytes output to ports.
* `erlang_vm_statistics_bytes_received_total`
  Type: counter.
  The total number of bytes received through ports.
* `erlang_vm_statistics_context_switches_total`
  Type: counter.
  The total number of context switches since the system started.
* `erlang_vm_statistics_dirty_cpu_run_queue_length`
  Type: gauge.
  Length of the dirty CPU run-queue.
* `erlang_vm_statistics_dirty_io_run_queue_length`
  Type: gauge.
  Length of the dirty IO run-queue.
* `erlang_vm_statistics_garbage_collection_number_of_gcs_total`
  Type: counter.
  The total number of garbage collections since the system started.
* `erlang_vm_statistics_garbage_collection_words_reclaimed_total`
  Type: counter.
  The total number of words reclaimed by GC since the system started.
* `erlang_vm_statistics_garbage_collection_bytes_reclaimed_total`
  Type: counter.
  The total number of bytes reclaimed by GC since the system started.
* `erlang_vm_statistics_reductions_total`
  Type: counter.
  Total reductions count.
* `erlang_vm_statistics_run_queues_length`
  Type: gauge.
  The total length of all normal run-queues. That is, the number of processes and ports that
    are ready to run on all available normal run-queues.
* `erlang_vm_statistics_runtime_seconds_total`
  Type: counter.
  The sum of the runtime for all threads in the Erlang runtime system.
* `erlang_vm_statistics_wallclock_time_seconds_total`
  Type: counter.
  Can be used in the same manner as `erlang_vm_statistics_runtime_seconds_total`,
    except that real time is measured as opposed to runtime or CPU time.

### Configuration

Metrics exported by this collector can be configured via `vm_statistics_collector_metrics` key
of the `prometheus` app environment.

Options are the same as the `Item` parameter values for `erlang:statistics/1`:

* `context_switches` for `erlang_vm_statistics_context_switches`;
* `garbage_collection` for `erlang_vm_statistics_garbage_collection_number_of_gcs`,
    `erlang_vm_statistics_garbage_collection_bytes_reclaimed`,
    and `erlang_vm_statistics_garbage_collection_words_reclaimed`;
* `io` for `erlang_vm_statistics_bytes_output_total` and `erlang_vm_statistics_bytes_received_total`;
* `reductions` for `erlang_vm_statistics_reductions_total`;
* `run_queue` for `erlang_vm_statistics_run_queues_length`;
* `runtime` for `erlang_vm_statistics_runtime_seconds_total`;
* `wall_clock` for `erlang_vm_statistics_wallclock_time_seconds_total`.

By default all metrics are enabled.
""").

-export([deregister_cleanup/1, collect_mf/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

-define(METRIC_NAME_PREFIX, "erlang_vm_statistics_").

?DOC(false).
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(_) ->
    ok.

?DOC(false).
-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:collect_mf_callback().
collect_mf(_Registry, Callback) ->
    Metrics = metrics(),
    EnabledMetrics = enabled_metrics(),
    [
        add_metric_family(Metric, Callback)
     || {Name, _, _, _} = Metric <- Metrics, metric_enabled(Name, EnabledMetrics)
    ],
    ok.

add_metric_family({Name, Type, Help, Metrics}, Callback) ->
    Callback(prometheus_model_helpers:create_mf(?METRIC_NAME(Name), Help, Type, Metrics)).

%%====================================================================
%% Private Parts
%%====================================================================

metrics() ->
    {{input, Input}, {output, Output}} = erlang:statistics(io),
    {ContextSwitches, _} = erlang:statistics(context_switches),
    [DirtyCPURunQueueLength, DirtyIORunQueueLength] = dirty_stat(),
    {NumberOfGCs, WordsReclaimed, _} = erlang:statistics(garbage_collection),
    WordSize = erlang:system_info(wordsize),
    {ReductionsTotal, _} = erlang:statistics(reductions),
    RunQueuesLength = erlang:statistics(run_queue),
    {Runtime, _} = erlang:statistics(runtime),
    {WallclockTime, _} = erlang:statistics(wall_clock),
    [
        {bytes_output_total, counter, "Total number of bytes output to ports.", Output},
        {bytes_received_total, counter, "Total number of bytes received through ports.", Input},
        {context_switches_total, counter,
            "Total number of context switches "
            "since the system started.", ContextSwitches},
        {dirty_cpu_run_queue_length, gauge, "Length of the dirty CPU run-queue.",
            DirtyCPURunQueueLength},
        {dirty_io_run_queue_length, gauge, "Length of the dirty IO run-queue.",
            DirtyIORunQueueLength},
        {garbage_collection_number_of_gcs_total, counter, "Garbage collection: number of GCs.",
            NumberOfGCs},
        {garbage_collection_bytes_reclaimed_total, counter, "Garbage collection: bytes reclaimed.",
            WordsReclaimed * WordSize},
        {garbage_collection_words_reclaimed_total, counter, "Garbage collection: words reclaimed.",
            WordsReclaimed},
        {reductions_total, counter, "Total reductions.", ReductionsTotal},
        {run_queues_length, gauge, "Length of normal run-queues.", RunQueuesLength},
        {runtime_seconds_total, counter,
            "The sum of the runtime for all threads "
            "in the Erlang runtime system. "
            "Can be greater than wall clock time.", Runtime},
        {wallclock_time_seconds_total, counter,
            "Information about wall clock. "
            "Same as erlang_vm_statistics_runtime_seconds "
            "except that real time is measured.", WallclockTime}
    ].

dirty_stat() ->
    try
        SO = erlang:system_info(schedulers_online),
        RQ = erlang:statistics(run_queue_lengths_all),
        case length(RQ) > SO of
            true -> lists:sublist(RQ, length(RQ) - 1, 2);
            false -> [undefined, undefined]
        end
    catch
        _:_ -> [undefined, undefined]
    end.

enabled_metrics() ->
    application:get_env(prometheus, vm_statistics_collector_metrics, all).

metric_enabled(Name, Metrics) ->
    Metrics =:= all orelse lists:member(Name, Metrics).
