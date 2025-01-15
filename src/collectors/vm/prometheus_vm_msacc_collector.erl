%% @doc
%% Collects microstate accounting metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#statistics_microstate_accounting">
%%   erlang:statistics(microstate_accounting)
%% </a>.
%%
%% In order for values to increase, microstate
%% accounting must be enabled. This is done by
%% calling <code>erlang:system_flag(microstate_accounting, true).</code>
%%
%% ==Exported metrics==
%% Some metrics are not available by default. They require a VM
%% configured with <code>./configure --with-microstate-accounting=extra</code>.
%%
%% <ul>
%%   <li>
%%     `erlang_vm_msacc_aux_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent handling auxiliary jobs.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_check_io_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent checking for new I/O events.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_emulator_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent executing Erlang processes.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_gc_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent doing garbage collection.
%%     When extra states are enabled this is the time spent
%%     doing non-fullsweep garbage collections.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_other_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent doing unaccounted things.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_port_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent executing ports.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_sleep_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent sleeping.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_alloc_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent managing memory.
%%     Without extra states this time is spread out over all other states.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_bif_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent in BIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_busy_wait_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent busy waiting.
%%     Without extra states this time is part of the 'other' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_ets_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent executing ETS BIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_gc_full_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent doing fullsweep garbage collection.
%%     Without extra states this time is part of the 'gc' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_nif_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent in NIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_send_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent sending messages (processes only).
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang_vm_msacc_timers_seconds_total'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent managing timers.
%%     Without extra states this time is part of the 'other' state.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_msacc_collector_metrics' key of `prometheus' app environment.
%%
%% Options are the same as MSAcc_Thread_State return type from
%% <a href="http://erlang.org/doc/man/erlang.html#statistics_microstate_accounting">
%%   erlang:statistics(microstate_accounting)
%% </a> with `_seconds_total' as the suffix:
%% <ul>
%%   <li>
%%     `aux_seconds_total' for `erlang_vm_msacc_aux_seconds_total'.
%%   </li>
%%   <li>
%%     `check_io_seconds_total' for `erlang_vm_msacc_check_io_seconds_total'.
%%   </li>
%%   <li>
%%     `emulator_seconds_total' for `erlang_vm_msacc_emulator_seconds_total'.
%%   </li>
%%   <li>
%%     `gc_seconds_total' for `erlang_vm_msacc_gc_seconds_total'.
%%   </li>
%%   <li>
%%     `other_seconds_total' for `erlang_vm_msacc_other_seconds_total'.
%%   </li>
%%   <li>
%%     `port_seconds_total' for `erlang_vm_msacc_port_seconds_total'.
%%   </li>
%%   <li>
%%     `sleep_seconds_total' for `erlang_vm_msacc_sleep_seconds_total'.
%%   </li>
%%   <li>
%%     `alloc_seconds_total' for `erlang_vm_msacc_alloc_seconds_total'.
%%   </li>
%%   <li>
%%     `bif_seconds_total' for `erlang_vm_msacc_bif_seconds_total'.
%%   </li>
%%   <li>
%%     `busy_wait_seconds_total' for `erlang_vm_msacc_busy_wait_seconds_total'.
%%   </li>
%%   <li>
%%     `ets_seconds_total' for `erlang_vm_msacc_ets_seconds_total'.
%%   </li>
%%   <li>
%%     `gc_full_seconds_total' for `erlang_vm_msacc_gc_full_seconds_total'.
%%   </li>
%%   <li>
%%     `nif_seconds_total' for `erlang_vm_msacc_nif_seconds_total'.
%%   </li>
%%   <li>
%%     `send_seconds_total' for `erlang_vm_msacc_send_seconds_total'.
%%   </li>
%%   <li>
%%     `timers_seconds_total' for `erlang_vm_msacc_timers_seconds_total'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled as far as Prometheus is concerned,
%% although some metrics could not be enabled by the VM itself.
%% @end
-module(prometheus_vm_msacc_collector).

-export([
    deregister_cleanup/1,
    collect_mf/2
]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(METRIC_NAME_PREFIX, "erlang_vm_msacc_").

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) -> ok.

-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
%% @private
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
    Data = erlang:statistics(microstate_accounting),
    SecondAsPerfCounter = erlang:convert_time_unit(1, second, perf_counter),
    [
        %% Base states.
        {aux_seconds_total, counter, "Total time in seconds spent handling auxiliary jobs.",
            metric(aux, Data, SecondAsPerfCounter)},
        {check_io_seconds_total, counter,
            "Total time in seconds spent checking for new I/O events.",
            metric(check_io, Data, SecondAsPerfCounter)},
        {emulator_seconds_total, counter, "Total time in seconds spent executing Erlang processes.",
            metric(emulator, Data, SecondAsPerfCounter)},
        {gc_seconds_total, counter,
            "Total time in seconds spent doing garbage collection. "
            "When extra states are enabled this is the time spent "
            "doing non-fullsweep garbage collections.", metric(gc, Data, SecondAsPerfCounter)},
        {other_seconds_total, counter, "Total time in seconds spent doing unaccounted things.",
            metric(other, Data, SecondAsPerfCounter)},
        {port_seconds_total, counter, "Total time in seconds spent executing ports.",
            metric(port, Data, SecondAsPerfCounter)},
        {sleep_seconds_total, counter, "Total time in seconds spent sleeping.",
            metric(sleep, Data, SecondAsPerfCounter)},
        %% Extra states.
        {alloc_seconds_total, counter,
            "Total time in seconds spent managing memory. "
            "Without extra states this time is spread out over all other states.",
            metric(alloc, Data, SecondAsPerfCounter)},
        {bif_seconds_total, counter,
            "Total time in seconds spent in BIFs. "
            "Without extra states this time is part of the 'emulator' state.",
            metric(bif, Data, SecondAsPerfCounter)},
        {busy_wait_seconds_total, counter,
            "Total time in seconds spent busy waiting. "
            "Without extra states this time is part of the 'other' state.",
            metric(busy_wait, Data, SecondAsPerfCounter)},
        {ets_seconds_total, counter,
            "Total time in seconds spent executing ETS BIFs. "
            "Without extra states this time is part of the 'emulator' state.",
            metric(ets, Data, SecondAsPerfCounter)},
        {gc_full_seconds_total, counter,
            "Total time in seconds spent doing fullsweep garbage collection. "
            "Without extra states this time is part of the 'gc' state.",
            metric(gc_full, Data, SecondAsPerfCounter)},
        {nif_seconds_total, counter,
            "Total time in seconds spent in NIFs. "
            "Without extra states this time is part of the 'emulator' state.",
            metric(nif, Data, SecondAsPerfCounter)},
        {send_seconds_total, counter,
            "Total time in seconds spent sending messages (processes only). "
            "Without extra states this time is part of the 'emulator' state.",
            metric(send, Data, SecondAsPerfCounter)},
        {timers_seconds_total, counter,
            "Total time in seconds spent managing timers. "
            "Without extra states this time is part of the 'other' state.",
            metric(timers, Data, SecondAsPerfCounter)}
    ].

metric(Counter, Data, SecondAsPerfCounter) ->
    [
        {[{type, Type}, {id, ID}], Value / SecondAsPerfCounter}
     || #{type := Type, id := ID, counters := #{Counter := Value}} <- Data
    ].

enabled_metrics() ->
    application:get_env(prometheus, vm_msacc_collector_metrics, all).

metric_enabled(Name, Metrics) ->
    Metrics =:= all orelse lists:member(Name, Metrics).
