-module(prometheus_vm_memory_collector).
-compile({parse_transform, prometheus_pt}).
-moduledoc """
Collects information about memory dynamically allocated by the Erlang emulator using
`erlang:memory/0`, also provides basic (D)ETS statistics.

### Exported metrics

* `erlang_vm_memory_atom_bytes_total{usage=\"free|used\"}`
  Type: gauge.
  The total amount of memory currently allocated for atoms.
    This memory is part of the memory presented as system memory.
* `erlang_vm_memory_bytes_total{kind=\"system|processes\"}`
  Type: gauge.
  The total amount of memory currently allocated.
    This is the same as the sum of the memory size for processes and system.
* `erlang_vm_memory_dets_tables`
  Type: gauge.
  Erlang VM DETS Tables count.
* `erlang_vm_memory_ets_tables`
  Type: gauge.
  Erlang VM ETS Tables count.
* `erlang_vm_memory_processes_bytes_total{usage=\"free|used\"}`
  Type: gauge.
  The total amount of memory currently allocated for the Erlang processes.
* `erlang_vm_memory_system_bytes_total{usage=\"atom|binary|code|ets|other\"}`
  Type: gauge.
  The total amount of memory currently allocated for the emulator that is not directly related
    to any Erlang process. Memory presented as processes is not included in this memory.

### Configuration

Metrics exported by this collector can be configured via `vm_memory_collector_metrics` key
of the `prometheus` app environment.

Available options:

* `atom_bytes_total` for `erlang_vm_memory_atom_bytes_total`.
* `bytes_total` for `erlang_vm_memory_bytes_total`.
* `dets_tables` for `erlang_vm_dets_tables`.
* `ets_tables` for `erlang_vm_ets_tables`.
* `processes_bytes_total` for `erlang_vm_memory_processes_bytes_total`.
* `system_bytes_total` for `erlang_vm_memory_system_bytes_total`.

By default all metrics are enabled.
""".

-export([deregister_cleanup/1, collect_mf/2]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

-define(METRIC_NAME_PREFIX, "erlang_vm_memory_").

-doc false.
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(_) ->
    ok.

-doc false.
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
    Data = erlang:memory(),
    [
        {atom_bytes_total, gauge,
            "The total amount of memory currently allocated "
            "for atoms. This memory is part of the memory "
            "presented as system memory.", [
                {[{usage, used}], proplists:get_value(atom_used, Data)},
                {
                    [{usage, free}],
                    proplists:get_value(atom, Data) - proplists:get_value(atom_used, Data)
                }
            ]},
        {bytes_total, gauge,
            "The total amount of memory currently allocated. "
            "This is the same as the sum of the memory size "
            "for processes and system.", [
                {[{kind, system}], proplists:get_value(system, Data)},
                {[{kind, processes}], proplists:get_value(processes, Data)}
            ]},
        {dets_tables, gauge, "Erlang VM DETS Tables count.", length(dets:all())},
        {ets_tables, gauge, "Erlang VM ETS Tables count.", length(ets:all())},
        {processes_bytes_total, gauge,
            "The total amount of memory currently allocated "
            "for the Erlang processes.", [
                {[{usage, used}], proplists:get_value(processes_used, Data)},
                {
                    [{usage, free}],
                    proplists:get_value(processes, Data) - proplists:get_value(processes_used, Data)
                }
            ]},
        {system_bytes_total, gauge,
            "The total amount of memory currently allocated "
            "for the emulator that is not directly related "
            "to any Erlang process. Memory presented as processes "
            "is not included in this memory.", [
                {[{usage, atom}], proplists:get_value(atom, Data)},
                {[{usage, binary}], proplists:get_value(binary, Data)},
                {[{usage, code}], proplists:get_value(code, Data)},
                {[{usage, ets}], proplists:get_value(ets, Data)},
                {[{usage, other}], memory_other(Data)}
            ]}
    ].

memory_other(Data) ->
    proplists:get_value(system, Data) -
        proplists:get_value(atom, Data) -
        proplists:get_value(binary, Data) -
        proplists:get_value(code, Data) -
        proplists:get_value(ets, Data).

enabled_metrics() ->
    application:get_env(prometheus, vm_memory_collector_metrics, all).

metric_enabled(Name, Metrics) ->
    Metrics =:= all orelse lists:member(Name, Metrics).
