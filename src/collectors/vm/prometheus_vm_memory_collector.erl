%% @doc
%% Collects information about memory dynamically allocated
%% by the Erlang emulator using
%% <a href="http://erlang.org/doc/man/erlang.html#memory-0">
%%   erlang:memory/0
%% </a>, also provides basic (D)ETS statistics.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang_vm_memory_atom_bytes_total{usage="free|used"}'<br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for atoms.
%%     This memory is part of the memory presented as system memory.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_bytes_total{kind="system|processes"}'<br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated.
%%     This is the same as the sum of the memory size for processes and system.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_dets_tables'<br/>
%%     Type: gauge.<br/>
%%     Erlang VM DETS Tables count.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_ets_tables'<br/>
%%     Type: gauge.<br/>
%%     Erlang VM ETS Tables count.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_processes_bytes_total{usage="free|used"}'<br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for the Erlang processes.
%%   </li>
%%   <li>
%%     `erlang_vm_memory_system_bytes_total{usage="atom|binary|code|ets|other"}'
%%     <br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for the emulator
%%     that is not directly related to any Erlang process.
%%     Memory presented as processes is not included in this memory.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_memory_collector_metrics' key of `prometheus' app environment.
%%
%% Available options:
%% <ul>
%%   <li>
%%     `atom_bytes_total' for `erlang_vm_memory_atom_bytes_total'.
%%   </li>
%%   <li>
%%     `bytes_total' for `erlang_vm_memory_bytes_total'.
%%   </li>
%%   <li>
%%     `dets_tables' for `erlang_vm_dets_tables'.
%%   </li>
%%   <li>
%%     `ets_tables' for `erlang_vm_ets_tables'.
%%   </li>
%%   <li>
%%     `processes_bytes_total' for `erlang_vm_memory_processes_bytes_total'.
%%   </li>
%%   <li>
%%     `system_bytes_total' for `erlang_vm_memory_system_bytes_total'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled.
%% @end

-module(prometheus_vm_memory_collector).

-export([
    deregister_cleanup/1,
    collect_mf/2
]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(METRIC_NAME_PREFIX, "erlang_vm_memory_").

%%====================================================================
%% Collector API
%%====================================================================

%% @private
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(_) ->
    ok.

-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:collect_mf_callback().
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
