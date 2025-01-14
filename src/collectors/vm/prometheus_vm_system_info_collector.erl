%% @doc
%% Collects Erlang VM metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#system_info-1">
%%   erlang:system_info/1
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang_vm_dirty_cpu_schedulers'<br/>
%%     Type: gauge.<br/>
%%     The number of scheduler dirty CPU scheduler threads used by the emulator.
%%   </li>
%%   <li>
%%     `erlang_vm_dirty_cpu_schedulers_online'<br/>
%%     Type: gauge.<br/>
%%     The number of dirty CPU scheduler threads online.
%%   </li>
%%   <li>
%%     `erlang_vm_dirty_io_schedulers'<br/>
%%     Type: gauge.<br/>
%%     The number of scheduler dirty I/O scheduler threads used by the emulator.
%%   </li>
%%   <li>
%%     `erlang_vm_ets_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of ETS tables allowed.
%%   </li>
%%   <li>
%%     `erlang_vm_logical_processors'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors configured in the system.
%%   </li>
%%   <li>
%%     `erlang_vm_logical_processors_available'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors
%%     available to the Erlang runtime system.
%%   </li>
%%   <li>
%%     `erlang_vm_logical_processors_online'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors online on the system.
%%   </li>
%%   <li>
%%     `erlang_vm_port_count'<br/>
%%     Type: gauge.<br/>
%%     The number of ports currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_port_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing ports at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_process_count'<br/>
%%     Type: gauge.<br/>
%%     The number of processes currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_process_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing processes
%%     at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_schedulers'<br/>
%%     Type: gauge.<br/>
%%     The number of scheduler threads used by the emulator.
%%   </li>
%%   <li>
%%     `erlang_vm_schedulers_online'<br/>
%%     Type: gauge.<br/>
%%     The number of schedulers online.
%%   </li>
%%   <li>
%%     `erlang_vm_smp_support'<br/>
%%     Type: boolean.<br/>
%%     1 if the emulator has been compiled with SMP support, otherwise 0.
%%   </li>
%%   <li>
%%     `erlang_vm_threads'<br/>
%%     Type: boolean.<br/>
%%     1 if the emulator has been compiled with thread support, otherwise 0.
%%   </li>
%%   <li>
%%     `erlang_vm_thread_pool_size'<br/>
%%     Type: gauge.<br/>
%%     The number of async threads in the async thread pool
%%     used for asynchronous driver calls.
%%   </li>
%%   <li>
%%     `erlang_vm_time_correction'<br/>
%%     Type: boolean.<br/>
%%     1 if time correction is enabled, otherwise 0.
%%   </li>
%%   <li>
%%     `erlang_vm_wordsize_bytes'<br/>
%%     Type: gauge.<br/>
%%     The size of Erlang term words in bytes.
%%   </li>
%%   <li>
%%     `erlang_vm_atom_count'<br/>
%%     Type: gauge.<br/>
%%     The number of atom currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_atom_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing atom at the local node.
%%   </li>
%%   <li>
%%     `erlang_vm_allocators'<br/>
%%     Type: gauge.<br/>
%%     Allocated (carriers_size) and used (blocks_size) memory
%%     for the different allocators in the VM. See erts_alloc(3).
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_system_info_collector_metrics' key of `prometheus' app environment.
%%
%% Options are the same as Item parameter values for
%% <a href="http://erlang.org/doc/man/erlang.html#system_info-1">
%%   erlang:system_info/1
%% </a>:
%% <ul>
%%   <li>
%%     `ets_limit' for `erlang_vm_ets_limit'.
%%   </li>
%%   <li>
%%     `logical_processors' for `erlang_vm_logical_processors'.
%%   </li>
%%   <li>
%%     `logical_processors_available' for
%%     `erlang_vm_logical_processors_available'.
%%   </li>
%%   <li>
%%     `logical_processors_online' for `erlang_vm_logical_processors_online'.
%%   </li>
%%   <li>
%%     `port_count' for `erlang_vm_port_count'.
%%   </li>
%%   <li>
%%     `port_limit' for `erlang_vm_port_limit'.
%%   </li>
%%   <li>
%%     `process_count' for `erlang_vm_process_count'.
%%   </li>
%%   <li>
%%     `process_limit' for `erlang_vm_process_limit'.
%%   </li>
%%   <li>
%%     `schedulers' for `erlang_vm_schedulers'.
%%   </li>
%%   <li>
%%     `schedulers_online' for `erlang_vm_schedulers_online'.
%%   </li>
%%   <li>
%%     `smp_support' for `erlang_vm_smp_support'.
%%   </li>
%%   <li>
%%     `threads' for `erlang_threads'.
%%   </li>
%%   <li>
%%     `thread_pool_size' for `erlang_vm_thread_pool_size'.
%%   </li>
%%   <li>
%%     `time_correction' for `erlang_vm_time_correction'.
%%   </li>
%%   <li>
%%     `wordsize_bytes' for `erlang_vm_wordsize_bytes'.
%%   </li>
%%   <li>
%%     `atom_count' for `erlang_vm_atom_count'.
%%   </li>
%%   <li>
%%     `atom_limit' for `erlang_vm_atom_limit'.
%%   </li>
%%   <li>
%%     `allocators' for `erlang_vm_allocators'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled.
%% @end
-module(prometheus_vm_system_info_collector).

-export([
    deregister_cleanup/1,
    collect_mf/2
]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

%%====================================================================
%% Macros
%%====================================================================

-define(METRIC_NAME_PREFIX, "erlang_vm_").

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
     || {Name, _, _} = Metric <- Metrics, metric_enabled(Name, EnabledMetrics)
    ],
    ok.

add_metric_family({Name, Type, Help}, Callback) ->
    Callback(
        prometheus_model_helpers:create_mf(?METRIC_NAME(Name), Help, Type, collect_metrics(Name))
    ).

%%====================================================================
%% Private Parts
%%====================================================================

metrics() ->
    [
        {dirty_cpu_schedulers, gauge,
            "The number of scheduler dirty CPU scheduler "
            "threads used by the emulator."},
        {dirty_cpu_schedulers_online, gauge, "The number of dirty CPU scheduler threads online."},
        {dirty_io_schedulers, gauge,
            "The number of scheduler dirty I/O scheduler "
            "threads used by the emulator."},
        {ets_limit, gauge, "The maximum number of ETS tables allowed."},
        {logical_processors, gauge,
            "The detected number of logical processors "
            "configured in the system."},
        {logical_processors_available, gauge,
            "The detected number of logical processors "
            "available to the Erlang runtime system."},
        {logical_processors_online, gauge,
            "The detected number of logical processors "
            "online on the system."},
        {port_count, gauge,
            "The number of ports currently existing "
            "at the local node."},
        {port_limit, gauge,
            "The maximum number of simultaneously existing ports "
            "at the local node."},
        {process_count, gauge,
            "The number of processes currently existing "
            "at the local node."},
        {process_limit, gauge,
            "The maximum number of simultaneously existing "
            "processes at the local node."},
        {schedulers, gauge, "The number of scheduler threads used by the emulator."},
        {schedulers_online, gauge, "The number of schedulers online."},
        {smp_support, boolean,
            "1 if the emulator has been compiled with SMP "
            "support, otherwise 0."},
        {threads, boolean,
            "1 if the emulator has been compiled with thread "
            "support, otherwise 0."},
        {thread_pool_size, gauge,
            "The number of async threads in the async thread pool "
            "used for asynchronous driver calls."},
        {time_correction, boolean, "1 if time correction is enabled, otherwise 0."},
        {wordsize_bytes, gauge, "The size of Erlang term words in bytes."},
        {atom_count, gauge,
            "The number of atom currently existing "
            "at the local node."},
        {atom_limit, gauge,
            "The maximum number of simultaneously existing "
            "atom at the local node."},
        {allocators, gauge,
            "Allocated (carriers_size) and used (blocks_size) "
            "memory for the different allocators in the VM. "
            "See erts_alloc(3)."}
    ].

collect_metrics(allocators) ->
    collect_allocator_metrics();
collect_metrics(wordsize_bytes) ->
    erlang:system_info(wordsize);
collect_metrics(Name) ->
    try
        case erlang:system_info(Name) of
            unknown -> undefined;
            Value -> Value
        end
    catch
        error:badarg -> undefined
    end.

enabled_metrics() ->
    application:get_env(prometheus, vm_system_info_collector_metrics, all).

metric_enabled(Name, Metrics) ->
    Metrics =:= all orelse lists:member(Name, Metrics).

collect_allocator_metrics() ->
    Metrics = lists:flatten([
        collect_allocator_metrics_(Alloc, Instance, Info)
     || {{Alloc, Instance}, Info} <- allocators()
    ]),
    prometheus_model_helpers:gauge_metrics(Metrics).

collect_allocator_metrics_(Alloc, Instance, Info) ->
    [
        [
            allocator_metric(Alloc, Instance, Kind, Key, KindInfo)
         || Key <- [carriers, carriers_size]
        ] ++
            [
                allocator_blocks_metric(Alloc, Instance, Kind, Key, KindInfo)
             || Key <- [count, size]
            ]
     || {Kind, KindInfo} <- Info,
        (Kind =:= mbcs) orelse (Kind =:= mbcs_pool) orelse (Kind =:= sbcs)
    ].

allocator_metric(Alloc, Instance, Kind, Key, Values) ->
    {
        [{alloc, Alloc}, {instance_no, Instance}, {kind, Kind}, {usage, Key}],
        element(2, lists:keyfind(Key, 1, Values))
    }.

%% Originally copied from recon_alloc.
allocators() ->
    Allocators = erlang:system_info(alloc_util_allocators),
    %% versions is deleted in order to allow the use of the orddict api,
    %% and never really having come across a case where it was useful to know.
    [
        {{A, N}, lists:sort(proplists:delete(versions, Props))}
     || A <- Allocators,
        Allocs <- [erlang:system_info({allocator, A})],
        Allocs =/= false,
        {_, N, Props} <- Allocs
    ].

allocator_blocks_metric(Alloc, Instance, Kind, count, KindInfo) ->
    Count =
        case lists:keyfind(blocks, 1, KindInfo) of
            {blocks, L} when is_list(L) ->
                sum_alloc_block_list(count, L, 0);
            Tuple ->
                element(2, Tuple)
        end,
    {[{alloc, Alloc}, {instance_no, Instance}, {kind, Kind}, {usage, blocks}], Count};
allocator_blocks_metric(Alloc, Instance, Kind, size, KindInfo) ->
    Size =
        case lists:keyfind(blocks_size, 1, KindInfo) of
            false ->
                sum_alloc_block_list(size, element(2, lists:keyfind(blocks, 1, KindInfo)), 0);
            Tuple ->
                element(2, Tuple)
        end,
    {[{alloc, Alloc}, {instance_no, Instance}, {kind, Kind}, {usage, blocks_size}], Size}.

sum_alloc_block_list(Type, [{_, L} | Rest], Acc) ->
    Value =
        case lists:keyfind(Type, 1, L) of
            false -> 0;
            Tuple -> element(2, Tuple)
        end,
    sum_alloc_block_list(Type, Rest, Value + Acc);
sum_alloc_block_list(Type, [_ | Rest], Acc) ->
    sum_alloc_block_list(Type, Rest, Acc);
sum_alloc_block_list(_Type, [], Acc) ->
    Acc.
