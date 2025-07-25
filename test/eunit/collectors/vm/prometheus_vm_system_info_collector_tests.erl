-module(prometheus_vm_system_info_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_default_metrics/1,
        fun test_all_metrics/1,
        fun test_custom_metrics/1,
        fun test_global_labels/1
    ]}.

test_default_metrics(_) ->
    prometheus_registry:register_collector(prometheus_vm_system_info_collector),
    Metrics = prometheus_text_format:format(),
    [
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_dirty_cpu_schedulers")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_dirty_cpu_schedulers_online")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_dirty_io_schedulers")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_ets_limit")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_logical_processors")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_logical_processors_available")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_logical_processors_online")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_ports")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_port_limit")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_processes")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_process_limit")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_schedulers")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_schedulers_online")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_smp_support")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_threads")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_thread_pool_size")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_time_correction")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_wordsize_bytes")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_atoms")),
        ?_assertMatch({match, _}, re:run(Metrics, "\nerlang_vm_atom_limit"))
    ].

test_all_metrics(_) ->
    try
        application:set_env(
            prometheus,
            vm_system_info_collector_metrics,
            [
                ets_limit,
                logical_processors,
                logical_processors_available,
                logical_processors_online,
                ports,
                port_limit,
                processes,
                process_limit,
                schedulers,
                schedulers_online,
                smp_support,
                threads,
                thread_pool_size,
                time_correction,
                wordsize_bytes,
                atoms,
                atom_limit
            ]
        ),
        prometheus_registry:register_collector(prometheus_vm_system_info_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ets_limit")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_logical_processors")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_logical_processors_available")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_logical_processors_online")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ports")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_port_limit")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_processes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_process_limit")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_schedulers")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_schedulers_online")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_smp_support")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_threads")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_thread_pool_size")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_time_correction")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_wordsize_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_atoms")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_atom_limit"))
        ]
    after
        application:unset_env(prometheus, vm_system_info_collector_metrics)
    end.

test_custom_metrics(_) ->
    try
        application:set_env(
            prometheus,
            vm_system_info_collector_metrics,
            [
                ets_limit,
                logical_processors,
                ports,
                processes,
                process_limit,
                schedulers
            ]
        ),
        prometheus_registry:register_collector(prometheus_vm_system_info_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ets_limit")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_logical_processors")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_logical_processors_available")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_logical_processors_online")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_ports")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_port_limit")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_processes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_process_limit")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_schedulers")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_schedulers_online")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_smp_support")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_threads")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_thread_pool_size")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_time_correction")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_wordsize_bytes"))
        ]
    after
        application:unset_env(prometheus, vm_system_info_collector_metrics)
    end.

test_global_labels(_) ->
    Metrics =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_registry:register_collector(prometheus_vm_system_info_collector),
            prometheus_text_format:format()
        after
            application:unset_env(prometheus, global_labels)
        end,
    [
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dirty_cpu_schedulers{node="))
    ].
