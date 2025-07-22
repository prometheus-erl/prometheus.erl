-module(prometheus_vm_memory_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_default_metrics/1,
        fun test_all_metrics/1,
        fun test_custom_metrics/1,
        fun test_global_labels/1
    ]}.

test_default_metrics(_) ->
    prometheus_registry:register_collector(prometheus_vm_memory_collector),
    Metrics = prometheus_text_format:format(),
    [
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_bytes")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_dets_tables")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_ets_tables")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_processes_bytes")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_system_bytes"))
    ].

test_all_metrics(_) ->
    try
        application:set_env(prometheus, vm_memory_collector_metrics, [
            atom_bytes,
            bytes,
            dets_tables,
            ets_tables,
            processes_bytes,
            system_bytes
        ]),
        prometheus_registry:register_collector(prometheus_vm_memory_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_dets_tables")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_ets_tables")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_processes_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_system_bytes"))
        ]
    after
        application:unset_env(prometheus, vm_memory_collector_metrics)
    end.

test_custom_metrics(_) ->
    try
        application:set_env(prometheus, vm_memory_collector_metrics, [
            atom_bytes,
            dets_tables
        ]),
        prometheus_registry:register_collector(prometheus_vm_memory_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_memory_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_dets_tables")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_memory_ets_tables")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_memory_processes_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_memory_system_bytes"))
        ]
    after
        application:unset_env(prometheus, vm_memory_collector_metrics)
    end.

test_global_labels(_) ->
    Metrics =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_registry:register_collector(prometheus_vm_memory_collector),
            prometheus_text_format:format()
        after
            application:unset_env(prometheus, global_labels)
        end,
    [
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes{node="))
    ].
