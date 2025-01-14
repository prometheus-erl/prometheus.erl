-module(prometheus_vm_msacc_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_default_metrics/1,
        fun test_all_metrics/1,
        fun test_custom_metrics/1,
        fun test_global_labels/1
    ]}.

test_default_metrics(_) ->
    prometheus_registry:register_collector(prometheus_vm_msacc_collector),
    Metrics = prometheus_text_format:format(),
    [
        %% Base.
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_check_io_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_other_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_port_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_sleep_seconds_total")),
        %% Extra.
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_alloc_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_bif_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_busy_wait_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_ets_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_send_seconds_total")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_timers_seconds_total"))
    ].

test_all_metrics(_) ->
    try
        application:set_env(
            prometheus,
            vm_msacc_collector_metrics,
            [
                %% Base.
                aux_seconds_total,
                check_io_seconds_total,
                emulator_seconds_total,
                gc_seconds_total,
                other_seconds_total,
                port_seconds_total,
                sleep_seconds_total,
                %% Extra.
                alloc_seconds_total,
                bif_seconds_total,
                busy_wait_seconds_total,
                ets_seconds_total,
                gc_full_seconds_total,
                nif_seconds_total,
                send_seconds_total,
                timers_seconds_total
            ]
        ),
        prometheus_registry:register_collector(prometheus_vm_msacc_collector),
        Metrics = prometheus_text_format:format(),
        [
            %% Base.
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_check_io_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_other_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_port_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_sleep_seconds_total")),
            %% Extra.
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_alloc_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_bif_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_busy_wait_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_ets_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_send_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_timers_seconds_total"))
        ]
    after
        application:unset_env(prometheus, vm_msacc_collector_metrics)
    end.

test_custom_metrics(_) ->
    try
        application:set_env(
            prometheus,
            vm_msacc_collector_metrics,
            [
                aux_seconds_total,
                emulator_seconds_total,
                gc_seconds_total,
                gc_full_seconds_total,
                nif_seconds_total
            ]
        ),
        prometheus_registry:register_collector(prometheus_vm_msacc_collector),
        Metrics = prometheus_text_format:format(),
        [
            %% Base.
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_check_io_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_emulator_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_other_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_port_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_sleep_seconds_total")),
            %% Extra.
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_alloc_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_bif_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_busy_wait_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_ets_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_gc_full_seconds_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_nif_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_send_seconds_total")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_msacc_timers_seconds_total"))
        ]
    after
        application:unset_env(prometheus, vm_msacc_collector_metrics)
    end.

test_global_labels(_) ->
    Metrics =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_registry:register_collector(prometheus_vm_msacc_collector),
            prometheus_text_format:format()
        after
            application:unset_env(prometheus, global_labels)
        end,
    [
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_msacc_aux_seconds_total{node="))
    ].
