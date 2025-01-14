-module(prometheus_vm_statistics_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_default_metrics/1,
        fun test_all_metrics/1,
        fun test_custom_metrics/1,
        fun test_global_labels/1
    ]}.

test_default_metrics(_) ->
    prometheus_registry:register_collector(prometheus_vm_statistics_collector),
    Metrics = prometheus_text_format:format(),
    [
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_bytes_output_total")
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_bytes_received_total")
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_context_switches")
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_dirty_cpu_run_queue_length")
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_dirty_io_run_queue_length")
        ),
        ?_assertMatch(
            {match, _},
            re:run(
                Metrics,
                "erlang_vm_statistics_garbage_collection_number_of_gcs"
            )
        ),
        ?_assertMatch(
            {match, _},
            re:run(
                Metrics,
                "erlang_vm_statistics_garbage_collection_words_reclaimed"
            )
        ),
        ?_assertMatch(
            {match, _},
            re:run(
                Metrics,
                "erlang_vm_statistics_garbage_collection_bytes_reclaimed"
            )
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_reductions_total")
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_run_queues_length")
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_runtime_milliseconds")
        ),
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_wallclock_time_milliseconds")
        )
    ].

test_all_metrics(_) ->
    try
        application:set_env(
            prometheus,
            vm_statistics_collector_metrics,
            [
                bytes_output_total,
                bytes_received_total,
                context_switches,
                dirty_cpu_run_queue_length,
                dirty_io_run_queue_length,
                garbage_collection_number_of_gcs,
                garbage_collection_bytes_reclaimed,
                garbage_collection_words_reclaimed,
                reductions_total,
                run_queues_length,
                runtime_milliseconds,
                wallclock_time_milliseconds
            ]
        ),
        prometheus_registry:register_collector(prometheus_vm_statistics_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_bytes_output_total")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_bytes_received_total")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_context_switches")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_dirty_cpu_run_queue_length")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_dirty_io_run_queue_length")
            ),
            ?_assertMatch(
                {match, _},
                re:run(
                    Metrics,
                    "erlang_vm_statistics_garbage_collection_number_of_gcs"
                )
            ),
            ?_assertMatch(
                {match, _},
                re:run(
                    Metrics,
                    "erlang_vm_statistics_garbage_collection_words_reclaimed"
                )
            ),
            ?_assertMatch(
                {match, _},
                re:run(
                    Metrics,
                    "erlang_vm_statistics_garbage_collection_bytes_reclaimed"
                )
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_reductions_total")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_run_queues_length")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_runtime_milliseconds")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_wallclock_time_milliseconds")
            )
        ]
    after
        application:unset_env(prometheus, vm_statistics_collector_metrics)
    end.

test_custom_metrics(_) ->
    try
        application:set_env(prometheus, vm_statistics_collector_metrics, [
            bytes_output_total,
            bytes_received_total,
            reductions_total
        ]),
        prometheus_registry:register_collector(prometheus_vm_statistics_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_bytes_output_total")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_bytes_received_total")
            ),
            ?_assertMatch(
                nomatch,
                re:run(Metrics, "erlang_vm_statistics_context_switches")
            ),
            ?_assertMatch(
                nomatch,
                re:run(
                    Metrics,
                    "erlang_vm_statistics_garbage_collection_number_of_gcs"
                )
            ),
            ?_assertMatch(
                nomatch,
                re:run(
                    Metrics,
                    "erlang_vm_statistics_garbage_collection_words_reclaimed"
                )
            ),
            ?_assertMatch(
                nomatch,
                re:run(
                    Metrics,
                    "erlang_vm_statistics_garbage_collection_bytes_reclaimed"
                )
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_reductions_total")
            ),
            ?_assertMatch(
                nomatch,
                re:run(Metrics, "erlang_vm_statistics_run_queues_length")
            ),
            ?_assertMatch(
                nomatch,
                re:run(Metrics, "erlang_vm_statistics_runtime_milliseconds")
            ),
            ?_assertMatch(
                nomatch,
                re:run(Metrics, "erlang_vm_statistics_wallclock_time_milliseconds")
            )
        ]
    after
        application:unset_env(prometheus, vm_statistics_collector_metrics)
    end.

test_global_labels(_) ->
    Metrics =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_registry:register_collector(prometheus_vm_statistics_collector),
            prometheus_text_format:format()
        after
            application:unset_env(prometheus, global_labels)
        end,
    [
        ?_assertMatch(
            {match, _},
            re:run(Metrics, "erlang_vm_statistics_bytes_output_total{node=")
        )
    ].
