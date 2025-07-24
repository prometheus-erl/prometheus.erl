-module(prometheus_collectors_compat_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
    {setup,
        fun() ->
            mnesia:start(),
            prometheus_eunit_common:start()
        end,
        fun(X) ->
            mnesia:stop(),
            prometheus_eunit_common:stop(X)
        end,
        fun test_all_metrics/1}.

test_all_metrics(_) ->
    try
        application:set_env(prometheus, collectors_compat, true),
        Collectors = [
            prometheus_mnesia_collector,
            prometheus_vm_memory_collector,
            prometheus_vm_msacc_collector,
            prometheus_vm_statistics_collector,
            prometheus_vm_system_info_collector
        ],
        [prometheus_registry:register_collector(Collector) || Collector <- Collectors],
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_mnesia_failed_transactions")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_mnesia_committed_transactions")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_mnesia_logged_transactions")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_mnesia_restarted_transactions")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_atom_bytes_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_bytes_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_processes_bytes_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_memory_system_bytes_total")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_statistics_context_switches")),
            ?_assertMatch(
                {match, _}, re:run(Metrics, "erlang_vm_statistics_garbage_collection_number_of_gcs")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_garbage_collection_words_reclaimed")
            ),
            ?_assertMatch(
                {match, _},
                re:run(Metrics, "erlang_vm_statistics_garbage_collection_bytes_reclaimed")
            ),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_statistics_runtime_milliseconds")),
            ?_assertMatch(
                {match, _}, re:run(Metrics, "erlang_vm_statistics_wallclock_time_milliseconds")
            ),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_port_count")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_process_count")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_atom_count"))
        ]
    after
        application:unset_env(prometheus, collectors_compat)
    end.
