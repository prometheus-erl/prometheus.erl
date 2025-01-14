-module(prometheus_vm_dist_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_dist_off_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_no_distribution/1
    ]}.

test_no_distribution(_) ->
    prometheus_registry:register_collector(prometheus_vm_dist_collector),
    Metrics = prometheus_text_format:format(),
    [
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_cnt")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_max_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_avg_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_dvi_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_cnt")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_max_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_avg_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_pend_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_port_input_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_port_output_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_port_memory_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_port_queue_size_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_memory_bytes")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_heap_size_words")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_min_heap_size_words")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_min_bin_vheap_size_words")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_stack_size_words")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_total_heap_size_words")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_message_queue_len")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_reductions")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_status")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_node_state")),
        ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_node_queue_size_bytes"))
    ].

-if(?OTP_RELEASE >= 25).
start_test_peer() ->
    {ok, Peer, _Node} = peer:start(#{name => prometheus_dist_collector_peer, wait_boot => 5000}),
    Peer.

stop_test_peer(Peer) ->
    peer:stop(Peer).

-else.
start_test_peer() ->
    ct_slave:start(prometheus_dist_collector_peer),
    undefined.

stop_test_peer(_) ->
    ct_slave:stop(prometheus_dist_collector_peer).

-endif.

prometheus_dist_on_test_() ->
    {setup,
        fun() ->
            os:cmd("erl -sname test"),
            {ok, _} = net_kernel:start([prometheus_dist_collector, shortnames]),
            start_test_peer()
        end,
        fun(Peer) ->
            stop_test_peer(Peer),
            ok = net_kernel:stop()
        end,
        {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
            fun test_default_metrics/1,
            fun test_all_metrics/1,
            fun test_custom_metrics/1,
            fun test_global_labels/1
        ]}}.

test_default_metrics(_) ->
    prometheus_registry:register_collector(prometheus_vm_dist_collector),
    Metrics = prometheus_text_format:format(),
    [
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_cnt{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_max_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_avg_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_dvi_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_cnt{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_max_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_avg_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_pend_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_input_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_output_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_memory_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_queue_size_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_memory_bytes{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_heap_size_words{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_min_heap_size_words{peer")),
        ?_assertMatch(
            {match, _}, re:run(Metrics, "erlang_vm_dist_proc_min_bin_vheap_size_words{peer")
        ),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_stack_size_words{peer")),
        ?_assertMatch(
            {match, _}, re:run(Metrics, "erlang_vm_dist_proc_total_heap_size_words{peer")
        ),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_message_queue_len{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_reductions{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_status{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_node_state{peer")),
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_node_queue_size_bytes{peer"))
    ].

test_all_metrics(_) ->
    try
        application:set_env(
            prometheus,
            vm_dist_collector_metrics,
            [
                recv_bytes,
                recv_cnt,
                recv_max_bytes,
                recv_avg_bytes,
                recv_dvi_bytes,
                send_bytes,
                send_cnt,
                send_max_bytes,
                send_avg_bytes,
                send_pend_bytes,
                port_input_bytes,
                port_output_bytes,
                port_memory_bytes,
                port_queue_size_bytes,
                proc_memory_bytes,
                proc_heap_size_words,
                proc_min_heap_size_words,
                proc_min_bin_vheap_size_words,
                proc_stack_size_words,
                proc_total_heap_size_words,
                proc_message_queue_len,
                proc_reductions,
                proc_status,
                node_state,
                node_queue_size_bytes
            ]
        ),
        prometheus_registry:register_collector(prometheus_vm_dist_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_cnt")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_max_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_avg_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_dvi_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_cnt")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_max_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_avg_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_pend_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_input_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_output_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_memory_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_queue_size_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_memory_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_heap_size_words")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_min_heap_size_words")),
            ?_assertMatch(
                {match, _}, re:run(Metrics, "erlang_vm_dist_proc_min_bin_vheap_size_words")
            ),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_stack_size_words")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_total_heap_size_words")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_message_queue_len")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_reductions")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_status")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_node_state")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_node_queue_size_bytes"))
        ]
    after
        application:unset_env(prometheus, vm_dist_collector_metrics)
    end.

test_custom_metrics(_) ->
    try
        application:set_env(
            prometheus,
            vm_dist_collector_metrics,
            [
                recv_cnt,
                recv_dvi_bytes,
                send_max_bytes,
                send_pend_bytes,
                port_memory_bytes,
                proc_memory_bytes,
                proc_reductions,
                proc_status,
                node_state
            ]
        ),
        prometheus_registry:register_collector(prometheus_vm_dist_collector),
        Metrics = prometheus_text_format:format(),
        [
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_cnt")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_max_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_recv_avg_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_recv_dvi_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_cnt")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_max_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_send_avg_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_send_pend_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_port_input_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_port_output_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_port_memory_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_port_queue_size_bytes")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_memory_bytes")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_heap_size_words")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_min_heap_size_words")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_min_bin_vheap_size_words")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_stack_size_words")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_total_heap_size_words")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_proc_message_queue_len")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_reductions")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_proc_status")),
            ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_node_state")),
            ?_assertMatch(nomatch, re:run(Metrics, "erlang_vm_dist_node_queue_size_bytes"))
        ]
    after
        application:unset_env(prometheus, vm_dist_collector_metrics)
    end.

test_global_labels(_) ->
    Metrics =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_registry:register_collector(prometheus_vm_dist_collector),
            prometheus_text_format:format()
        after
            application:unset_env(prometheus, global_labels)
        end,
    [
        ?_assertMatch({match, _}, re:run(Metrics, "erlang_vm_dist_node_state{node="))
    ].
