-module(prometheus_summary_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_registration/1,
        fun test_errors/1,
        fun test_observe/1,
        fun test_observe_duration_seconds/1,
        fun test_observe_duration_milliseconds/1,
        fun test_deregister/1,
        fun test_remove/1,
        fun test_default_value/1,
        fun test_values/1,
        fun test_collector1/1,
        fun test_collector2/1,
        fun test_collector3/1
    ]}.

test_registration(_) ->
    Name = orders_summary,
    SpecWithRegistry = [
        {name, Name},
        {help, ""},
        {registry, qwe}
    ],
    [
        ?_assertEqual(
            true,
            prometheus_summary:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_summary:new(SpecWithRegistry)
        )
    ].

test_errors(_) ->
    prometheus_summary:new([{name, db_query_duration}, {labels, [repo]}, {help, ""}]),
    %% basic name/labels/help validations test
    [
        ?_assertError(
            {invalid_metric_name, 12, "metric name is not a string"},
            prometheus_summary:new([{name, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_labels, 12, "not list"},
            prometheus_summary:new([{name, "qwe"}, {labels, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_label_name, "quantile",
                "summary cannot have a label named \"quantile\""},
            prometheus_summary:new([
                {name, "qwe"},
                {labels, ["qua", "quantile"]},
                {help, ""}
            ])
        ),
        ?_assertError(
            {invalid_metric_help, 12, "metric help is not a string"},
            prometheus_summary:new([{name, "qwe"}, {help, 12}])
        ),
        %% mf/arity errors
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_summary:observe(unknown_metric, 1)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_summary:observe(db_query_duration, [repo, db], 1)
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_summary:observe_duration(
                unknown_metric,
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_summary:observe_duration(
                db_query_duration,
                [repo, db],
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_summary:reset(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_summary:reset(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_summary:value(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_summary:value(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_summary:remove(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_summary:remove(db_query_duration, [repo, db])
        ),
        %% summary specific errors
        ?_assertError(
            {invalid_value, "qwe", "observe accepts only numbers"},
            prometheus_summary:observe(orders_summary, "qwe")
        ),
        ?_assertError(
            {invalid_value, "qwe", "observe_duration accepts only functions"},
            prometheus_summary:observe_duration(pool_size, "qwe")
        )
    ].

test_observe(_) ->
    prometheus_summary:new([
        {name, orders_summary},
        {labels, [department]},
        {help, "Track orders count/total sum"}
    ]),
    prometheus_summary:observe(orders_summary, [electronics], 10),
    prometheus_summary:observe(orders_summary, [electronics], 15),
    prometheus_summary:observe(orders_summary, [electronics], 1.5),
    prometheus_summary:observe(orders_summary, [electronics], 2.7),

    Value = prometheus_summary:value(orders_summary, [electronics]),
    prometheus_summary:reset(orders_summary, [electronics]),
    RValue = prometheus_summary:value(orders_summary, [electronics]),
    [
        ?_assertMatch({4, Sum} when Sum > 29.1 andalso Sum < 29.3, Value),
        ?_assertEqual({0, 0}, RValue)
    ].

test_observe_duration_seconds(_) ->
    prometheus_summary:new([
        {name, <<"fun_duration_seconds">>},
        {help, ""},
        {duration_unit, seconds}
    ]),
    prometheus_summary:observe_duration(<<"fun_duration_seconds">>, fun() ->
        timer:sleep(1000)
    end),

    {Count, Sum} = prometheus_summary:value(<<"fun_duration_seconds">>),

    [MF] = prometheus_collector:collect_mf_to_list(prometheus_summary),

    #'MetricFamily'{
        metric =
            [
                #'Metric'{
                    summary =
                        #'Summary'{
                            sample_sum = MFSum,
                            sample_count = MFCount
                        }
                }
            ]
    } = MF,

    try
        prometheus_summary:observe_duration(
            <<"fun_duration_seconds">>,
            fun() ->
                erlang:error({qwe})
            end
        )
    catch
        _:_ -> ok
    end,

    {CountE, SumE} = prometheus_summary:value(<<"fun_duration_seconds">>),

    [
        ?_assertEqual(1, Count),
        ?_assertEqual(1, MFCount),
        ?_assertEqual(2, CountE),
        ?_assertMatch(true, 0.9 < Sum andalso Sum < 1.2),
        ?_assertMatch(true, 0.9 < MFSum andalso MFSum < 1.2),
        ?_assertMatch(true, 0.9 < SumE andalso SumE < 1.2)
    ].

test_observe_duration_milliseconds(_) ->
    prometheus_summary:new([
        {name, fun_duration},
        {help, ""},
        {duration_unit, milliseconds}
    ]),
    prometheus_summary:observe_duration(fun_duration, fun() ->
        timer:sleep(1100)
    end),

    {Count, Sum} = prometheus_summary:value(fun_duration),

    try
        prometheus_summary:observe_duration(fun_duration, fun() ->
            erlang:error({qwe})
        end)
    catch
        _:_ -> ok
    end,

    {CountE, SumE} = prometheus_summary:value(fun_duration),

    [
        ?_assertEqual(1, Count),
        ?_assertEqual(2, CountE),
        ?_assertMatch(true, 900 < Sum andalso Sum < 1200),
        ?_assertMatch(true, 900 < SumE andalso SumE < 1200)
    ].

test_deregister(_) ->
    prometheus_summary:new([{name, summary}, {labels, [pool]}, {help, ""}]),
    prometheus_summary:new([{name, simple_summary}, {help, ""}]),

    prometheus_summary:observe(summary, [mongodb], 1),
    prometheus_summary:observe(simple_summary, 1),

    [
        ?_assertMatch({true, true}, prometheus_summary:deregister(summary)),
        ?_assertMatch({false, false}, prometheus_summary:deregister(summary)),
        ?_assertEqual(2, length(ets:tab2list(prometheus_summary_table))),
        ?_assertEqual({1, 1}, prometheus_summary:value(simple_summary))
    ].

test_remove(_) ->
    prometheus_summary:new([{name, summary}, {labels, [pool]}, {help, ""}]),
    prometheus_summary:new([{name, simple_summary}, {help, ""}]),

    prometheus_summary:observe(summary, [mongodb], 1),
    prometheus_summary:observe(simple_summary, 1),

    BRValue1 = prometheus_summary:value(summary, [mongodb]),
    BRValue2 = prometheus_summary:value(simple_summary),

    RResult1 = prometheus_summary:remove(summary, [mongodb]),
    RResult2 = prometheus_summary:remove(simple_summary),

    ARValue1 = prometheus_summary:value(summary, [mongodb]),
    ARValue2 = prometheus_summary:value(simple_summary),

    RResult3 = prometheus_summary:remove(summary, [mongodb]),
    RResult4 = prometheus_summary:remove(simple_summary),

    [
        ?_assertEqual({1, 1}, BRValue1),
        ?_assertEqual({1, 1}, BRValue2),
        ?_assertEqual(true, RResult1),
        ?_assertEqual(true, RResult2),
        ?_assertEqual(undefined, ARValue1),
        ?_assertEqual(undefined, ARValue2),
        ?_assertEqual(false, RResult3),
        ?_assertEqual(false, RResult4)
    ].

test_default_value(_) ->
    prometheus_summary:new([
        {name, orders_summary},
        {labels, [department]},
        {help, "Track orders count/total sum"}
    ]),
    UndefinedValue = prometheus_summary:value(orders_summary, [electronics]),

    prometheus_summary:new([
        {name, something_summary},
        {labels, []},
        {help, ""}
    ]),
    SomethingValue = prometheus_summary:value(something_summary),
    [
        ?_assertEqual(undefined, UndefinedValue),
        ?_assertEqual({0, 0}, SomethingValue)
    ].

test_values(_) ->
    prometheus_summary:new([
        {name, orders_summary},
        {labels, [department]},
        {help, "Track orders count/total sum"}
    ]),
    prometheus_summary:observe(orders_summary, [electronics], 765.5),
    prometheus_summary:observe(orders_summary, [groceries], 112.3),

    [
        ?_assertEqual(
            [
                {[{"department", electronics}], 1, 765.5},
                {[{"department", groceries}], 1, 112.3}
            ],
            lists:sort(prometheus_summary:values(default, orders_summary))
        )
    ].

test_collector1(_) ->
    prometheus_summary:new([
        {name, simple_summary},
        {labels, ["label"]},
        {help, ""}
    ]),
    prometheus_summary:observe(simple_summary, [label_value], 4),
    [
        ?_assertMatch(
            [
                #'MetricFamily'{
                    metric =
                        [
                            #'Metric'{
                                label = [
                                    #'LabelPair'{
                                        name = "label",
                                        value = <<"label_value">>
                                    }
                                ],
                                summary = #'Summary'{
                                    sample_count = 1,
                                    sample_sum = 4
                                }
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_summary)
        )
    ].

test_collector2(_) ->
    prometheus_summary:new([
        {name, simple_summary},
        {labels, ["label"]},
        {constant_labels, #{qwe => qwa}},
        {help, ""}
    ]),
    prometheus_summary:observe(simple_summary, [label_value], 5),
    [
        ?_assertMatch(
            [
                #'MetricFamily'{
                    metric =
                        [
                            #'Metric'{
                                label = [
                                    #'LabelPair'{
                                        name = <<"qwe">>,
                                        value = <<"qwa">>
                                    },
                                    #'LabelPair'{
                                        name = "label",
                                        value = <<"label_value">>
                                    }
                                ],
                                summary = #'Summary'{
                                    sample_count = 1,
                                    sample_sum = 5
                                }
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_summary)
        )
    ].

test_collector3(_) ->
    MFList =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_summary:new([
                {name, simple_summary},
                {labels, ["label"]},
                {help, ""}
            ]),
            prometheus_summary:observe(simple_summary, [label_value], 5),
            prometheus_collector:collect_mf_to_list(prometheus_summary)
        after
            application:unset_env(prometheus, global_labels)
        end,
    NodeBin = atom_to_binary(node(), utf8),
    [
        ?_assertMatch(
            [
                #'MetricFamily'{
                    metric =
                        [
                            #'Metric'{
                                label = [
                                    #'LabelPair'{
                                        name = <<"node">>,
                                        value = NodeBin
                                    },
                                    #'LabelPair'{
                                        name = "label",
                                        value = <<"label_value">>
                                    }
                                ],
                                summary = #'Summary'{
                                    sample_count = 1,
                                    sample_sum = 5
                                }
                            }
                        ]
                }
            ],
            MFList
        )
    ].
