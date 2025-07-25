-module(prometheus_quantile_summary_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

-define(TABLE, prometheus_quantile_summary_table).

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_registration_as_list/1,
        fun test_registration_as_map/1,
        fun test_errors/1,
        fun test_observe/1,
        fun test_observe_quantiles/1,
        fun test_observe_configured_quantiles/1,
        fun test_observe_configured_quantiles_and_error/1,
        fun test_observe_duration_seconds/1,
        fun test_observe_duration_milliseconds/1,
        fun test_deregister/1,
        fun test_remove/1,
        fun test_default_value/1,
        fun test_values_when_empty/1,
        fun test_values_when_multiple_in_parallel/1,
        fun test_values_when_non_existing/1,
        fun test_values/1,
        fun test_collector1/1,
        fun test_collector2/1,
        fun test_collector3/1,
        fun test_merge_logic_when_fetching_value/1
    ]}.

test_merge_logic_when_fetching_value(_) ->
    Name = ?FUNCTION_NAME,
    prometheus_quantile_summary:declare(
        [{name, Name}, {labels, []}, {help, ""}, {error, 0.01}, {bound, 2184}]
    ),
    parallel_observe_sequence_of_values(Name),
    Value = prometheus_quantile_summary:value(Name),
    [
        ?_assertMatch(
            {100000, _, [
                {0.5, Q5},
                {0.90, Q90},
                {0.95, Q95}
            ]} when
                (abs(50 - Q5) =< 1) andalso
                    (abs(90 - Q90) =< 1) andalso
                    (abs(95 - Q95) =< 1),
            Value
        )
    ].

test_registration_as_list(_) ->
    Name = orders_summary,
    SpecWithRegistry = [{name, Name}, {help, ""}, {registry, qwe}],
    [
        ?_assertEqual(
            true,
            prometheus_quantile_summary:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_quantile_summary:new(SpecWithRegistry)
        )
    ].

test_registration_as_map(_) ->
    Name = orders_summary,
    SpecWithRegistry = #{name => Name, help => "", registry => qwe},
    [
        ?_assertEqual(
            true,
            prometheus_quantile_summary:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_quantile_summary:new(SpecWithRegistry)
        )
    ].

test_errors(_) ->
    prometheus_quantile_summary:new([{name, db_query_duration}, {labels, [repo]}, {help, ""}]),
    %% basic name/labels/help validations test
    [
        ?_assertError(
            {invalid_metric_name, 12, "metric name is not a string"},
            prometheus_quantile_summary:new([{name, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_labels, 12, "not list"},
            prometheus_quantile_summary:new([{name, "qwe"}, {labels, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_help, 12, "metric help is not a string"},
            prometheus_quantile_summary:new([{name, "qwe"}, {help, 12}])
        ),
        ?_assertError(
            {invalid_bound, 3.141592, "Bound should be a positive integer"},
            prometheus_quantile_summary:new([
                {name, "qwe"},
                {bound, 3.141592},
                {help, ""}
            ])
        ),
        ?_assertError(
            {invalid_error, 101, "Error should be a percentage point in (0,100)"},
            prometheus_quantile_summary:new([
                {name, "qwe"},
                {error, 101},
                {labels, ["qua", "quantile"]},
                {help, ""}
            ])
        ),
        %% mf/arity errors
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_quantile_summary:observe(unknown_metric, 1)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_quantile_summary:observe(db_query_duration, [repo, db], 1)
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_quantile_summary:observe_duration(
                unknown_metric,
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_quantile_summary:observe_duration(
                db_query_duration,
                [repo, db],
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_quantile_summary:reset(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_quantile_summary:reset(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_quantile_summary:value(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_quantile_summary:value(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_quantile_summary:remove(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_quantile_summary:remove(db_query_duration, [repo, db])
        ),
        %% summary specific errors
        ?_assertError(
            {invalid_value, "qwe", "observe accepts only numbers"},
            prometheus_quantile_summary:observe(orders_summary, "qwe")
        ),
        ?_assertError(
            {invalid_value, "qwe", "observe_duration accepts only functions"},
            prometheus_quantile_summary:observe_duration(pool_size, "qwe")
        )
    ].

test_observe(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary},
        {labels, [department]},
        {help, "Track orders count/total sum"}
    ]),
    prometheus_quantile_summary:observe(orders_summary, [electronics], 10),
    prometheus_quantile_summary:observe(orders_summary, [electronics], 15),
    prometheus_quantile_summary:observe(orders_summary, [electronics], 1.5),
    prometheus_quantile_summary:observe(orders_summary, [electronics], 2.7),

    Value = prometheus_quantile_summary:value(orders_summary, [electronics]),
    prometheus_quantile_summary:reset(orders_summary, [electronics]),
    RValue = prometheus_quantile_summary:value(orders_summary, [electronics]),
    [
        ?_assertMatch({4, Sum, QNs} when Sum > 29.1 andalso Sum < 29.3 andalso is_list(QNs), Value),
        ?_assertMatch({0, 0, []}, RValue)
    ].

test_observe_quantiles(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary_q},
        {labels, [department]},
        {quantiles, [0.0, 0.5, 0.75, 0.90, 0.95, 0.99, 0.999, 1.0]},
        {help, "Track orders quantiles"}
    ]),
    [
        prometheus_quantile_summary:observe(orders_summary_q, [electronics], N)
     || N <- lists:seq(1, 100)
    ],

    Value = prometheus_quantile_summary:value(orders_summary_q, [electronics]),
    prometheus_quantile_summary:reset(orders_summary_q, [electronics]),
    RValue = prometheus_quantile_summary:value(orders_summary_q, [electronics]),
    [
        ?_assertMatch(
            {100, 5050, [
                {+0.0, Q0},
                {0.5, Q5},
                {0.75, Q75},
                {0.90, Q90},
                {0.95, Q95},
                {0.99, Q99},
                {0.999, Q999},
                {1.0, Q1}
            ]} when
                (abs(1 - Q0) =< 1) andalso
                    (abs(50 - Q5) =< 1) andalso
                    (abs(75 - Q75) =< 1) andalso
                    (abs(90 - Q90) =< 1) andalso
                    (abs(95 - Q95) =< 1) andalso
                    (abs(99 - Q99) =< 1) andalso
                    (abs(100 - Q999) =< 1) andalso
                    (abs(100 - Q1) =< 1),
            Value
        ),
        ?_assertMatch({0, 0, []}, RValue)
    ].

test_observe_configured_quantiles(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary_q_custom},
        {labels, [department]},
        {help, "Track orders quantiles"},
        {quantiles, [0.5, 0.75]}
    ]),
    [
        prometheus_quantile_summary:observe(orders_summary_q_custom, [electronics], N)
     || N <- lists:seq(1, 100)
    ],

    Value = prometheus_quantile_summary:value(orders_summary_q_custom, [electronics]),
    prometheus_quantile_summary:reset(orders_summary_q_custom, [electronics]),
    RValue = prometheus_quantile_summary:value(orders_summary_q_custom, [electronics]),
    [
        ?_assertMatch(
            {100, 5050, [{0.5, Q5}, {0.75, Q75}]} when
                (abs(50 - Q5) =< 1) andalso
                    (abs(75 - Q75) =< 1),
            Value
        ),
        ?_assertMatch({0, 0, []}, RValue)
    ].

test_observe_configured_quantiles_and_error(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary_q_custom},
        {labels, [department]},
        {help, "Track orders quantiles"},
        {quantiles, [0.5, 0.95]},
        {error, 0.005}
    ]),
    [
        prometheus_quantile_summary:observe(orders_summary_q_custom, [electronics], N)
     || N <- lists:seq(1, 100)
    ],

    Value = prometheus_quantile_summary:value(orders_summary_q_custom, [electronics]),
    prometheus_quantile_summary:reset(orders_summary_q_custom, [electronics]),
    RValue = prometheus_quantile_summary:value(orders_summary_q_custom, [electronics]),
    [
        ?_assertMatch(
            {100, 5050, [{0.5, Q5}, {0.95, Q95}]} when
                (abs(50 - Q5) =< 0.5) andalso
                    (abs(95 - Q95) =< 0.5),
            Value
        ),
        ?_assertMatch({0, 0, []}, RValue)
    ].

test_observe_duration_seconds(_) ->
    prometheus_quantile_summary:new([
        {name, <<"fun_duration_seconds">>},
        {duration_unit, seconds},
        {help, ""}
    ]),
    prometheus_quantile_summary:observe_duration(<<"fun_duration_seconds">>, fun() ->
        timer:sleep(1000)
    end),

    {Count, Sum, _} = prometheus_quantile_summary:value(<<"fun_duration_seconds">>),

    [MF] = prometheus_collector:collect_mf_to_list(prometheus_quantile_summary),

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
        prometheus_quantile_summary:observe_duration(
            <<"fun_duration_seconds">>,
            fun() ->
                erlang:error({qwe})
            end
        )
    catch
        _:_ -> ok
    end,

    {CountE, SumE, _} = prometheus_quantile_summary:value(<<"fun_duration_seconds">>),

    [
        ?_assertEqual(1, Count),
        ?_assertEqual(1, MFCount),
        ?_assertEqual(2, CountE),
        ?_assertMatch(true, 0.9 < Sum andalso Sum < 1.2),
        ?_assertMatch(true, 0.9 < MFSum andalso MFSum < 1.2),
        ?_assertMatch(true, 0.9 < SumE andalso SumE < 1.2)
    ].

test_observe_duration_milliseconds(_) ->
    prometheus_quantile_summary:new([
        {name, fun_duration},
        {help, ""},
        {duration_unit, milliseconds}
    ]),
    prometheus_quantile_summary:observe_duration(fun_duration, fun() ->
        timer:sleep(1100)
    end),

    {Count, Sum, _} = prometheus_quantile_summary:value(fun_duration),

    try
        prometheus_quantile_summary:observe_duration(fun_duration, fun() ->
            erlang:error({qwe})
        end)
    catch
        _:_ -> ok
    end,

    {CountE, SumE, _} = prometheus_quantile_summary:value(fun_duration),

    [
        ?_assertEqual(1, Count),
        ?_assertEqual(2, CountE),
        ?_assertMatch(true, 900 < Sum andalso Sum < 1200),
        ?_assertMatch(true, 900 < SumE andalso SumE < 1200)
    ].

test_deregister(_) ->
    prometheus_quantile_summary:new([{name, summary}, {labels, [pool]}, {help, ""}]),
    prometheus_quantile_summary:new([{name, simple_summary}, {help, ""}]),

    prometheus_quantile_summary:observe(summary, [mongodb], 1),
    prometheus_quantile_summary:observe(simple_summary, 1),

    [
        ?_assertMatch({true, true}, prometheus_quantile_summary:deregister(summary)),
        ?_assertMatch({false, false}, prometheus_quantile_summary:deregister(summary)),
        ?_assertEqual(2, length(ets:tab2list(?TABLE))),
        ?_assertMatch({1, 1, _}, prometheus_quantile_summary:value(simple_summary))
    ].

test_remove(_) ->
    prometheus_quantile_summary:new([{name, summary}, {labels, [pool]}, {help, ""}]),
    prometheus_quantile_summary:new([{name, simple_summary}, {help, ""}]),

    prometheus_quantile_summary:observe(summary, [mongodb], 1),
    prometheus_quantile_summary:observe(simple_summary, 1),

    BRValue1 = prometheus_quantile_summary:value(summary, [mongodb]),
    BRValue2 = prometheus_quantile_summary:value(simple_summary),

    RResult1 = prometheus_quantile_summary:remove(summary, [mongodb]),
    RResult2 = prometheus_quantile_summary:remove(simple_summary),

    ARValue1 = prometheus_quantile_summary:value(summary, [mongodb]),
    ARValue2 = prometheus_quantile_summary:value(simple_summary),

    RResult3 = prometheus_quantile_summary:remove(summary, [mongodb]),
    RResult4 = prometheus_quantile_summary:remove(simple_summary),

    [
        ?_assertMatch({1, 1, _}, BRValue1),
        ?_assertMatch({1, 1, _}, BRValue2),
        ?_assertEqual(true, RResult1),
        ?_assertEqual(true, RResult2),
        ?_assertEqual(undefined, ARValue1),
        ?_assertEqual(undefined, ARValue2),
        ?_assertEqual(false, RResult3),
        ?_assertEqual(false, RResult4)
    ].

test_default_value(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary},
        {labels, [department]},
        {help, "Track orders count/total sum"}
    ]),
    UndefinedValue = prometheus_quantile_summary:value(orders_summary, [electronics]),

    [MF] = prometheus_collector:collect_mf_to_list(prometheus_quantile_summary),

    #'MetricFamily'{metric = EmptyMetric} = MF,

    prometheus_quantile_summary:new([
        {name, something_summary},
        {labels, []},
        {help, ""}
    ]),
    SomethingValue = prometheus_quantile_summary:value(something_summary),
    [
        ?_assertEqual(undefined, UndefinedValue),
        ?_assertMatch([], EmptyMetric),
        ?_assertMatch({0, 0, []}, SomethingValue)
    ].

test_values_when_empty(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary},
        {labels, [department]},
        {help, "Track orders count/total sum"}
    ]),
    [
        ?_assertMatch(
            [],
            lists:sort(prometheus_quantile_summary:values(default, orders_summary))
        )
    ].

test_values_when_multiple_in_parallel(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary},
        {labels, []},
        {help, "Track orders count/total sum"}
    ]),
    parallel_observe_sequence_of_values(orders_summary),
    [
        ?_assertMatch(
            [
                {[], 100000, 5050000, [
                    {0.5, 49.90296094906653},
                    {0.9, 89.13032933635913},
                    {0.95, 94.64203039019942}
                ]}
            ],
            lists:sort(prometheus_quantile_summary:values(default, orders_summary))
        )
    ].

test_values_when_non_existing(_) ->
    [
        ?_assertMatch(
            [],
            lists:sort(prometheus_quantile_summary:values(default, orders_summary))
        )
    ].

test_values(_) ->
    prometheus_quantile_summary:new([
        {name, orders_summary},
        {labels, [department]},
        {help, "Track orders count/total sum"}
    ]),
    prometheus_quantile_summary:observe(orders_summary, [electronics], 765.5),
    prometheus_quantile_summary:observe(orders_summary, [groceries], 112.3),
    [
        ?_assertMatch(
            [
                {[{department, electronics}], 1, 765.5, _},
                {[{department, groceries}], 1, 112.3, _}
            ],
            lists:sort(prometheus_quantile_summary:values(default, orders_summary))
        )
    ].

test_collector1(_) ->
    prometheus_quantile_summary:new([
        {name, simple_summary},
        {labels, ["label"]},
        {help, ""}
    ]),
    prometheus_quantile_summary:observe(simple_summary, [label_value], 4),
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
            prometheus_collector:collect_mf_to_list(prometheus_quantile_summary)
        )
    ].

test_collector2(_) ->
    prometheus_quantile_summary:new([
        {name, simple_summary},
        {labels, ["label"]},
        {constant_labels, #{qwe => qwa}},
        {help, ""}
    ]),
    prometheus_quantile_summary:observe(simple_summary, [label_value], 5),
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
            prometheus_collector:collect_mf_to_list(prometheus_quantile_summary)
        )
    ].

test_collector3(_) ->
    MFList =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_quantile_summary:new([
                {name, simple_summary},
                {labels, ["label"]},
                {help, ""}
            ]),
            prometheus_quantile_summary:observe(simple_summary, [label_value], 5),
            prometheus_collector:collect_mf_to_list(prometheus_quantile_summary)
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

parallel_observe_sequence_of_values(Name) ->
    % Observe many values
    Fun = fun() ->
        [
            prometheus_quantile_summary:observe(Name, N)
         || N <- lists:seq(1, 100)
        ]
    end,
    Monitors = [spawn_monitor(Fun) || _ <- lists:seq(1, 1000)],
    collect_monitors(Monitors).

collect_monitors([]) ->
    ok;
collect_monitors([{Pid, Ref} | Monitors]) ->
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            ?assertEqual(normal, Reason),
            collect_monitors(Monitors)
    after 5000 ->
        ct:fail("Tasks never finished in a reasonable amount of time")
    end.
