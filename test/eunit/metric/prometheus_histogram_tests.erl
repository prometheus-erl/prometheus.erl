-module(prometheus_histogram_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_registration_as_list/1,
        fun test_registration_as_map/1,
        fun test_errors/1,
        fun test_buckets/1,
        fun test_observe/1,
        fun test_pobserve/1,
        fun test_observe_n/0,
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

test_registration_as_list(_) ->
    Name = request_duration,
    SpecWithRegistry = #{
        name => Name,
        buckets => [100, 300, 500, 750, 1000],
        help => "",
        registry => qwe
    },
    [
        ?_assertEqual(
            true,
            prometheus_histogram:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_histogram:new(SpecWithRegistry)
        )
    ].

test_registration_as_map(_) ->
    Name = request_duration,
    SpecWithRegistry = [
        {name, Name},
        {buckets, [100, 300, 500, 750, 1000]},
        {help, ""},
        {registry, qwe}
    ],
    [
        ?_assertEqual(
            true,
            prometheus_histogram:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_histogram:new(SpecWithRegistry)
        )
    ].

test_errors(_) ->
    prometheus_histogram:new([
        {name, request_duration},
        {buckets, [100, 300, 500, 750, 1000]},
        {help, "Track requests duration"}
    ]),
    prometheus_histogram:new([
        {name, db_query_duration},
        {labels, [repo]},
        {help, ""}
    ]),

    %% basic name/labels/help validations test
    [
        ?_assertError(
            {invalid_metric_name, 12, "metric name is not a string"},
            prometheus_histogram:new([{name, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_labels, 12, "not list"},
            prometheus_histogram:new([{name, "qwe"}, {labels, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_label_name, "le", "histogram cannot have a label named \"le\""},
            prometheus_histogram:new([
                {name, "qwe"},
                {labels, ["qwe", "le"]},
                {help, ""}
            ])
        ),
        ?_assertError(
            {invalid_metric_help, 12, "metric help is not a string"},
            prometheus_histogram:new([{name, "qwe"}, {help, 12}])
        ),

        %% mf/arity errors
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_histogram:observe(unknown_metric, 1)
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_histogram:observe_n(unknown_metric, 1, 1)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_histogram:observe(db_query_duration, [repo, db], 1)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_histogram:observe_n(db_query_duration, [repo, db], 1, 1)
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_histogram:observe_duration(unknown_metric, fun() -> 1 end)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_histogram:observe_duration(
                db_query_duration,
                [repo, db],
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_histogram:reset(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_histogram:reset(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_histogram:value(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_histogram:value(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_histogram:buckets(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_histogram:buckets(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_histogram:remove(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_histogram:remove(db_query_duration, [repo, db])
        ),

        %% histogram specific errors
        ?_assertError(
            {no_buckets, []},
            prometheus_histogram:new([{name, "qwe"}, {help, ""}, {buckets, []}])
        ),
        ?_assertError(
            {no_buckets, undefined},
            prometheus_histogram:new([
                {name, "qwe"},
                {help, ""},
                {buckets, undefined}
            ])
        ),
        ?_assertError(
            {invalid_buckets, 1, "not a list"},
            prometheus_histogram:new([{name, "qwe"}, {help, ""}, {buckets, 1}])
        ),
        ?_assertError(
            {invalid_bound, "qwe"},
            prometheus_histogram:new([
                {name, "qwe"},
                {help, ""},
                {buckets, ["qwe"]}
            ])
        ),
        ?_assertError(
            {invalid_buckets, [1, 3, 2], "buckets not sorted"},
            prometheus_histogram:new([
                {name, "qwe"},
                {help, ""},
                {buckets, [1, 3, 2]}
            ])
        ),
        ?_assertError(
            {invalid_value, "qwe", "observe accepts only numbers"},
            prometheus_histogram:observe(request_duration, "qwe")
        ),
        ?_assertError(
            {invalid_value, "qwe", "observe_n accepts only number values"},
            prometheus_histogram:observe_n(request_duration, "qwe", 3)
        ),
        ?_assertError(
            {invalid_count, "qwe", "observe_n accepts only integer counts"},
            prometheus_histogram:observe_n(request_duration, 300, "qwe")
        ),
        ?_assertError(
            {invalid_value, "qwe", "observe_duration accepts only functions"},
            prometheus_histogram:observe_duration(pool_size, "qwe")
        )
    ].

test_buckets(_) ->
    prometheus_histogram:new([{name, "default_buckets"}, {help, ""}]),
    DefaultBuckets = prometheus_histogram:buckets("default_buckets"),
    prometheus_histogram:new([
        {name, http_request_duration_milliseconds},
        {labels, [method]},
        {buckets, [100, 300, 500, 750, 1000]},
        {help, "Http Request execution time"},
        {duration_unit, false}
    ]),

    prometheus_histogram:new([
        {name, "explicit_default_buckets"},
        {help, ""},
        {buckets, default}
    ]),
    ExplicitDefaultBuckets = prometheus_histogram:buckets("explicit_default_buckets"),

    prometheus_histogram:new([
        {name, "linear_buckets"},
        {help, ""},
        {buckets, {linear, -15, 5, 6}}
    ]),
    LinearBuckets = prometheus_histogram:buckets("linear_buckets"),

    prometheus_histogram:declare([
        {name, "exp_buckets"},
        {help, ""},
        {buckets, {exponential, 100, 1.2, 3}}
    ]),
    ExpBuckets = prometheus_histogram:buckets("exp_buckets"),

    CustomBuckets = prometheus_histogram:buckets(
        http_request_duration_milliseconds,
        [method]
    ),
    [
        ?_assertEqual(
            prometheus_buckets:default() ++ [infinity],
            DefaultBuckets
        ),
        ?_assertEqual(
            prometheus_buckets:default() ++ [infinity],
            ExplicitDefaultBuckets
        ),
        ?_assertEqual([100, 300, 500, 750, 1000, infinity], CustomBuckets),
        ?_assertEqual([-15, -10, -5, 0, 5, 10, infinity], LinearBuckets),
        ?_assertEqual([100, 120, 144, infinity], ExpBuckets)
    ].

test_observe(_) ->
    prometheus_histogram:new([
        {name, http_request_duration_milliseconds},
        {labels, [method]},
        {buckets, [100, 300, 500, 750, 1000]},
        {help, "Http Request execution time"},
        {duration_unit, false}
    ]),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 95),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 100),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 102),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 150),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 250),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 75),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 350),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 550),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 950),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 500.2),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 150.4),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 450.5),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 850.3),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 750.9),
    prometheus_histogram:observe(http_request_duration_milliseconds, [get], 1650.23),
    Value = prometheus_histogram:value(http_request_duration_milliseconds, [get]),
    prometheus_histogram:reset(http_request_duration_milliseconds, [get]),
    RValue = prometheus_histogram:value(http_request_duration_milliseconds, [get]),
    [
        ?_assertMatch(
            {[3, 4, 2, 2, 3, 1], Sum} when
                Sum > 6974.5 andalso Sum < 6974.55,
            Value
        ),
        ?_assertEqual({[0, 0, 0, 0, 0, 0], 0}, RValue)
    ].

test_pobserve(_) ->
    Name = http_request_duration_milliseconds,
    Buckets = [100, 300, 500, 750, 1000, infinity],
    prometheus_histogram:new([
        {name, Name},
        {labels, [method]},
        {buckets, Buckets},
        {help, "Http Request execution time"},
        {duration_unit, false}
    ]),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 0, 95),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 0, 100),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 1, 102),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 1, 150),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 1, 250),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 0, 75),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 2, 350),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 3, 550),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 4, 950),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 3, 500.2),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 1, 150.4),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 2, 450.5),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 4, 850.3),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 4, 750.9),
    prometheus_histogram:pobserve(default, Name, [get], Buckets, 5, 1650.23),
    Value = prometheus_histogram:value(Name, [get]),
    prometheus_histogram:reset(Name, [get]),
    RValue = prometheus_histogram:value(Name, [get]),
    [
        ?_assertMatch(
            {[3, 4, 2, 2, 3, 1], Sum} when
                Sum > 6974.5 andalso Sum < 6974.55,
            Value
        ),
        ?_assertEqual({[0, 0, 0, 0, 0, 0], 0}, RValue)
    ].

test_observe_n() ->
    prometheus_histogram:new([{name, temp}, {help, "temp"}, {buckets, [10, 20, 30, 40, 50]}]),
    ?assertEqual({[0, 0, 0, 0, 0, 0], 0}, prometheus_histogram:value(temp)),

    prometheus_histogram:observe_n(temp, 5.5, 2),
    Sum1 = 5.5 * 2,
    ?assertEqual({[2, 0, 0, 0, 0, 0], Sum1}, prometheus_histogram:value(temp)),

    prometheus_histogram:observe(temp, 15.5),
    Sum2 = Sum1 + 15.5,
    ?assertEqual({[2, 1, 0, 0, 0, 0], Sum2}, prometheus_histogram:value(temp)),

    prometheus_histogram:observe(temp, 1),
    Sum3 = Sum2 + 1,
    ?assertEqual({[3, 1, 0, 0, 0, 0], Sum3}, prometheus_histogram:value(temp)),

    ok.

test_observe_duration_seconds(_) ->
    prometheus_histogram:new([
        {name, fun_duration_seconds},
        {buckets, [0.5, 1.1]},
        {help, ""}
    ]),
    prometheus_histogram:observe_duration(fun_duration_seconds, fun() ->
        timer:sleep(1000)
    end),
    {Buckets, Sum} = prometheus_histogram:value(fun_duration_seconds),

    try
        prometheus_histogram:observe_duration(
            fun_duration_seconds,
            fun() ->
                erlang:error({qwe})
            end
        )
    catch
        _:_ -> ok
    end,

    {BucketsE, SumE} = prometheus_histogram:value(fun_duration_seconds),

    [MF] = prometheus_collector:collect_mf_to_list(prometheus_histogram),

    MBuckets = [
        #'Bucket'{
            cumulative_count = 1,
            upper_bound = 0.5
        },
        #'Bucket'{
            cumulative_count = 2,
            upper_bound = 1.1
        },
        #'Bucket'{
            cumulative_count = 2,
            upper_bound = infinity
        }
    ],

    #'MetricFamily'{
        metric =
            [
                #'Metric'{
                    histogram =
                        #'Histogram'{
                            sample_sum = MFSum,
                            sample_count = MFCount,
                            bucket = MBuckets
                        }
                }
            ]
    } = MF,

    [
        ?_assertEqual([0, 1, 0], Buckets),
        ?_assertEqual([1, 1, 0], BucketsE),
        ?_assertEqual(true, 0.9 < Sum andalso Sum < 1.2),
        ?_assertEqual(true, 0.9 < SumE andalso SumE < 1.2),
        ?_assertEqual(2, MFCount),
        ?_assertEqual(true, 0.9 < MFSum andalso MFSum < 1.2)
    ].

test_observe_duration_milliseconds(_) ->
    prometheus_histogram:new([
        {name, fun_duration_histogram},
        {buckets, [500, 1100]},
        {help, ""},
        {duration_unit, milliseconds}
    ]),
    prometheus_histogram:observe_duration(fun_duration_histogram, fun() ->
        timer:sleep(1000)
    end),
    {Buckets, Sum} = prometheus_histogram:value(fun_duration_histogram),

    try
        prometheus_histogram:observe_duration(
            fun_duration_histogram,
            fun() ->
                erlang:error({qwe})
            end
        )
    catch
        _:_ -> ok
    end,

    {BucketsE, SumE} = prometheus_histogram:value(fun_duration_histogram),

    [
        ?_assertEqual([0, 1, 0], Buckets),
        ?_assertEqual([1, 1, 0], BucketsE),
        ?_assertMatch(true, 900 < Sum andalso Sum < 1200),
        ?_assertMatch(true, 900 < SumE andalso SumE < 1200)
    ].

test_deregister(_) ->
    prometheus_histogram:new([
        {name, histogram},
        {buckets, [5, 10]},
        {labels, [pool]},
        {help, ""}
    ]),
    prometheus_histogram:new([
        {name, simple_histogram},
        {buckets, [5, 10]},
        {help, ""}
    ]),

    prometheus_histogram:observe(histogram, [mongodb], 1),
    prometheus_histogram:observe(simple_histogram, 1),
    prometheus_histogram:observe(histogram, [mongodb], 6),
    prometheus_histogram:observe(simple_histogram, 6),

    [
        ?_assertMatch({true, true}, prometheus_histogram:deregister(histogram)),
        ?_assertMatch({false, false}, prometheus_histogram:deregister(histogram)),
        ?_assertEqual(2, length(ets:tab2list(prometheus_histogram_table))),
        ?_assertEqual({[1, 1, 0], 7}, prometheus_histogram:value(simple_histogram))
    ].

test_remove(_) ->
    prometheus_histogram:new([
        {name, histogram},
        {buckets, [5, 10]},
        {labels, [pool]},
        {help, ""}
    ]),
    prometheus_histogram:new([
        {name, simple_histogram},
        {buckets, [5, 10]},
        {help, ""}
    ]),

    prometheus_histogram:observe(histogram, [mongodb], 1),
    prometheus_histogram:observe(simple_histogram, 1),
    prometheus_histogram:observe(histogram, [mongodb], 6),
    prometheus_histogram:observe(simple_histogram, 6),

    BRValue1 = prometheus_histogram:value(histogram, [mongodb]),
    BRValue2 = prometheus_histogram:value(simple_histogram),

    RResult1 = prometheus_histogram:remove(histogram, [mongodb]),
    RResult2 = prometheus_histogram:remove(simple_histogram),

    ARValue1 = prometheus_histogram:value(histogram, [mongodb]),
    ARValue2 = prometheus_histogram:value(simple_histogram),

    RResult3 = prometheus_histogram:remove(histogram, [mongodb]),
    RResult4 = prometheus_histogram:remove(simple_histogram),

    [
        ?_assertEqual({[1, 1, 0], 7}, BRValue1),
        ?_assertEqual({[1, 1, 0], 7}, BRValue2),
        ?_assertEqual(true, RResult1),
        ?_assertEqual(true, RResult2),
        ?_assertEqual(undefined, ARValue1),
        ?_assertEqual(undefined, ARValue2),
        ?_assertEqual(false, RResult3),
        ?_assertEqual(false, RResult4)
    ].

test_default_value(_) ->
    prometheus_histogram:new([
        {name, duration_histogram},
        {labels, [label]},
        {help, ""}
    ]),
    UndefinedValue = prometheus_histogram:value(duration_histogram, [label]),

    prometheus_histogram:new([
        {name, something_histogram},
        {labels, []},
        {help, ""},
        {buckets, [5, 10]}
    ]),
    SomethingValue = prometheus_histogram:value(something_histogram),

    [
        ?_assertEqual(undefined, UndefinedValue),
        ?_assertEqual({[0, 0, 0], 0}, SomethingValue)
    ].

test_values(_) ->
    prometheus_histogram:new([
        {name, duration_histogram},
        {labels, [label]},
        {help, ""}
    ]),
    prometheus_histogram:observe(duration_histogram, [label1], 12),
    prometheus_histogram:observe(duration_histogram, [label2], 111),

    [
        ?_assertEqual(
            [
                {
                    [{label, label1}],
                    [
                        {0.005, 0},
                        {0.01, 0},
                        {0.025, 0},
                        {0.05, 0},
                        {0.1, 0},
                        {0.25, 0},
                        {0.5, 0},
                        {1, 0},
                        {2.5, 0},
                        {5, 0},
                        {10, 0},
                        {infinity, 1}
                    ],
                    12
                },
                {
                    [{label, label2}],
                    [
                        {0.005, 0},
                        {0.01, 0},
                        {0.025, 0},
                        {0.05, 0},
                        {0.1, 0},
                        {0.25, 0},
                        {0.5, 0},
                        {1, 0},
                        {2.5, 0},
                        {5, 0},
                        {10, 0},
                        {infinity, 1}
                    ],
                    111
                }
            ],
            lists:sort(prometheus_histogram:values(default, duration_histogram))
        )
    ].

test_collector1(_) ->
    prometheus_histogram:new([
        {name, simple_histogram},
        {labels, ["label"]},
        {buckets, [5, 10]},
        {help, ""}
    ]),
    prometheus_histogram:observe(simple_histogram, [label_value], 4),
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
                                histogram = #'Histogram'{
                                    sample_count = 1,
                                    sample_sum = 4,
                                    bucket = [
                                        #'Bucket'{
                                            cumulative_count = 1,
                                            upper_bound = 5
                                        },
                                        #'Bucket'{
                                            cumulative_count = 1,
                                            upper_bound = 10
                                        },
                                        #'Bucket'{
                                            cumulative_count = 1,
                                            upper_bound = infinity
                                        }
                                    ]
                                }
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_histogram)
        )
    ].

test_collector2(_) ->
    prometheus_histogram:new([
        {name, simple_histogram},
        {labels, ["label"]},
        {constant_labels, #{qwe => qwa}},
        {buckets, [5, 10]},
        {help, ""}
    ]),
    prometheus_histogram:observe(simple_histogram, [label_value], 7),
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
                                histogram = #'Histogram'{
                                    sample_count = 1,
                                    sample_sum = 7,
                                    bucket = [
                                        #'Bucket'{
                                            cumulative_count = 0,
                                            upper_bound = 5
                                        },
                                        #'Bucket'{
                                            cumulative_count = 1,
                                            upper_bound = 10
                                        },
                                        #'Bucket'{
                                            cumulative_count = 1,
                                            upper_bound = infinity
                                        }
                                    ]
                                }
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_histogram)
        )
    ].

test_collector3(_) ->
    MFList =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_histogram:new([
                {name, simple_histogram},
                {labels, ["label"]},
                {buckets, [5, 10]},
                {help, ""}
            ]),
            prometheus_histogram:observe(simple_histogram, [label_value], 7),
            prometheus_collector:collect_mf_to_list(prometheus_histogram)
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
                                histogram = #'Histogram'{
                                    sample_count = 1,
                                    sample_sum = 7,
                                    bucket = [
                                        #'Bucket'{
                                            cumulative_count = 0,
                                            upper_bound = 5
                                        },
                                        #'Bucket'{
                                            cumulative_count = 1,
                                            upper_bound = 10
                                        },
                                        #'Bucket'{
                                            cumulative_count = 1,
                                            upper_bound = infinity
                                        }
                                    ]
                                }
                            }
                        ]
                }
            ],
            MFList
        )
    ].
