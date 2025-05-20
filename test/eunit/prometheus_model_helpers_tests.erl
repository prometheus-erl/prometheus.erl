-module(prometheus_model_helpers_tests).

-include_lib("eunit/include/eunit.hrl").

-export([collect_metrics/2]).

-include("prometheus.hrl").
-include("prometheus_model.hrl").

-define(METRIC_NAME_PREFIX, "rabbitmq_").

metric_name_test() ->
    %% test ?METRIC_NAME macro here too
    ?assertMatch(
        <<"rabbitmq_memory_ets_bytes">>,
        prometheus_model_helpers:metric_name("rabbitmq_memory_ets_bytes")
    ),
    ?assertMatch(
        <<"rabbitmq_memory_ets_bytes">>,
        prometheus_model_helpers:metric_name(<<"rabbitmq_memory_ets_bytes">>)
    ),
    ?assertMatch(
        <<"rabbitmq_memory_ets_bytes">>,
        prometheus_model_helpers:metric_name(rabbitmq_memory_ets_bytes)
    ),
    ?assertMatch(
        <<"rabbitmq_memory_ets_bytes">>,
        prometheus_model_helpers:metric_name(?METRIC_NAME(memory_ets_bytes))
    ),
    ?assertMatch(
        <<"rabbitmq_memory_ets_bytes">>,
        prometheus_model_helpers:metric_name(
            ?METRIC_NAME(["memory_", ets, "_bytes"])
        )
    ).

gauge_metric_test() ->
    Value = 11,
    NoLabels = [],
    LName = <<"label">>,
    LValue = <<"value">>,
    Labels = [{LName, LValue}],
    LabelsMap = #{LName => LValue},
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            gauge = #'Gauge'{value = Value}
        },
        prometheus_model_helpers:gauge_metric(Value)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            gauge = #'Gauge'{value = Value}
        },
        prometheus_model_helpers:gauge_metric({Value})
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            gauge = #'Gauge'{value = Value}
        },
        prometheus_model_helpers:gauge_metric(Labels, Value)
    ),
    ?assertMatch(
        [
            #'Metric'{
                label = [
                    #'LabelPair'{
                        name = LName,
                        value = LValue
                    }
                ],
                gauge = #'Gauge'{value = Value}
            }
        ],
        prometheus_model_helpers:gauge_metrics([{Labels, Value}])
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            gauge = #'Gauge'{value = Value}
        },
        prometheus_model_helpers:gauge_metric(LabelsMap, Value)
    ),
    ?assertMatch(
        [
            #'Metric'{
                label = [
                    #'LabelPair'{
                        name = LName,
                        value = LValue
                    }
                ],
                gauge = #'Gauge'{value = Value}
            }
        ],
        prometheus_model_helpers:gauge_metrics([{LabelsMap, Value}])
    ).

untyped_metric_test() ->
    Value = 11,
    NoLabels = [],
    LName = <<"label">>,
    LValue = <<"value">>,
    Labels = [{LName, LValue}],
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = Value}
        },
        prometheus_model_helpers:untyped_metric(Value)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = Value}
        },
        prometheus_model_helpers:untyped_metric({Value})
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            untyped = #'Untyped'{value = Value}
        },
        prometheus_model_helpers:untyped_metric(Labels, Value)
    ),
    ?assertMatch(
        [
            #'Metric'{
                label = [
                    #'LabelPair'{
                        name = LName,
                        value = LValue
                    }
                ],
                untyped = #'Untyped'{value = Value}
            }
        ],
        prometheus_model_helpers:untyped_metrics([{Labels, Value}])
    ).

boolean_metric_test() ->
    Value = 11,
    NoLabels = [],
    LName = <<"label">>,
    LValue = <<"value">>,
    Labels = [{LName, LValue}],
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 1}
        },
        prometheus_model_helpers:boolean_metric(Value)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 1}
        },
        prometheus_model_helpers:boolean_metric({Value})
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            untyped = #'Untyped'{value = 1}
        },
        prometheus_model_helpers:boolean_metric(Labels, Value)
    ),
    ?assertMatch(
        [
            #'Metric'{
                label = [
                    #'LabelPair'{
                        name = LName,
                        value = LValue
                    }
                ],
                untyped = #'Untyped'{value = 1}
            }
        ],
        prometheus_model_helpers:boolean_metrics([{Labels, Value}])
    ),

    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 1}
        },
        prometheus_model_helpers:boolean_metric(true)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 0}
        },
        prometheus_model_helpers:boolean_metric(false)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 1}
        },
        prometheus_model_helpers:boolean_metric(1)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 0}
        },
        prometheus_model_helpers:boolean_metric(0)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 0}
        },
        prometheus_model_helpers:boolean_metric([])
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 1}
        },
        prometheus_model_helpers:boolean_metric(123)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = 1}
        },
        prometheus_model_helpers:boolean_metric([1])
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            untyped = #'Untyped'{value = undefined}
        },
        prometheus_model_helpers:boolean_metric(undefined)
    ).

counter_metric_test() ->
    Value = 11,
    NoLabels = [],
    LName = <<"label">>,
    LValue = <<"value">>,
    Labels = [{LName, LValue}],
    LabelsMap = #{LName => LValue},
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            counter = #'Counter'{value = Value}
        },
        prometheus_model_helpers:counter_metric(Value)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            counter = #'Counter'{value = Value}
        },
        prometheus_model_helpers:counter_metric({Value})
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            counter = #'Counter'{value = Value}
        },
        prometheus_model_helpers:counter_metric(Labels, Value)
    ),
    ?assertMatch(
        [
            #'Metric'{
                label = [
                    #'LabelPair'{
                        name = LName,
                        value = LValue
                    }
                ],
                counter = #'Counter'{value = Value}
            }
        ],
        prometheus_model_helpers:counter_metrics([{Labels, Value}])
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            counter = #'Counter'{value = Value}
        },
        prometheus_model_helpers:counter_metric(LabelsMap, Value)
    ).

summary_metric_test() ->
    Sum = 11,
    Count = 2,
    NoLabels = [],
    LName = <<"label">>,
    LValue = <<"value">>,
    Labels = [{LName, LValue}],
    LabelsMap = #{LName => LValue},
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            summary = #'Summary'{
                sample_sum = Sum,
                sample_count = Count
            }
        },
        prometheus_model_helpers:summary_metric(Count, Sum)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            summary = #'Summary'{
                sample_sum = Sum,
                sample_count = Count
            }
        },
        prometheus_model_helpers:summary_metric({Count, Sum})
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            summary = #'Summary'{
                sample_sum = Sum,
                sample_count = Count
            }
        },
        prometheus_model_helpers:summary_metric(Labels, Count, Sum)
    ),
    ?assertMatch(
        [
            #'Metric'{
                label = [
                    #'LabelPair'{
                        name = LName,
                        value = LValue
                    }
                ],
                summary = #'Summary'{
                    sample_sum = Sum,
                    sample_count = Count
                }
            }
        ],
        prometheus_model_helpers:summary_metrics([{Labels, Count, Sum}])
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            summary = #'Summary'{
                sample_sum = Sum,
                sample_count = Count
            }
        },
        prometheus_model_helpers:summary_metric(LabelsMap, Count, Sum)
    ).

histogram_metric_test() ->
    Sum = 11,
    Count = 2,
    Buckets = [{1, 1}, {2, 3}, {3, 4}, {infinity, 10}],
    NoLabels = [],
    LName = <<"label">>,
    LValue = <<"value">>,
    Labels = [{LName, LValue}],
    LabelsMap = #{LName => LValue},
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            histogram = #'Histogram'{
                sample_sum = Sum,
                sample_count = Count,
                bucket = [
                    #'Bucket'{
                        cumulative_count = 1,
                        upper_bound = 1
                    },
                    #'Bucket'{
                        cumulative_count = 3,
                        upper_bound = 2
                    },
                    #'Bucket'{
                        cumulative_count = 4,
                        upper_bound = 3
                    },
                    #'Bucket'{
                        cumulative_count = 10,
                        upper_bound = infinity
                    }
                ]
            }
        },
        prometheus_model_helpers:histogram_metric(Buckets, Count, Sum)
    ),
    ?assertMatch(
        #'Metric'{
            label = NoLabels,
            histogram = #'Histogram'{
                sample_sum = Sum,
                sample_count = Count,
                bucket = [
                    #'Bucket'{
                        cumulative_count = 1,
                        upper_bound = 1
                    },
                    #'Bucket'{
                        cumulative_count = 3,
                        upper_bound = 2
                    },
                    #'Bucket'{
                        cumulative_count = 4,
                        upper_bound = 3
                    },
                    #'Bucket'{
                        cumulative_count = 10,
                        upper_bound = infinity
                    }
                ]
            }
        },
        prometheus_model_helpers:histogram_metric({Buckets, Count, Sum})
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            histogram = #'Histogram'{
                sample_sum = Sum,
                sample_count = Count,
                bucket = [
                    #'Bucket'{
                        cumulative_count = 1,
                        upper_bound = 1
                    },
                    #'Bucket'{
                        cumulative_count = 3,
                        upper_bound = 2
                    },
                    #'Bucket'{
                        cumulative_count = 4,
                        upper_bound = 3
                    },
                    #'Bucket'{
                        cumulative_count = 10,
                        upper_bound = infinity
                    }
                ]
            }
        },
        prometheus_model_helpers:histogram_metric(Labels, Buckets, Count, Sum)
    ),
    ?assertMatch(
        [
            #'Metric'{
                label = [
                    #'LabelPair'{
                        name = LName,
                        value = LValue
                    }
                ],
                histogram = #'Histogram'{
                    sample_sum = Sum,
                    sample_count = Count,
                    bucket = [
                        #'Bucket'{
                            cumulative_count = 1,
                            upper_bound = 1
                        },
                        #'Bucket'{
                            cumulative_count = 3,
                            upper_bound = 2
                        },
                        #'Bucket'{
                            cumulative_count = 4,
                            upper_bound = 3
                        },
                        #'Bucket'{
                            cumulative_count = 10,
                            upper_bound = infinity
                        }
                    ]
                }
            }
        ],
        prometheus_model_helpers:histogram_metrics([{Labels, Buckets, Count, Sum}])
    ),
    ?assertMatch(
        #'Metric'{
            label = [
                #'LabelPair'{
                    name = LName,
                    value = LValue
                }
            ],
            histogram = #'Histogram'{
                sample_sum = Sum,
                sample_count = Count,
                bucket = [
                    #'Bucket'{
                        cumulative_count = 1,
                        upper_bound = 1
                    },
                    #'Bucket'{
                        cumulative_count = 3,
                        upper_bound = 2
                    },
                    #'Bucket'{
                        cumulative_count = 4,
                        upper_bound = 3
                    },
                    #'Bucket'{
                        cumulative_count = 10,
                        upper_bound = infinity
                    }
                ]
            }
        },
        prometheus_model_helpers:histogram_metric(LabelsMap, Buckets, Count, Sum)
    ).

fitler_undefined_metrics_test() ->
    ?assertEqual(
        [1, 2, 3],
        prometheus_model_helpers:filter_undefined_metrics(
            [undefined, 1, undefined, 2, 3, undefined, undefined]
        )
    ).

eunsure_mf_type_test() ->
    ?assertEqual('GAUGE', prometheus_model_helpers:ensure_mf_type(gauge)),
    ?assertEqual('COUNTER', prometheus_model_helpers:ensure_mf_type(counter)),
    ?assertEqual('SUMMARY', prometheus_model_helpers:ensure_mf_type(summary)),
    ?assertEqual('HISTOGRAM', prometheus_model_helpers:ensure_mf_type(histogram)),
    ?assertEqual('UNTYPED', prometheus_model_helpers:ensure_mf_type(untyped)),
    ?assertError(
        {invalid_metric_type, qwe},
        prometheus_model_helpers:ensure_mf_type(qwe)
    ).

ensure_binary_or_string_test() ->
    ?assertEqual(<<"qwe">>, prometheus_model_helpers:ensure_binary_or_string(qwe)),
    ?assertEqual("qwe", prometheus_model_helpers:ensure_binary_or_string("qwe")),
    ?assertEqual(<<"qwe">>, prometheus_model_helpers:ensure_binary_or_string(<<"qwe">>)),
    ?assertEqual(["2"], prometheus_model_helpers:ensure_binary_or_string(2)).

create_mf_test() ->
    ?assertMatch(
        #'MetricFamily'{
            name = <<"g1">>,
            help = "help",
            type = 'GAUGE',
            metric = [
                #'Metric'{
                    label = [],
                    gauge = #'Gauge'{value = g1_value}
                }
            ]
        },
        create_mf(g1, "help", gauge)
    ),

    ?assertMatch(
        #'MetricFamily'{
            name = ["ga", <<"uge1">>],
            help = "help",
            type = 'GAUGE',
            metric = [
                #'Metric'{
                    label = [],
                    gauge = #'Gauge'{value = g1_value}
                }
            ]
        },
        create_mf(["ga", <<"uge1">>], "help", gauge, {[], g1_value})
    ),

    ?assertMatch(
        #'MetricFamily'{
            name = <<"create_mf_with_empty_map">>,
            help = "help",
            type = 'GAUGE',
            metric = [
                #'Metric'{
                    label = [],
                    gauge = #'Gauge'{value = 0.42}
                }
            ]
        },
        create_mf(<<"create_mf_with_empty_map">>, "help", gauge, #{#{} => 0.42})
    ),

    ?assertMatch(
        #'MetricFamily'{
            name = <<"create_mf_with_map">>,
            help = "help",
            type = 'GAUGE',
            metric = [
                #'Metric'{
                    label = [
                        #'LabelPair'{
                            name = <<"l1">>,
                            value = <<"v1">>
                        }
                    ],
                    gauge = #'Gauge'{value = my_value}
                }
            ]
        },
        create_mf(<<"create_mf_with_map">>, "help", gauge, #{#{<<"l1">> => <<"v1">>} => my_value})
    ).

collect_metrics(g1, _Data) ->
    prometheus_model_helpers:gauge_metric(g1_value).

create_mf(Name, Help, Type, Metrics) ->
    prometheus_model_helpers:create_mf(Name, Help, Type, Metrics).
create_mf(Name, Help, Type) ->
    prometheus_model_helpers:create_mf(Name, Help, Type, ?MODULE, undefined).
