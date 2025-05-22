-module(prometheus_gauge_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_registration/1,
        fun test_errors/1,
        fun test_set/1,
        fun test_set_undefined/1,
        fun test_inc/1,
        fun test_dec/1,
        fun test_set_to_current_time/1,
        fun test_track_inprogress/1,
        fun test_set_duration_seconds/1,
        fun test_set_duration_milliseconds/1,
        fun test_get_value_from_undefined_with_float/1,
        fun test_deregister/1,
        fun test_remove/1,
        fun test_default_value/1,
        fun test_values/1,
        fun test_collector1/1,
        fun test_collector2/1,
        fun test_collector3/1
    ]}.

test_registration(_) ->
    Name = pool_size,
    SpecWithRegistry = [
        {name, Name},
        {help, ""},
        {registry, qwe}
    ],
    [
        ?_assertEqual(
            true,
            prometheus_gauge:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_gauge:new(SpecWithRegistry)
        )
    ].

test_errors(_) ->
    prometheus_gauge:new([{name, with_label}, {labels, [label]}, {help, ""}]),
    prometheus_gauge:new([{name, no_labels}, {help, ""}]),

    %% basic name/labels/help validations test
    [
        ?_assertError(
            {invalid_metric_name, 12, "metric name is not a string"},
            prometheus_gauge:new([{name, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_labels, 12, "not list"},
            prometheus_gauge:new([{name, "qwe"}, {labels, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_help, 12, "metric help is not a string"},
            prometheus_gauge:new([{name, "qwe"}, {help, 12}])
        ),

        %% gauge specific errors,
        ?_assertError(
            {invalid_value, "qwe", "set accepts only numbers and 'undefined'"},
            prometheus_gauge:set(pool_size, "qwe")
        ),
        ?_assertError(
            {invalid_value, "qwe", "inc accepts only numbers"},
            prometheus_gauge:inc(pool_size, [], "qwe")
        ),
        ?_assertError(
            {invalid_value, <<"qwe">>, "dec accepts only numbers"},
            prometheus_gauge:dec(no_labels, <<"qwe">>)
        ),
        ?_assertError(
            {invalid_value, "qwe", "dec accepts only numbers"},
            prometheus_gauge:dec(pool_size, [], "qwe")
        ),
        ?_assertError(
            {invalid_value, "qwe", "dec accepts only numbers"},
            prometheus_gauge:dec(default, pool_size, [], "qwe")
        ),

        ?_assertError(
            {invalid_value, "qwe", "track_inprogress accepts only functions"},
            prometheus_gauge:track_inprogress(pool_size, "qwe")
        ),
        ?_assertError(
            {invalid_value, "qwe", "set_duration accepts only functions"},
            prometheus_gauge:set_duration(pool_size, "qwe")
        ),

        %% mf/arity errors
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_gauge:set(unknown_metric, 2)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_gauge:set(with_label, [repo, db], 2)
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_gauge:set_to_current_time(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_gauge:set_to_current_time(with_label, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_gauge:track_inprogress(
                unknown_metric,
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_gauge:track_inprogress(
                with_label,
                [repo, db],
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_gauge:set_duration(
                unknown_metric,
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_gauge:set_duration(
                with_label,
                [repo, db],
                fun() -> 1 end
            )
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_gauge:reset(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_gauge:reset(with_label, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_gauge:value(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_gauge:value(with_label, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_gauge:remove(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_gauge:remove(with_label, [repo, db])
        )
    ].

test_set(_) ->
    prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
    prometheus_gauge:set(pool_size, [redis], undefined),
    ValueUndefined = prometheus_gauge:value(pool_size, [redis]),
    prometheus_gauge:set(pool_size, [float_pool], 92.3),
    ValueFloat = prometheus_gauge:value(pool_size, [float_pool]),
    prometheus_gauge:set(pool_size, [mongodb], 100),
    Value1 = prometheus_gauge:value(pool_size, [mongodb]),
    prometheus_gauge:set(pool_size, [mongodb], 105),
    Value2 = prometheus_gauge:value(pool_size, [mongodb]),
    prometheus_gauge:reset(pool_size, [mongodb]),
    RValue = prometheus_gauge:value(pool_size, [mongodb]),
    [
        ?_assertEqual(undefined, ValueUndefined),
        ?_assertEqual(92.3, ValueFloat),
        ?_assertEqual(100, Value1),
        ?_assertEqual(105, Value2),
        ?_assertEqual(0, RValue)
    ].

test_set_undefined(_) ->
    prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
    prometheus_gauge:set(pool_size, [redis], undefined),
    ValueUndefined = prometheus_gauge:value(pool_size, [redis]),
    [
        ?_assertError(
            {invalid_operation, 'inc/dec', "Can't inc/dec undefined"},
            prometheus_gauge:inc(pool_size, [redis], 1)
        ),
        ?_assertError(
            {invalid_operation, 'inc/dec', "Can't inc/dec undefined"},
            prometheus_gauge:inc(pool_size, [redis], 0.8)
        ),
        ?_assertEqual(undefined, ValueUndefined)
    ].

test_inc(_) ->
    prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
    prometheus_gauge:new([{name, temperature}, {help, ""}]),
    prometheus_gauge:inc(pool_size, [mongodb]),
    prometheus_gauge:inc(pool_size, [mongodb], 3),
    prometheus_gauge:inc(pool_size, [mongodb], 3.5),
    prometheus_gauge:inc(temperature),
    prometheus_gauge:inc(temperature, 3),
    prometheus_gauge:inc(temperature, 3.5),

    PSValue = prometheus_gauge:value(pool_size, [mongodb]),
    TValue = prometheus_gauge:value(temperature),

    prometheus_gauge:reset(pool_size, [mongodb]),
    RValue = prometheus_gauge:value(pool_size, [mongodb]),
    [
        ?_assertEqual(7.5, PSValue),
        ?_assertEqual(7.5, TValue),
        ?_assertEqual(0, RValue)
    ].

test_dec(_) ->
    prometheus_gauge:new([{name, pool_size}, {labels, [client]}, {help, ""}]),
    prometheus_gauge:new([{name, temperature}, {help, ""}]),
    prometheus_gauge:inc(pool_size, [mongodb]),
    prometheus_gauge:inc(pool_size, [mongodb], 10),
    prometheus_gauge:dec(pool_size, [mongodb]),
    prometheus_gauge:dec(pool_size, [mongodb], 6),
    prometheus_gauge:dec(pool_size, [mongodb], 0.5),
    prometheus_gauge:inc(temperature),
    prometheus_gauge:inc(temperature, 10),
    prometheus_gauge:dec(temperature),
    prometheus_gauge:dec(temperature, 6),
    prometheus_gauge:dec(temperature, 2.7),

    PSValue = prometheus_gauge:value(pool_size, [mongodb]),
    TValue = prometheus_gauge:value(temperature),

    prometheus_gauge:reset(pool_size, [mongodb]),
    RValue = prometheus_gauge:value(pool_size, [mongodb]),
    [
        ?_assertMatch(_ when PSValue > 3.4 andalso PSValue < 3.6, PSValue),
        ?_assertMatch(_ when TValue > 1.2 andalso TValue < 1.4, TValue),
        ?_assertEqual(0, RValue)
    ].

test_set_to_current_time(_) ->
    prometheus_gauge:new([{name, cur_time}, {labels, []}, {help, ""}]),
    Timestamp = os:system_time(seconds),
    prometheus_gauge:set_to_current_time(cur_time),
    STimestamp = prometheus_gauge:value(cur_time),
    [?_assertEqual(Timestamp, STimestamp)].

test_track_inprogress(_) ->
    prometheus_gauge:new([{name, gauge}, {help, ""}]),
    Value = prometheus_gauge:track_inprogress(
        gauge,
        fun() ->
            prometheus_gauge:value(gauge)
        end
    ),

    try
        prometheus_gauge:track_inprogress(gauge, fun() ->
            erlang:error({qwe})
        end)
    catch
        _:_ -> ok
    end,

    [
        ?_assertEqual(1, Value),
        ?_assertEqual(0, prometheus_gauge:value(gauge))
    ].

test_set_duration_seconds(_) ->
    prometheus_gauge:new([
        {name, gauge_seconds},
        {help, ""}
    ]),
    ValueF = prometheus_gauge:set_duration(gauge_seconds, fun() ->
        timer:sleep(1000),
        1
    end),
    Value = prometheus_gauge:value(gauge_seconds),

    [MF] = prometheus_collector:collect_mf_to_list(prometheus_gauge),

    #'MetricFamily'{
        metric =
            [
                #'Metric'{
                    gauge =
                        #'Gauge'{value = MFValue}
                }
            ]
    } = MF,

    try
        prometheus_gauge:set_duration(gauge_seconds, fun() ->
            erlang:error({qwe})
        end)
    catch
        _:_ -> ok
    end,

    ValueE = prometheus_gauge:value(gauge_seconds),

    [
        ?_assertMatch(1, ValueF),
        ?_assertMatch(true, 0.9 < Value andalso Value < 1.2),
        ?_assertMatch(true, 0.9 < MFValue andalso MFValue < 1.2),
        ?_assertMatch(true, 0.0 < ValueE andalso ValueE < 0.1)
    ].

test_set_duration_milliseconds(_) ->
    prometheus_gauge:new([
        {name, gauge},
        {help, ""},
        {duration_unit, milliseconds}
    ]),
    ValueF = prometheus_gauge:set_duration(gauge, fun() ->
        timer:sleep(1100),
        1
    end),
    Value = prometheus_gauge:value(gauge),

    try
        prometheus_gauge:set_duration(gauge, fun() ->
            erlang:error({qwe})
        end)
    catch
        _:_ -> ok
    end,

    ValueE = prometheus_gauge:value(gauge),

    [
        ?_assertMatch(1, ValueF),
        ?_assertMatch(true, 900 < Value andalso Value < 1200),
        ?_assertMatch(true, 0 < ValueE andalso ValueE < 100)
    ].

test_get_value_from_undefined_with_float(_) ->
    prometheus_gauge:new([{name, gauge_seconds}, {labels, [client]}, {help, ""}]),
    prometheus_gauge:inc(gauge_seconds, [redis], 2),
    prometheus_gauge:inc(gauge_seconds, [redis], 1.2),
    Value = prometheus_gauge:value(gauge_seconds, [redis]),
    prometheus_gauge:set(gauge_seconds, [redis], undefined),
    ValueUndefined = prometheus_gauge:value(gauge_seconds, [redis]),
    [
        ?_assertError(
            {invalid_operation, 'inc/dec', "Can't inc/dec undefined"},
            prometheus_gauge:inc(gauge_seconds, [redis], 1)
        ),
        ?_assertError(
            {invalid_operation, 'inc/dec', "Can't inc/dec undefined"},
            prometheus_gauge:inc(gauge_seconds, [redis], 0.8)
        ),
        ?_assertEqual(3.0e-9, Value),
        ?_assertEqual(undefined, ValueUndefined)
    ].

test_deregister(_) ->
    prometheus_gauge:new([
        {name, pool_size},
        {labels, [pool]},
        {help, "Http request pool size"}
    ]),
    prometheus_gauge:new([{name, simple_gauge}, {help, ""}]),

    prometheus_gauge:inc(pool_size, [mongodb]),
    prometheus_gauge:inc(simple_gauge),

    [
        ?_assertMatch({true, true}, prometheus_gauge:deregister(pool_size)),
        ?_assertMatch({false, false}, prometheus_gauge:deregister(pool_size)),
        ?_assertEqual(2, length(ets:tab2list(prometheus_gauge_table))),
        ?_assertEqual(1, prometheus_gauge:value(simple_gauge))
    ].

test_remove(_) ->
    prometheus_gauge:new([
        {name, pool_size},
        {labels, [pool]},
        {help, "pool size"}
    ]),
    prometheus_gauge:new([{name, simple_gauge}, {help, ""}]),

    prometheus_gauge:inc(pool_size, [mongodb]),
    prometheus_gauge:inc(simple_gauge),

    BRValue1 = prometheus_gauge:value(pool_size, [mongodb]),
    BRValue2 = prometheus_gauge:value(simple_gauge),

    RResult1 = prometheus_gauge:remove(pool_size, [mongodb]),
    RResult2 = prometheus_gauge:remove(simple_gauge),

    ARValue1 = prometheus_gauge:value(pool_size, [mongodb]),
    ARValue2 = prometheus_gauge:value(simple_gauge),

    RResult3 = prometheus_gauge:remove(pool_size, [mongodb]),
    RResult4 = prometheus_gauge:remove(simple_gauge),

    [
        ?_assertEqual(1, BRValue1),
        ?_assertEqual(1, BRValue2),
        ?_assertEqual(true, RResult1),
        ?_assertEqual(true, RResult2),
        ?_assertEqual(undefined, ARValue1),
        ?_assertEqual(undefined, ARValue2),
        ?_assertEqual(false, RResult3),
        ?_assertEqual(false, RResult4)
    ].

test_default_value(_) ->
    prometheus_gauge:new([
        {name, pool_size},
        {labels, [client]},
        {help, ""}
    ]),
    UndefinedValue = prometheus_gauge:value(pool_size, [post]),

    prometheus_gauge:new([
        {name, something_gauge},
        {labels, []},
        {help, ""}
    ]),
    SomethingValue = prometheus_gauge:value(something_gauge),

    [
        ?_assertEqual(undefined, UndefinedValue),
        ?_assertEqual(0, SomethingValue)
    ].

test_values(_) ->
    prometheus_gauge:new([
        {name, pool_size},
        {labels, [pool]},
        {help, "pool size"}
    ]),
    prometheus_gauge:set(pool_size, [mongodb], 10),
    prometheus_gauge:inc(pool_size, [postgres], 13),

    [
        ?_assertEqual(
            [
                {[{pool, mongodb}], 10},
                {[{pool, postgres}], 13}
            ],
            lists:sort(prometheus_gauge:values(default, pool_size))
        )
    ].

test_collector1(_) ->
    prometheus_gauge:new([
        {name, simple_gauge},
        {labels, ["label"]},
        {help, ""}
    ]),
    prometheus_gauge:set(simple_gauge, [label_value], 1),
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
                                gauge = #'Gauge'{value = 1}
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_gauge)
        )
    ].

test_collector2(_) ->
    prometheus_gauge:new([
        {name, simple_gauge},
        {labels, ["label"]},
        {constant_labels, #{qwe => qwa}},
        {help, ""}
    ]),
    prometheus_gauge:set(simple_gauge, [label_value], 1),
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
                                gauge = #'Gauge'{value = 1}
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_gauge)
        )
    ].

test_collector3(_) ->
    MFList =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_gauge:new([
                {name, simple_gauge},
                {labels, ["label"]},
                {help, ""}
            ]),
            prometheus_gauge:set(simple_gauge, [label_value], 1),
            prometheus_collector:collect_mf_to_list(prometheus_gauge)
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
                                gauge = #'Gauge'{value = 1}
                            }
                        ]
                }
            ],
            MFList
        )
    ].
