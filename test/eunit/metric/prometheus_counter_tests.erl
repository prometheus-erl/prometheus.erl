-module(prometheus_counter_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_registration/1,
        fun test_errors/1,
        fun test_inc/1,
        fun test_deregister/1,
        fun test_remove/1,
        fun test_default_value/1,
        fun test_values/1,
        fun test_collector1/1,
        fun test_collector2/1,
        fun test_collector3/1
    ]}.

test_registration(_) ->
    Name = http_requests_total,
    SpecWithRegistry = [
        {name, Name},
        {help, ""},
        {registry, qwe}
    ],
    [
        ?_assertEqual(
            true,
            prometheus_counter:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_counter:new(SpecWithRegistry)
        )
    ].

test_errors(_) ->
    prometheus_counter:new([{name, db_query_duration}, {labels, [repo]}, {help, ""}]),

    %% basic name/labels/help validations test
    [
        ?_assertError(
            {invalid_metric_name, 12, "metric name is not a string"},
            prometheus_counter:new([{name, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_labels, 12, "not list"},
            prometheus_counter:new([{name, "qwe"}, {labels, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_help, 12, "metric help is not a string"},
            prometheus_counter:new([{name, "qwe"}, {help, 12}])
        ),

        %% counter specific errors
        ?_assertError(
            {invalid_value, -1, "inc accepts only non-negative numbers"},
            prometheus_counter:inc(http_requests_total, -1)
        ),
        ?_assertError(
            {invalid_value, "qwe", "inc accepts only non-negative numbers"},
            prometheus_counter:inc(http_requests_total, [], "qwe")
        ),

        %% mf/arity errors
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_counter:inc(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_counter:inc(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_counter:reset(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_counter:reset(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_counter:value(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_counter:value(db_query_duration, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_counter:remove(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_counter:remove(db_query_duration, [repo, db])
        )
    ].

test_inc(_) ->
    prometheus_counter:new([
        {name, http_requests_total},
        {labels, [method]},
        {help, "Http request count"}
    ]),
    prometheus_counter:inc(http_requests_total, [get]),
    prometheus_counter:inc(http_requests_total, [get], 3),
    prometheus_counter:inc(http_requests_total, [get], 3.5),
    Value = prometheus_counter:value(http_requests_total, [get]),
    prometheus_counter:reset(http_requests_total, [get]),
    RValue = prometheus_counter:value(http_requests_total, [get]),
    [
        ?_assertMatch(_ when Value > 7.4 andalso Value < 7.6, Value),
        ?_assertEqual(0, RValue)
    ].

test_deregister(_) ->
    prometheus_counter:new([
        {name, http_requests_total},
        {labels, [method]},
        {help, "Http request count"}
    ]),
    prometheus_counter:new([{name, simple_counter}, {help, ""}]),

    prometheus_counter:inc(http_requests_total, [get]),
    prometheus_counter:inc(simple_counter),

    [
        ?_assertMatch({true, true}, prometheus_counter:deregister(http_requests_total)),
        ?_assertMatch({false, false}, prometheus_counter:deregister(http_requests_total)),
        ?_assertEqual(2, length(ets:tab2list(prometheus_counter_table))),
        ?_assertEqual(1, prometheus_counter:value(simple_counter))
    ].

test_remove(_) ->
    prometheus_counter:new([
        {name, http_requests_total},
        {labels, [method]},
        {help, "Http request count"}
    ]),
    prometheus_counter:new([{name, simple_counter}, {help, ""}]),

    prometheus_counter:inc(http_requests_total, [get]),
    prometheus_counter:inc(simple_counter),

    BRValue1 = prometheus_counter:value(http_requests_total, [get]),
    BRValue2 = prometheus_counter:value(simple_counter),

    RResult1 = prometheus_counter:remove(http_requests_total, [get]),
    RResult2 = prometheus_counter:remove(simple_counter),

    ARValue1 = prometheus_counter:value(http_requests_total, [get]),
    ARValue2 = prometheus_counter:value(simple_counter),

    RResult3 = prometheus_counter:remove(http_requests_total, [get]),
    RResult4 = prometheus_counter:remove(simple_counter),

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
    prometheus_counter:new([
        {name, http_requests_total},
        {labels, [method]},
        {help, "Http request count"}
    ]),
    UndefinedValue = prometheus_counter:value(http_requests_total, [post]),

    prometheus_counter:new([
        {name, something_total},
        {labels, []},
        {help, ""}
    ]),
    SomethingValue = prometheus_counter:value(something_total),

    [
        ?_assertEqual(undefined, UndefinedValue),
        ?_assertEqual(0, SomethingValue)
    ].

test_values(_) ->
    prometheus_counter:new([
        {name, http_requests_total},
        {labels, [method]},
        {help, "Http request count"}
    ]),

    prometheus_counter:inc(http_requests_total, [get], 4),
    prometheus_counter:inc(http_requests_total, [post], 56),

    [
        ?_assertEqual(
            [
                {[{method, get}], 4},
                {[{method, post}], 56}
            ],
            lists:sort(prometheus_counter:values(default, http_requests_total))
        )
    ].

test_collector1(_) ->
    prometheus_counter:new([
        {name, simple_counter},
        {labels, ["label"]},
        {help, ""}
    ]),
    prometheus_counter:inc(simple_counter, [label_value], 1),
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
                                counter = #'Counter'{value = 1}
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_counter)
        )
    ].

test_collector2(_) ->
    prometheus_counter:new([
        {name, simple_counter},
        {labels, ["label"]},
        {constant_labels, #{qwe => qwa}},
        {help, ""}
    ]),
    prometheus_counter:inc(simple_counter, [label_value], 1),
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
                                counter = #'Counter'{value = 1}
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_counter)
        )
    ].

test_collector3(_) ->
    MFList =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_counter:new([
                {name, simple_counter},
                {labels, ["label"]},
                {help, ""}
            ]),
            prometheus_counter:inc(simple_counter, [label_value], 1),
            prometheus_collector:collect_mf_to_list(prometheus_counter)
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
                                counter = #'Counter'{value = 1}
                            }
                        ]
                }
            ],
            MFList
        )
    ].
