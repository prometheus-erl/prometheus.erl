-module(prometheus_boolean_tests).

-include_lib("eunit/include/eunit.hrl").

-include("prometheus_model.hrl").

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_registration/1,
        fun test_errors/1,
        fun test_set/1,
        fun test_toggle/1,
        fun test_deregister/1,
        fun test_remove/1,
        fun test_default_value/1,
        fun test_collector1/1,
        fun test_collector2/1,
        fun test_collector3/1,
        fun test_values/1
    ]}.

test_registration(_) ->
    Name = fuse_state,
    SpecWithRegistry = [
        {name, Name},
        {help, ""},
        {registry, qwe}
    ],
    [
        ?_assertEqual(
            true,
            prometheus_boolean:declare(SpecWithRegistry)
        ),
        ?_assertError(
            {mf_already_exists, {qwe, Name}, "Consider using declare instead."},
            prometheus_boolean:new(SpecWithRegistry)
        )
    ].

test_errors(_) ->
    prometheus_boolean:new([{name, with_label}, {labels, [label]}, {help, ""}]),

    %% basic name/labels/help validations test
    [
        ?_assertError(
            {invalid_metric_name, 12, "metric name is not a string"},
            prometheus_boolean:new([{name, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_labels, 12, "not list"},
            prometheus_boolean:new([{name, "qwe"}, {labels, 12}, {help, ""}])
        ),
        ?_assertError(
            {invalid_metric_help, 12, "metric help is not a string"},
            prometheus_boolean:new([{name, "qwe"}, {help, 12}])
        ),

        %% boolean specific errors,
        ?_assertError(
            {invalid_value, #{}, "value is not boolean"},
            prometheus_boolean:set(fuse_state, #{})
        ),
        begin
            prometheus_boolean:set(with_label, [label], undefined),
            ?_assertError(
                {invalid_value, undefined, "can't toggle undefined boolean"},
                prometheus_boolean:toggle(with_label, [label])
            )
        end,

        %% mf/arity errors
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_boolean:set(unknown_metric, 2)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_boolean:set(with_label, [repo, db], 2)
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_boolean:reset(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_boolean:reset(with_label, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_boolean:value(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_boolean:value(with_label, [repo, db])
        ),
        ?_assertError(
            {unknown_metric, default, unknown_metric},
            prometheus_boolean:remove(unknown_metric)
        ),
        ?_assertError(
            {invalid_metric_arity, 2, 1},
            prometheus_boolean:remove(with_label, [repo, db])
        )
    ].

test_set(_) ->
    prometheus_boolean:new([{name, fuse_state}, {labels, [name]}, {help, ""}]),
    prometheus_boolean:set(fuse_state, [mongodb], 110),
    Value = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:set(fuse_state, [mongodb], 0),
    Value1 = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:set(fuse_state, [mongodb], true),
    Value = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:set(fuse_state, [mongodb], false),
    Value1 = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:set(fuse_state, [mongodb], [1]),
    Value = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:set(fuse_state, [mongodb], []),
    Value1 = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:reset(fuse_state, [mongodb]),
    RValue = prometheus_boolean:value(fuse_state, [mongodb]),
    [
        ?_assertEqual(true, Value),
        ?_assertEqual(false, Value1),
        ?_assertEqual(false, RValue)
    ].

test_toggle(_) ->
    prometheus_boolean:new([{name, fuse_state}, {labels, [name]}, {help, ""}]),
    prometheus_boolean:set(fuse_state, [mongodb], true),

    PSValue = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:toggle(fuse_state, [mongodb]),
    TValue1 = prometheus_boolean:value(fuse_state, [mongodb]),
    prometheus_boolean:toggle(fuse_state, [mongodb]),
    TValue2 = prometheus_boolean:value(fuse_state, [mongodb]),
    [
        ?_assertEqual(true, PSValue),
        ?_assertEqual(false, TValue1),
        ?_assertEqual(true, TValue2)
    ].

test_deregister(_) ->
    prometheus_boolean:new([{name, fuse_state}, {labels, [pool]}, {help, ""}]),
    prometheus_boolean:new([{name, simple_boolean}, {help, ""}]),

    prometheus_boolean:set(fuse_state, [mongodb], true),
    prometheus_boolean:set(simple_boolean, true),

    [
        ?_assertMatch({true, true}, prometheus_boolean:deregister(fuse_state)),
        ?_assertMatch({false, false}, prometheus_boolean:deregister(fuse_state)),
        ?_assertEqual(2, length(ets:tab2list(prometheus_boolean_table))),
        ?_assertEqual(true, prometheus_boolean:value(simple_boolean))
    ].

test_remove(_) ->
    prometheus_boolean:new([
        {name, fuse_state},
        {labels, [pool]},
        {help, ""}
    ]),
    prometheus_boolean:new([{name, simple_boolean}, {help, ""}]),

    prometheus_boolean:set(fuse_state, [mongodb], true),
    prometheus_boolean:set(simple_boolean, true),

    BRValue1 = prometheus_boolean:value(fuse_state, [mongodb]),
    BRValue2 = prometheus_boolean:value(simple_boolean),

    RResult1 = prometheus_boolean:remove(fuse_state, [mongodb]),
    RResult2 = prometheus_boolean:remove(simple_boolean),

    ARValue1 = prometheus_boolean:value(fuse_state, [mongodb]),
    ARValue2 = prometheus_boolean:value(simple_boolean),

    RResult3 = prometheus_boolean:remove(fuse_state, [mongodb]),
    RResult4 = prometheus_boolean:remove(simple_boolean),

    [
        ?_assertEqual(true, BRValue1),
        ?_assertEqual(true, BRValue2),
        ?_assertEqual(true, RResult1),
        ?_assertEqual(true, RResult2),
        ?_assertEqual(undefined, ARValue1),
        ?_assertEqual(undefined, ARValue2),
        ?_assertEqual(false, RResult3),
        ?_assertEqual(false, RResult4)
    ].

test_default_value(_) ->
    prometheus_boolean:new([
        {name, fuse_state},
        {labels, [name]},
        {help, ""}
    ]),
    UndefinedValue = prometheus_boolean:value(fuse_state, [post]),

    prometheus_boolean:new([
        {name, something_boolean},
        {labels, []},
        {help, ""}
    ]),
    SomethingValue = prometheus_boolean:value(something_boolean),

    [
        ?_assertEqual(undefined, UndefinedValue),
        ?_assertEqual(undefined, SomethingValue)
    ].

test_values(_) ->
    prometheus_boolean:new([
        {name, fuse_state},
        {labels, [name]},
        {help, ""}
    ]),
    prometheus_boolean:set(fuse_state, [mysql], true),
    prometheus_boolean:set(fuse_state, [postgres], false),

    [
        ?_assertEqual(
            [
                {[{name, mysql}], true},
                {[{name, postgres}], false}
            ],
            lists:sort(prometheus_boolean:values(default, fuse_state))
        )
    ].

test_collector1(_) ->
    prometheus_boolean:new([
        {name, simple_boolean},
        {labels, ["label"]},
        {help, ""}
    ]),
    prometheus_boolean:set(simple_boolean, [label_value], true),
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
                                untyped = #'Untyped'{value = 1}
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_boolean)
        )
    ].

test_collector2(_) ->
    prometheus_boolean:new([
        {name, simple_boolean},
        {labels, ["label"]},
        {constant_labels, #{qwe => qwa}},
        {help, ""}
    ]),
    prometheus_boolean:set(simple_boolean, [label_value], false),
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
                                untyped = #'Untyped'{value = 0}
                            }
                        ]
                }
            ],
            prometheus_collector:collect_mf_to_list(prometheus_boolean)
        )
    ].

test_collector3(_) ->
    MFList =
        try
            prometheus:start(),
            application:set_env(prometheus, global_labels, [{node, node()}]),
            prometheus_boolean:new([
                {name, simple_boolean},
                {labels, ["label"]},
                {help, ""}
            ]),
            prometheus_boolean:set(simple_boolean, [label_value], false),
            prometheus_collector:collect_mf_to_list(prometheus_boolean)
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
                                untyped = #'Untyped'{value = 0}
                            }
                        ]
                }
            ],
            MFList
        )
    ].
