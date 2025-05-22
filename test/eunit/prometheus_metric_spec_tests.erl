-module(prometheus_metric_spec_tests).

-include_lib("eunit/include/eunit.hrl").

get_value_test() ->
    Spec1 = [{name, "qwe"}],
    Spec2 = #{name => "qwe"},

    ?assertMatch(
        undefined,
        prometheus_metric_spec:get_value(labels, Spec1)
    ),
    ?assertMatch(
        [default],
        prometheus_metric_spec:get_value(labels, Spec1, [default])
    ),

    ?assertMatch(
        undefined,
        prometheus_metric_spec:get_value(labels, Spec2)
    ),
    ?assertMatch(
        [default],
        prometheus_metric_spec:get_value(labels, Spec2, [default])
    ),

    ?assertEqual("qwe", prometheus_metric_spec:get_value(name, Spec1)),
    ?assertEqual("qwe", prometheus_metric_spec:get_value(name, Spec2)).

fetch_value_test() ->
    Spec1 = [{name, "qwe"}],
    Spec2 = #{name => "qwe"},

    ?assertError(
        {missing_metric_spec_key, labels, Spec1},
        prometheus_metric_spec:fetch_value(labels, Spec1)
    ),

    ?assertError(
        {missing_metric_spec_key, labels, Spec2},
        prometheus_metric_spec:fetch_value(labels, Spec2)
    ),

    ?assertEqual("qwe", prometheus_metric_spec:fetch_value(name, Spec1)),
    ?assertEqual("qwe", prometheus_metric_spec:fetch_value(name, Spec2)).

validate_metric_name_test() ->
    ?assertError(
        {invalid_metric_name, 12, "metric name is not a string"},
        prometheus_metric_spec:validate_metric_name(12)
    ),
    ?assertError(
        {invalid_metric_name, <<0, 0, 123>>, "metric name is invalid string"},
        prometheus_metric_spec:validate_metric_name(<<0, 0, 123>>)
    ),
    ?assertError(
        {invalid_metric_name, "1qwe", "metric name doesn't match regex ^[a-zA-Z_:][a-zA-Z0-9_:]*$"},
        prometheus_metric_spec:validate_metric_name("1qwe")
    ),

    ?assertEqual('qwe_:qwe', prometheus_metric_spec:validate_metric_name('qwe_:qwe')),
    ?assertEqual("qwe_:qwe", prometheus_metric_spec:validate_metric_name("qwe_:qwe")),
    ?assertEqual(
        <<"qwe_:qwe">>,
        prometheus_metric_spec:validate_metric_name(<<"qwe_:qwe">>)
    ).

validate_metric_label_names_test() ->
    ?assertError(
        {invalid_metric_labels, 12, "not list"},
        prometheus_metric_spec:validate_metric_label_names(12)
    ),
    ?assertError(
        {invalid_metric_label_name, 12, "metric label is not a string"},
        prometheus_metric_spec:validate_metric_label_names([12])
    ),
    ?assertError(
        {invalid_metric_label_name, [0, 0, 123], "metric label is invalid string"},
        prometheus_metric_spec:validate_metric_label_names([<<0, 0, 123>>])
    ),
    ?assertError(
        {invalid_metric_label_name, "__qwe", "metric label can't start with __"},
        prometheus_metric_spec:validate_metric_label_names(["__qwe"])
    ),
    ?assertError(
        {invalid_metric_label_name, "qwe:",
            "metric label doesn't match regex ^[a-zA-Z_][a-zA-Z0-9_]*$"},
        prometheus_metric_spec:validate_metric_label_names(["qwe:"])
    ),

    ?assertEqual(
        [<<"_qwe">>, weq123],
        prometheus_metric_spec:validate_metric_label_names([
            <<"_qwe">>,
            weq123
        ])
    ),
    ?assertEqual(
        [<<"_qwe">>, "weq123"],
        prometheus_metric_spec:validate_metric_label_names([
            <<"_qwe">>,
            "weq123"
        ])
    ).

validate_metric_help_test() ->
    ?assertError(
        {invalid_metric_help, 12, "metric help is not a string"},
        prometheus_metric_spec:validate_metric_help(12)
    ),
    ?assertError(
        {invalid_metric_help, [0, 0, 123], "metric help is invalid string"},
        prometheus_metric_spec:validate_metric_help(<<0, 0, 123>>)
    ),

    ?assertEqual("qwe_:qwe", prometheus_metric_spec:validate_metric_help("qwe_:qwe")),
    ?assertEqual(<<"qwe_:qwe">>, prometheus_metric_spec:validate_metric_help(<<"qwe_:qwe">>)).

constant_labels_test() ->
    ?assertEqual(
        [{qwe, qwa}], prometheus_metric_spec:constant_labels([{constant_labels, #{qwe => qwa}}])
    ),

    ?assertError(
        {invalid_value, invalid, "constant labels is not a map"},
        prometheus_metric_spec:constant_labels([{constant_labels, invalid}])
    ),

    ?assertError(
        {invalid_metric_label_name, [],
            "metric label doesn't match regex ^[a-zA-Z_][a-zA-Z0-9_]*$"},
        prometheus_metric_spec:constant_labels([{constant_labels, #{[] => qwe}}])
    ).

duration_unit_test() ->
    ?assertEqual(undefined, prometheus_metric_spec:duration_unit([{name, "qwe"}])),
    ?assertEqual(
        milliseconds,
        prometheus_metric_spec:duration_unit([{name, "request_duration_milliseconds"}])
    ),
    ?assertEqual(
        undefined,
        prometheus_metric_spec:duration_unit([
            {name, "request_duration_milliseconds"},
            {duration_unit, false}
        ])
    ),
    ?assertEqual(
        microseconds,
        prometheus_metric_spec:duration_unit([
            {name, "qwe"},
            {duration_unit, microseconds}
        ])
    ),
    ?assertError(
        {invalid_value, invalid, "unknown duration unit"},
        prometheus_metric_spec:duration_unit([
            {name, "qwe"},
            {duration_unit, invalid}
        ])
    ),
    ?assertError(
        {invalid_value, microseconds, "duration unit doesn't match metric name"},
        prometheus_metric_spec:duration_unit([
            {name, "request_duration_milliseconds"},
            {duration_unit, microseconds}
        ])
    ).

extract_common_params_test() ->
    ?assertError(
        {invalid_metric_name, 12, "metric name is not a string"},
        prometheus_metric_spec:extract_common_params([{name, 12}])
    ),
    ?assertError(
        {invalid_metric_labels, 12, "not list"},
        prometheus_metric_spec:extract_common_params([
            {name, "qwe"},
            {labels, 12}
        ])
    ),
    ?assertError(
        {invalid_metric_help, 12, "metric help is not a string"},
        prometheus_metric_spec:extract_common_params([
            {name, "qwe"},
            {labels, ["qwe"]},
            {help, 12}
        ])
    ),

    ?assertEqual(
        {default, "qwe", [], "qwe", [], undefined, undefined},
        prometheus_metric_spec:extract_common_params([
            {name, "qwe"},
            {help, "qwe"}
        ])
    ),
    ?assertEqual(
        {qwe, "qwe", ["qwe"], "qwe", [{qwe, qwa}], undefined, data},
        prometheus_metric_spec:extract_common_params([
            {name, "qwe"},
            {labels, ["qwe"]},
            {constant_labels, #{qwe => qwa}},
            {help, "qwe"},
            {registry, qwe},
            {data, data}
        ])
    ).
