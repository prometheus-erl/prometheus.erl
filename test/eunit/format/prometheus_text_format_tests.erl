-module(prometheus_text_format_tests).

-include_lib("eunit/include/eunit.hrl").

-export([
    deregister_cleanup/1,
    collect_mf/2,
    collect_metrics/2
]).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
    Callback(
        create_untyped(
            pool_size,
            "MongoDB Connections pool size"
        )
    ),

    ok.

collect_metrics(pool_size, _) ->
    prometheus_model_helpers:untyped_metric(365).

create_untyped(Name, Help) ->
    prometheus_model_helpers:create_mf(Name, Help, untyped, ?MODULE, undefined).

escape_metric_help_test() ->
    ?assertEqual(
        <<"qwe\\\\qwe\\nqwe">>,
        prometheus_text_format:escape_metric_help("qwe\\qwe\nqwe")
    ).

escape_label_value_test() ->
    ?assertEqual(
        <<"qwe\\\\qwe\\nq\\\"we\\\"qwe">>,
        prometheus_text_format:escape_label_value("qwe\\qwe\nq\"we\"qwe")
    ).

escape_label_value_no_special_char_test() ->
    LabelBin = <<"qweqweqwe">>,
    ?assert(erts_debug:same(LabelBin, prometheus_text_format:escape_label_value(LabelBin))).

prometheus_format_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_gauge/1,
        fun test_untyped/1,
        fun test_nan_gauge/1,
        fun test_counter/1,
        fun test_dcounter/1,
        fun test_summary/1,
        fun test_dsummary/1,
        fun test_quantile_summary/1,
        fun test_quantile_dsummary/1,
        fun test_histogram/1,
        fun test_histogram_float/1,
        fun test_dhistogram/1
    ]}.

content_type_test() ->
    ?assertEqual(<<"text/plain; version=0.0.4">>, prometheus_text_format:content_type()).

test_gauge(_) ->
    prometheus_gauge:new([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
    prometheus_gauge:set(pool_size, 365),
    ?_assertEqual(
        <<
            "# TYPE pool_size gauge\n"
            "# HELP pool_size MongoDB Connections pool size\n"
            "pool_size 365\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_untyped(_) ->
    prometheus_registry:register_collector(?MODULE),
    ?_assertEqual(
        <<
            "# TYPE pool_size untyped\n"
            "# HELP pool_size MongoDB Connections pool size\n"
            "pool_size 365\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_nan_gauge(_) ->
    prometheus_gauge:new([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
    prometheus_gauge:set(pool_size, undefined),
    ?_assertEqual(
        <<
            "# TYPE pool_size gauge\n"
            "# HELP pool_size MongoDB Connections pool size\n"
            "pool_size NaN\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_counter(_) ->
    prometheus_counter:new([{name, http_requests_total}, {help, "Http request count"}]),
    prometheus_counter:inc(http_requests_total),
    ?_assertEqual(
        <<
            "# TYPE http_requests_total counter\n"
            "# HELP http_requests_total Http request count\n"
            "http_requests_total 1\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_dcounter(_) ->
    prometheus_counter:new([{name, dtest}, {help, "qw\"\\e"}]),
    prometheus_counter:inc(dtest, 1.5),
    prometheus_counter:inc(dtest, 3.5),
    prometheus_counter:inc(dtest, 1.5),

    ?_assertEqual(
        <<
            "# TYPE dtest counter\n"
            "# HELP dtest qw\"\\\\e\n"
            "dtest 6.5\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_summary(_) ->
    prometheus_summary:new([
        {name, orders_summary},
        {help, "Track orders count/total sum"}
    ]),
    prometheus_summary:observe(orders_summary, 10),
    prometheus_summary:observe(orders_summary, 15),
    ?_assertEqual(
        <<
            "# TYPE orders_summary summary\n"
            "# HELP orders_summary Track orders count/total sum\n"
            "orders_summary_count 2\n"
            "orders_summary_sum 25\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_dsummary(_) ->
    prometheus_summary:new([{name, dsummary}, {labels, [host]}, {help, "qwe"}]),
    prometheus_summary:observe(dsummary, [123], 1.5),
    prometheus_summary:observe(dsummary, [123], 2.7),

    ?_assertEqual(
        <<
            "# TYPE dsummary summary\n"
            "# HELP dsummary qwe\n"
            "dsummary_count{host=\"123\"} 2\n"
            "dsummary_sum{host=\"123\"} 4.2\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_quantile_summary(_) ->
    prometheus_quantile_summary:new([
        {name, orders_quantile_summary},
        {help, "Track orders count/total sum"}
    ]),
    prometheus_quantile_summary:observe(orders_quantile_summary, 10),
    prometheus_quantile_summary:observe(orders_quantile_summary, 15),
    ?_assertEqual(
        <<
            "# TYPE orders_quantile_summary summary\n"
            "# HELP orders_quantile_summary Track orders count/total sum\n"
            "orders_quantile_summary_count 2\n"
            "orders_quantile_summary_sum 25\n"
            "orders_quantile_summary{quantile=\"0.5\"} 15\n"
            "orders_quantile_summary{quantile=\"0.9\"} 15\n"
            "orders_quantile_summary{quantile=\"0.95\"} 15\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_quantile_dsummary(_) ->
    prometheus_quantile_summary:new([{name, quantile_dsummary}, {labels, [host]}, {help, "qwe"}]),
    prometheus_quantile_summary:observe(quantile_dsummary, [123], 1.5),
    prometheus_quantile_summary:observe(quantile_dsummary, [123], 2.7),

    ?_assertEqual(
        <<
            "# TYPE quantile_dsummary summary\n"
            "# HELP quantile_dsummary qwe\n"
            "quantile_dsummary_count{host=\"123\"} 2\n"
            "quantile_dsummary_sum{host=\"123\"} 4.2\n"
            "quantile_dsummary{host=\"123\",quantile=\"0.5\"} 2.7\n"
            "quantile_dsummary{host=\"123\",quantile=\"0.9\"} 2.7\n"
            "quantile_dsummary{host=\"123\",quantile=\"0.95\"} 2.7\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_histogram(_) ->
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
    ?_assertEqual(
        <<
            "# TYPE http_request_duration_milliseconds histogram\n"
            "# HELP http_request_duration_milliseconds Http Request execution time\n"
            "http_request_duration_milliseconds_bucket{method=\"get\",le=\"100\"} 3\n"
            "http_request_duration_milliseconds_bucket{method=\"get\",le=\"300\"} 6\n"
            "http_request_duration_milliseconds_bucket{method=\"get\",le=\"500\"} 7\n"
            "http_request_duration_milliseconds_bucket{method=\"get\",le=\"750\"} 8\n"
            "http_request_duration_milliseconds_bucket{method=\"get\",le=\"1000\"} 9\n"
            "http_request_duration_milliseconds_bucket{method=\"get\",le=\"+Inf\"} 9\n"
            "http_request_duration_milliseconds_count{method=\"get\"} 9\n"
            "http_request_duration_milliseconds_sum{method=\"get\"} 2622\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_histogram_float(_) ->
    prometheus_histogram:new([
        {name, http_request_duration_seconds},
        {labels, [method]},
        {buckets, [0.01, 0.1, 0.5, 1, 3]},
        {help, "Http Request execution time"},
        {duration_unit, false}
    ]),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 0.95),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 0.1),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 0.102),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 0.15),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 0.25),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 0.35),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 0.55),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 1.55),
    prometheus_histogram:observe(http_request_duration_seconds, [get], 2.05),
    ?_assertEqual(
        <<
            "# TYPE http_request_duration_seconds histogram\n"
            "# HELP http_request_duration_seconds Http Request execution time\n"
            "http_request_duration_seconds_bucket{method=\"get\",le=\"0.01\"} 0\n"
            "http_request_duration_seconds_bucket{method=\"get\",le=\"0.1\"} 1\n"
            "http_request_duration_seconds_bucket{method=\"get\",le=\"0.5\"} 5\n"
            "http_request_duration_seconds_bucket{method=\"get\",le=\"1\"} 7\n"
            "http_request_duration_seconds_bucket{method=\"get\",le=\"3\"} 9\n"
            "http_request_duration_seconds_bucket{method=\"get\",le=\"+Inf\"} 9\n"
            "http_request_duration_seconds_count{method=\"get\"} 9\n"
            "http_request_duration_seconds_sum{method=\"get\"} 6.052\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).

test_dhistogram(_) ->
    prometheus_histogram:new([
        {name, http_request_duration_milliseconds},
        {labels, [method]},
        {buckets, [100, 300, 500, 750, 1000]},
        {help, "Http Request execution time"},
        {duration_unit, false}
    ]),
    prometheus_histogram:observe(http_request_duration_milliseconds, [post], 500.2),
    prometheus_histogram:observe(http_request_duration_milliseconds, [post], 150.4),
    prometheus_histogram:observe(http_request_duration_milliseconds, [post], 450.5),
    prometheus_histogram:observe(http_request_duration_milliseconds, [post], 850.3),
    prometheus_histogram:observe(http_request_duration_milliseconds, [post], 750.9),
    prometheus_histogram:observe(http_request_duration_milliseconds, [post], 1650.23),

    ?_assertEqual(
        <<
            "# TYPE http_request_duration_milliseconds histogram\n"
            "# HELP http_request_duration_milliseconds Http Request execution time\n"
            "http_request_duration_milliseconds_bucket{method=\"post\",le=\"100\"} 0\n"
            "http_request_duration_milliseconds_bucket{method=\"post\",le=\"300\"} 1\n"
            "http_request_duration_milliseconds_bucket{method=\"post\",le=\"500\"} 2\n"
            "http_request_duration_milliseconds_bucket{method=\"post\",le=\"750\"} 3\n"
            "http_request_duration_milliseconds_bucket{method=\"post\",le=\"1000\"} 5\n"
            "http_request_duration_milliseconds_bucket{method=\"post\",le=\"+Inf\"} 6\n"
            "http_request_duration_milliseconds_count{method=\"post\"} 6\n"
            "http_request_duration_milliseconds_sum{method=\"post\"} 4352.53\n"
            "\n"
        >>,
        prometheus_text_format:format()
    ).
