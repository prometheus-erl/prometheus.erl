-module(prometheus_text_format_tests).

-include_lib("eunit/include/eunit.hrl").

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  Callback(create_untyped(pool_size,
                          "MongoDB Connections pool size")),

  ok.

collect_metrics(pool_size, _) ->
  prometheus_model_helpers:untyped_metric(365).

create_untyped(Name, Help) ->
  prometheus_model_helpers:create_mf(Name, Help, untyped, ?MODULE, undefined).

escape_metric_help_test() ->
  ?assertEqual(<<"qwe\\\\qwe\\nqwe">>,
               prometheus_text_format:escape_metric_help("qwe\\qwe\nqwe")).

escape_label_value_test()->
  ?assertEqual(<<"qwe\\\\qwe\\nq\\\"we\\\"qwe">>,
               prometheus_text_format:escape_label_value("qwe\\qwe\nq\"we\"qwe")).

escape_label_value_no_special_char_test() ->
    LabelBin = <<"qweqweqwe">>,
    ?assert(erts_debug:same(LabelBin, prometheus_text_format:escape_label_value(LabelBin))).

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_gauge/1,
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
    fun test_dhistogram/1]}.

content_type_test() ->
  ?assertEqual(<<"text/plain; version=0.0.4">>, prometheus_text_format:content_type()).

test_gauge(_) ->
  prometheus_gauge:new([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
  prometheus_gauge:set(pool_size, 365),
  ?_assertEqual(<<"# TYPE pool_size gauge
# HELP pool_size MongoDB Connections pool size
pool_size 365

">>, prometheus_text_format:format()).

test_untyped(_) ->
  prometheus_registry:register_collector(?MODULE),
  ?_assertEqual(<<"# TYPE pool_size untyped
# HELP pool_size MongoDB Connections pool size
pool_size 365

">>, prometheus_text_format:format()).

test_nan_gauge(_) ->
  prometheus_gauge:new([{name, pool_size}, {help, "MongoDB Connections pool size"}]),
  prometheus_gauge:set(pool_size, undefined),
  ?_assertEqual(<<"# TYPE pool_size gauge
# HELP pool_size MongoDB Connections pool size
pool_size NaN

">>, prometheus_text_format:format()).

test_counter(_) ->
  prometheus_counter:new([{name, http_requests_total}, {help, "Http request count"}]),
  prometheus_counter:inc(http_requests_total),
  ?_assertEqual(<<"# TYPE http_requests_total counter
# HELP http_requests_total Http request count
http_requests_total 1

">>, prometheus_text_format:format()).

test_dcounter(_) ->
  prometheus_counter:new([{name, dtest}, {help, "qw\"\\e"}]),
  prometheus_counter:inc(dtest, 1.5),
  prometheus_counter:inc(dtest, 3.5),
  prometheus_counter:inc(dtest, 1.5),

  ?_assertEqual(<<"# TYPE dtest counter
# HELP dtest qw\"\\\\e
dtest 6.5

">>, prometheus_text_format:format()).

test_summary(_) ->
  prometheus_summary:new([{name, orders_summary},
                          {help, "Track orders count/total sum"}]),
  prometheus_summary:observe(orders_summary,  10),
  prometheus_summary:observe(orders_summary,  15),
  ?_assertEqual(<<"# TYPE orders_summary summary
# HELP orders_summary Track orders count/total sum
orders_summary_count 2
orders_summary_sum 25

">>, prometheus_text_format:format()).

test_dsummary(_) ->
  prometheus_summary:new([{name, dsummary}, {labels, [host]}, {help, "qwe"}]),
  prometheus_summary:observe(dsummary, [123], 1.5),
  prometheus_summary:observe(dsummary, [123], 2.7),

  ?_assertEqual(<<"# TYPE dsummary summary
# HELP dsummary qwe
dsummary_count{host=\"123\"} 2
dsummary_sum{host=\"123\"} 4.2

">>, prometheus_text_format:format()).

test_quantile_summary(_) ->
  prometheus_quantile_summary:new([{name, orders_quantile_summary},
                          {help, "Track orders count/total sum"}]),
  prometheus_quantile_summary:observe(orders_quantile_summary,  10),
  prometheus_quantile_summary:observe(orders_quantile_summary,  15),
  ?_assertEqual(<<"# TYPE orders_quantile_summary summary
# HELP orders_quantile_summary Track orders count/total sum
orders_quantile_summary_count 2
orders_quantile_summary_sum 25
orders_quantile_summary{quantile=\"0.5\"} 15
orders_quantile_summary{quantile=\"0.9\"} 15
orders_quantile_summary{quantile=\"0.95\"} 15

">>, prometheus_text_format:format()).

test_quantile_dsummary(_) ->
  prometheus_quantile_summary:new([{name, quantile_dsummary}, {labels, [host]}, {help, "qwe"}]),
  prometheus_quantile_summary:observe(quantile_dsummary, [123], 1.5),
  prometheus_quantile_summary:observe(quantile_dsummary, [123], 2.7),

  ?_assertEqual(<<"# TYPE quantile_dsummary summary
# HELP quantile_dsummary qwe
quantile_dsummary_count{host=\"123\"} 2
quantile_dsummary_sum{host=\"123\"} 4.2
quantile_dsummary{host=\"123\",quantile=\"0.5\"} 2.7
quantile_dsummary{host=\"123\",quantile=\"0.9\"} 2.7
quantile_dsummary{host=\"123\",quantile=\"0.95\"} 2.7

">>, prometheus_text_format:format()).

test_histogram(_) ->
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {buckets, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"},
                            {duration_unit, false}]),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 95),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 100),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 102),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 150),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 250),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 75),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 350),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 550),
  prometheus_histogram:observe(http_request_duration_milliseconds, [get], 950),
  ?_assertEqual(<<"# TYPE http_request_duration_milliseconds histogram
# HELP http_request_duration_milliseconds Http Request execution time
http_request_duration_milliseconds_bucket{method=\"get\",le=\"100\"} 3
http_request_duration_milliseconds_bucket{method=\"get\",le=\"300\"} 6
http_request_duration_milliseconds_bucket{method=\"get\",le=\"500\"} 7
http_request_duration_milliseconds_bucket{method=\"get\",le=\"750\"} 8
http_request_duration_milliseconds_bucket{method=\"get\",le=\"1000\"} 9
http_request_duration_milliseconds_bucket{method=\"get\",le=\"+Inf\"} 9
http_request_duration_milliseconds_count{method=\"get\"} 9
http_request_duration_milliseconds_sum{method=\"get\"} 2622

">>, prometheus_text_format:format()).

test_histogram_float(_) ->
  prometheus_histogram:new([{name, http_request_duration_seconds},
                            {labels, [method]},
                            {buckets, [0.01, 0.1, 0.5, 1, 3]},
                            {help, "Http Request execution time"},
                            {duration_unit, false}]),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 0.95),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 0.1),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 0.102),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 0.15),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 0.25),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 0.35),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 0.55),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 1.55),
  prometheus_histogram:observe(http_request_duration_seconds, [get], 2.05),
  ?_assertEqual(<<"# TYPE http_request_duration_seconds histogram
# HELP http_request_duration_seconds Http Request execution time
http_request_duration_seconds_bucket{method=\"get\",le=\"0.01\"} 0
http_request_duration_seconds_bucket{method=\"get\",le=\"0.1\"} 1
http_request_duration_seconds_bucket{method=\"get\",le=\"0.5\"} 5
http_request_duration_seconds_bucket{method=\"get\",le=\"1\"} 7
http_request_duration_seconds_bucket{method=\"get\",le=\"3\"} 9
http_request_duration_seconds_bucket{method=\"get\",le=\"+Inf\"} 9
http_request_duration_seconds_count{method=\"get\"} 9
http_request_duration_seconds_sum{method=\"get\"} 6.052

">>, prometheus_text_format:format()).

test_dhistogram(_) ->
  prometheus_histogram:new([{name, http_request_duration_milliseconds},
                            {labels, [method]},
                            {buckets, [100, 300, 500, 750, 1000]},
                            {help, "Http Request execution time"},
                            {duration_unit, false}]),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 500.2),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 150.4),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 450.5),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 850.3),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 750.9),
  prometheus_histogram:observe(http_request_duration_milliseconds, [post], 1650.23),

  ?_assertEqual(<<"# TYPE http_request_duration_milliseconds histogram
# HELP http_request_duration_milliseconds Http Request execution time
http_request_duration_milliseconds_bucket{method=\"post\",le=\"100\"} 0
http_request_duration_milliseconds_bucket{method=\"post\",le=\"300\"} 1
http_request_duration_milliseconds_bucket{method=\"post\",le=\"500\"} 2
http_request_duration_milliseconds_bucket{method=\"post\",le=\"750\"} 3
http_request_duration_milliseconds_bucket{method=\"post\",le=\"1000\"} 5
http_request_duration_milliseconds_bucket{method=\"post\",le=\"+Inf\"} 6
http_request_duration_milliseconds_count{method=\"post\"} 6
http_request_duration_milliseconds_sum{method=\"post\"} 4352.53

">>, prometheus_text_format:format()).
