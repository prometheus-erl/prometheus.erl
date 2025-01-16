-module(prometheus_test_instrumenter).
-compile({parse_transform, prometheus_pt}).
-moduledoc false.

-behaviour(prometheus_instrumenter).

-export([setup_instrumenter/0]).

-spec setup_instrumenter() -> ok.
setup_instrumenter() ->
    ets:new(prometheus_instrumenter_tests, [set, named_table, public]),
    ok.
