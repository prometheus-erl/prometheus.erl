-module(prometheus_test_instrumenter).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-behaviour(prometheus_instrumenter).

-export([setup_instrumenter/0]).

-spec setup_instrumenter() -> ok.
setup_instrumenter() ->
    ets:new(prometheus_instrumenter_tests, [set, named_table, public]),
    ok.
