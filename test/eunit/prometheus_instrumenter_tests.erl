-module(prometheus_instrumenter_tests).

-include_lib("eunit/include/eunit.hrl").

instrumenter_setup_test() ->
    prometheus:start(),
    ?assertMatch(undefined, ets:info(prometheus_instrumenter_tests)),
    try
        application:set_env(prometheus, instrumenters, [qwe]),
        ?assertMatch([qwe], prometheus_instrumenter:enabled_instrumenters())
    after
        application:unset_env(prometheus, instrumenters)
    end,
    try
        application:set_env(prometheus, instrumenters, all_loaded),
        Expected = [prometheus_test_instrumenter],
        ?assertMatch(Expected, prometheus_instrumenter:enabled_instrumenters())
    after
        application:unset_env(prometheus, instrumenters)
    end,
    ?assertMatch([], prometheus_instrumenter:enabled_instrumenters()).
