-module(prometheus_collector_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_COLLECTORS, [
    prometheus_boolean,
    prometheus_counter,
    prometheus_gauge,
    prometheus_histogram,
    prometheus_mnesia_collector,
    prometheus_quantile_summary,
    prometheus_summary,
    prometheus_vm_dist_collector,
    prometheus_vm_memory_collector,
    prometheus_vm_msacc_collector,
    prometheus_vm_statistics_collector,
    prometheus_vm_system_info_collector
]).

collector_setup_test() ->
    prometheus:start(),
    application:set_env(prometheus, collectors, [qwe]),
    try
        ?assertMatch([qwe], prometheus_collector:enabled_collectors())
    after
        application:unset_env(prometheus, collectors)
    end,
    application:set_env(prometheus, collectors, [qwe, default]),
    try
        C1 = ?DEFAULT_COLLECTORS ++ [qwe],
        ?assertMatch(C1, prometheus_collector:enabled_collectors())
    after
        application:unset_env(prometheus, collectors)
    end,
    ?assertMatch(?DEFAULT_COLLECTORS, prometheus_collector:enabled_collectors()).
