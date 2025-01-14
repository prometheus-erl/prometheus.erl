-module(prometheus_registry_tests).

-export([deregister_cleanup/1]).

-include_lib("eunit/include/eunit.hrl").

prometheus_registry_test_() ->
    {setup, fun start/0, fun stop/1, fun(ok) ->
        {inorder, [
            default_registry(),
            test_registry(),
            test_registry_exists()
        ]}
    end}.

start() ->
    prometheus:start(),
    ok.

stop(ok) ->
    ok.

%% this should be moved to app-level tests [as well as default instrumenters]
%% maybe after migration to CT
default_registry() ->
    ?_assertEqual(
        [
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
        ],
        prometheus_registry:collectors(default)
    ).

test_registry() ->
    %% test whole register cleanup
    ok = prometheus_registry:register_collector(custom_registry, prometheus_registry_tests),
    Collectors = prometheus_registry:collectors(custom_registry),
    prometheus_registry:clear(custom_registry),

    %% remove just this one collector from custom_registry1
    ok = prometheus_registry:register_collector(custom_registry1, prometheus_registry_tests),
    Collectors1 = prometheus_registry:collectors(custom_registry1),
    prometheus_registry:deregister_collector(custom_registry1, prometheus_registry_tests),

    DefaultCollectors = prometheus_eunit_common:start(),

    {ADC, DCR, DCD, DCR1, DCD1, ADC1} =
        try
            {
                prometheus_registry:collectors(default),
                begin
                    prometheus_registry:register_collector(prometheus_registry_tests),
                    prometheus_registry:collector_registeredp(default, prometheus_registry_tests)
                end,
                begin
                    prometheus_registry:deregister_collector(prometheus_registry_tests),
                    prometheus_registry:collector_registeredp(prometheus_registry_tests)
                end,

                begin
                    prometheus_registry:register_collector(prometheus_registry_tests),
                    prometheus_registry:collector_registeredp(default, prometheus_registry_tests)
                end,
                begin
                    prometheus_registry:clear(),
                    prometheus_registry:collector_registeredp(prometheus_registry_tests)
                end,
                prometheus_registry:collectors(default)
            }
        after
            prometheus_eunit_common:stop(DefaultCollectors)
        end,
    [
        ?_assertEqual([prometheus_registry_tests], Collectors),
        ?_assertEqual([], prometheus_registry:collectors(custom_registry)),
        ?_assertEqual([prometheus_registry_tests], Collectors1),
        ?_assertEqual([], prometheus_registry:collectors(custom_registry1)),

        ?_assertEqual([], ADC),
        ?_assertEqual(true, DCR),
        ?_assertEqual(false, DCD),

        ?_assertEqual(true, DCR1),
        ?_assertEqual(false, DCD1),
        ?_assertEqual([], ADC1)
    ].

test_registry_exists() ->
    prometheus_registry:register_collector(test_registry, prometheus_registry_tests),

    [
        ?_assertMatch(true, prometheus_registry:exists(test_registry)),

        ?_assertMatch(test_registry, prometheus_registry:exists("test_registry")),
        ?_assertMatch(test_registry, prometheus_registry:exists(<<"test_registry">>)),

        ?_assertMatch(false, prometheus_registry:exists(qweqwa)),
        ?_assertMatch(false, prometheus_registry:exists("qweqwa"))
    ].

deregister_cleanup(_) -> ok.
