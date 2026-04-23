-module(prometheus_metric_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_metric_test_() ->
    {foreach, fun prometheus_eunit_common:start/0, fun prometheus_eunit_common:stop/1, [
        fun test_set_default_proxy_default_registry/1,
        fun test_set_default_proxy_explicit_registry/1,
        fun test_set_default_proxy_unknown_metric/1
    ]}.

test_set_default_proxy_default_registry(_) ->
    Name = metric_proxy_counter_default,
    ok = prometheus_counter:new([{name, Name}, {labels, [status]}, {help, ""}]),
    [
        ?_assertEqual(true, prometheus_metric:set_default(prometheus_counter, Name, ["200"])),
        ?_assertEqual(false, prometheus_metric:set_default(prometheus_counter, Name, ["200"])),
        ?_assertEqual(0, prometheus_counter:value(Name, ["200"]))
    ].

test_set_default_proxy_explicit_registry(_) ->
    Name = metric_proxy_counter_default_registry,
    Registry = default,
    ok = prometheus_counter:new([{registry, Registry}, {name, Name}, {labels, [status]}, {help, ""}]),
    [
        ?_assertEqual(true, prometheus_metric:set_default(prometheus_counter, Registry, Name, ["404"])),
        ?_assertEqual(0, prometheus_counter:value(Registry, Name, ["404"]))
    ].

test_set_default_proxy_unknown_metric(_) ->
    [
        ?_assertError(
            {unknown_metric, default, missing_metric_proxy_default},
            prometheus_metric:set_default(prometheus_counter, missing_metric_proxy_default, [])
        ),
        ?_assertError(
            {unknown_metric, default, missing_metric_proxy_custom},
            prometheus_metric:set_default(prometheus_counter, default, missing_metric_proxy_custom, [])
        )
    ].
