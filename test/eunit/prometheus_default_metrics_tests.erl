-module(prometheus_default_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

default_metric_test() ->
    try
        Name = metric_name,
        Spec = [
            {name, Name},
            {help, ""},
            {registry, qwe},
            {buckets, [1, 2, 3]}
        ],
        Spec1 = [
            {name, Name},
            {help, ""},
            {buckets, [1, 2, 3]}
        ],

        application:stop(prometheus),
        application:set_env(
            prometheus,
            default_metrics,
            [
                {counter, Spec},
                {gauge, Spec},
                {qwe, summary, Spec1},
                {prometheus_histogram, Spec},
                {boolean, Spec}
            ]
        ),
        application:start(prometheus),
        ?assertEqual(false, prometheus_counter:declare(Spec)),
        ?assertEqual(false, prometheus_gauge:declare(Spec)),
        ?assertEqual(false, prometheus_summary:declare([{registry, qwe}] ++ Spec)),
        ?assertEqual(false, prometheus_histogram:declare(Spec)),
        ?assertEqual(false, prometheus_boolean:declare(Spec))
    after
        application:unset_env(prometheus, default_metrics)
    end.
