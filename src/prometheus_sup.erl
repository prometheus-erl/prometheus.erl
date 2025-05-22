-module(prometheus_sup).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-export([start_link/0]).
-export([init/1]).
-export([register_metrics/1]).

-behaviour(supervisor).

-include("prometheus.hrl").

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    create_tables(),
    register_collectors(),
    register_metrics(),
    setup_instrumenters(),
    {ok, {{one_for_one, 5, 1}, []}}.

%%====================================================================
%% Private Parts
%%====================================================================

create_tables() ->
    Tables = [
        {?PROMETHEUS_REGISTRY_TABLE, [bag, {read_concurrency, true}]},
        {?PROMETHEUS_COUNTER_TABLE, [{write_concurrency, true}]},
        {?PROMETHEUS_GAUGE_TABLE, [{write_concurrency, true}]},
        {?PROMETHEUS_SUMMARY_TABLE, [{write_concurrency, true}]},
        {?PROMETHEUS_QUANTILE_SUMMARY_TABLE, [{read_concurrency, true}, {write_concurrency, true}]},
        {?PROMETHEUS_HISTOGRAM_TABLE, [{read_concurrency, true}, {write_concurrency, true}]},
        {?PROMETHEUS_BOOLEAN_TABLE, [{write_concurrency, true}]}
    ],
    [maybe_create_table(Name, Options) || {Name, Options} <- Tables],
    ok.

register_collectors() ->
    Collectors = prometheus_collector:enabled_collectors(),
    prometheus_registry:register_collectors(Collectors).

register_metrics() ->
    [declare_metric(Decl) || Decl <- default_metrics()].

-spec register_metrics([term()]) -> [boolean()].
register_metrics(Metrics) ->
    DefaultMetrics0 = default_metrics(),
    DefaultMetrics1 = lists:usort(DefaultMetrics0 ++ Metrics),
    application:set_env(prometheus, default_metrics, DefaultMetrics1),
    [declare_metric(Decl) || Decl <- Metrics].

setup_instrumenters() ->
    [
        prometheus_instrumenter:setup(Instrumenter)
     || Instrumenter <- prometheus_instrumenter:enabled_instrumenters()
    ].

default_metrics() ->
    application:get_env(prometheus, default_metrics, []).

maybe_create_table(Name, Options) ->
    case ets:info(Name) of
        undefined ->
            ets:new(Name, [named_table, public | Options]);
        _ ->
            ok
    end.

declare_metric({Metric, Spec}) ->
    declare_metric(Metric, Spec);
declare_metric({Registry, Metric, Spec}) ->
    declare_metric(Metric, prometheus_metric_spec:add_value(registry, Registry, Spec)).

declare_metric(counter, Spec) ->
    prometheus_counter:declare(Spec);
declare_metric(gauge, Spec) ->
    prometheus_gauge:declare(Spec);
declare_metric(summary, Spec) ->
    prometheus_summary:declare(Spec);
declare_metric(histogram, Spec) ->
    prometheus_histogram:declare(Spec);
declare_metric(boolean, Spec) ->
    prometheus_boolean:declare(Spec);
declare_metric(Other, Spec) ->
    Other:declare(Spec).
