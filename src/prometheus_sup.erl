-module(prometheus_sup).
-compile({parse_transform, prometheus_pt}).
-moduledoc false.

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
        {?PROMETHEUS_REGISTRY_TABLE, {bag, read_concurrency}},
        {?PROMETHEUS_COUNTER_TABLE, write_concurrency},
        {?PROMETHEUS_GAUGE_TABLE, write_concurrency},
        {?PROMETHEUS_SUMMARY_TABLE, write_concurrency},
        {?PROMETHEUS_QUANTILE_SUMMARY_TABLE, write_concurrency},
        {?PROMETHEUS_HISTOGRAM_TABLE, write_concurrency},
        {?PROMETHEUS_BOOLEAN_TABLE, write_concurrency}
    ],
    [maybe_create_table(Name, Concurrency) || {Name, Concurrency} <- Tables],
    ok.

register_collectors() ->
    Collectors = prometheus_collector:enabled_collectors(),
    prometheus_registry:register_collectors(Collectors).

register_metrics() ->
    [declare_metric(Decl) || Decl <- default_metrics()].

-spec register_metrics([term()]) -> ok.
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

maybe_create_table(Name, {Type, Concurrency}) ->
    case ets:info(Name) of
        undefined ->
            ets:new(Name, [Type, named_table, public, {Concurrency, true}]);
        _ ->
            ok
    end;
maybe_create_table(Name, Concurrency) ->
    maybe_create_table(Name, {set, Concurrency}).

declare_metric({Metric, Spec}) ->
    declare_metric(Metric, Spec);
declare_metric({Registry, Metric, Spec}) ->
    declare_metric(Metric, [{registry, Registry}] ++ Spec).

declare_metric(Metric, Spec) ->
    Module = type_to_module(Metric),
    Module:declare(Spec).

type_to_module(counter) ->
    prometheus_counter;
type_to_module(gauge) ->
    prometheus_gauge;
type_to_module(summary) ->
    prometheus_summary;
type_to_module(histogram) ->
    prometheus_histogram;
type_to_module(boolean) ->
    prometheus_boolean;
type_to_module(Type) ->
    Type.
