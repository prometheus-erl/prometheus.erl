-module(prometheus_collector).
-compile({parse_transform, prometheus_pt}).

-moduledoc """
A collector for a set of metrics.

Normal users should use `m:prometheus_gauge`, `m:prometheus_counter`,
`m:prometheus_summary` and `m:prometheus_histogram`.

Implementing `prometheus_collector` behaviour is for advanced uses such as proxying
metrics from another monitoring system. It is it the responsibility of the implementer
to ensure produced metrics are valid.

You will be working with Prometheus data model directly (see `m:prometheus_model_helpers`).

### Example (simplified `prometheus_vm_memory_collector`):

```erlang
-module(prometheus_vm_memory_collector).

-export([deregister_cleanup/1, collect_mf/2, collect_metrics/2]).

-behaviour(prometheus_collector).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
    Memory = erlang:memory(),
    Callback(create_gauge(erlang_vm_bytes_total,
                          \"The total amount of memory currently allocated. \"
                          \"This is the same as the sum of the memory size \"
                          \"for processes and system.\",
                          Memory)),
    ok.

collect_metrics(erlang_vm_bytes_total, Memory) ->
    prometheus_model_helpers:gauge_metrics(
      [
        {[{kind, system}], proplists:get_value(system,  Memory)},
        {[{kind, processes}], proplists:get_value(processes, Memory)}
      ]).

%%====================================================================
%% Private Parts
%%====================================================================

create_gauge(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).

```
""".

-export([enabled_collectors/0, collect_mf/3]).

-ifdef(TEST).
-export([collect_mf_to_list/1]).
-endif.

-export_type([collector/0, data/0, collect_mf_callback/0]).

-compile({no_auto_import, [register/2]}).

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

-include("prometheus_model.hrl").

-type collector() :: atom().
-type data() :: any().
-type collect_mf_callback() :: fun((prometheus_model:'MetricFamily'()) -> any()).

-doc "Should call `Callback` for each `MetricFamily` of this collector".
-callback collect_mf(Registry, Callback) -> ok when
    Registry :: prometheus_registry:registry(),
    Callback :: collect_mf_callback().

-doc """
Should return Metric list for each MetricFamily identified by `Name`.

`Data` is a term associated with MetricFamily by `c:collect_mf/2`.
""".
-callback collect_metrics(Name, Data) -> Metrics when
    Name :: prometheus_metric:name(),
    Data :: data(),
    Metrics :: prometheus_model:'Metric'() | [prometheus_model:'Metric'()].

-doc "Called when collector is deregistered. If collector is stateful you can put cleanup code here".
-callback deregister_cleanup(Registry) -> ok when
    Registry :: prometheus_registry:registry().

-optional_callbacks([collect_metrics/2]).

%%====================================================================
%% Public API
%%====================================================================

-doc false.
-spec enabled_collectors() -> [collector()].
enabled_collectors() ->
    lists:usort(
        case application:get_env(prometheus, collectors) of
            undefined -> all_known_collectors();
            {ok, Collectors} -> catch_default_collectors(Collectors)
        end
    ).

%% pre-rendering optimization for the text format violates type constraints, but we still want it. So here is the
%% separate function with relevant dialyzer check disabled.
-dialyzer({no_match, global_labels_callback_wrapper/2}).
-spec global_labels_callback_wrapper([], Callback) -> Callback when
    Callback :: collect_mf_callback().
global_labels_callback_wrapper(GlobalLabels0, Callback) ->
    GlobalLabels = prometheus_model_helpers:label_pairs(GlobalLabels0),
    GlobalLabelsPrerendered = prometheus_text_format:render_labels(GlobalLabels),
    fun(#'MetricFamily'{metric = Metrics0} = MF) ->
        Metrics =
            [
                case ML of
                    %% empty binary singals to us that an app knows what it's doing,
                    %% so we can optimize for the text format
                    <<>> ->
                        M#'Metric'{label = GlobalLabelsPrerendered};
                    <<_/binary>> when GlobalLabelsPrerendered =:= <<>> -> M;
                    <<_/binary>> ->
                        M#'Metric'{label = <<GlobalLabelsPrerendered/binary, ",", ML/binary>>};
                    _ ->
                        M#'Metric'{label = GlobalLabels ++ ML}
                end
             || M = #'Metric'{label = ML} <- Metrics0
            ],
        Callback(MF#'MetricFamily'{metric = Metrics})
    end.

-doc "Calls `Callback` for each MetricFamily of this collector.".
-spec collect_mf(Registry, Collector, Callback) -> ok when
    Registry :: prometheus_registry:registry(),
    Collector :: collector(),
    Callback :: collect_mf_callback().
collect_mf(Registry, Collector, Callback0) ->
    Callback =
        case application:get_env(prometheus, global_labels) of
            undefined -> Callback0;
            {ok, []} -> Callback0;
            {ok, GlobalLabels} -> global_labels_callback_wrapper(GlobalLabels, Callback0)
        end,
    ok = Collector:collect_mf(Registry, Callback).

-ifdef(TEST).
-doc false.
-spec collect_mf_to_list(collector()) -> [prometheus_model:'MetricFamily'()].
collect_mf_to_list(Collector) ->
    collect_mf_to_list(default, Collector).

-spec collect_mf_to_list(prometheus_registry:registry(), collector()) ->
    [prometheus_model:'MetricFamily'()].
collect_mf_to_list(Registry, Collector) ->
    try
        Callback = fun(MF) ->
            put(Collector, [MF | get_list(Collector)])
        end,
        prometheus_collector:collect_mf(Registry, Collector, Callback),

        get_list(Collector)
    after
        erase(Collector)
    end.

get_list(Key) ->
    case get(Key) of
        undefined ->
            [];
        Value ->
            Value
    end.
-endif.

all_known_collectors() ->
    lists:umerge(
        prometheus_misc:behaviour_modules(prometheus_collector),
        ?DEFAULT_COLLECTORS
    ).

catch_default_collectors(Collectors) ->
    maybe_replace_default(Collectors, []).

maybe_replace_default([default | Rest], Acc) ->
    maybe_replace_default(Rest, ?DEFAULT_COLLECTORS ++ Acc);
maybe_replace_default([], Acc) ->
    Acc;
maybe_replace_default([H | R], Acc) ->
    maybe_replace_default(R, [H | Acc]).
