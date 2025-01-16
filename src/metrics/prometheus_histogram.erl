-module(prometheus_histogram).
-compile({parse_transform, prometheus_pt}).
-moduledoc """
A Histogram tracks the size and number of events in buckets.
You can use Histograms for aggregatable calculation of quantiles.

Example use cases for Histograms:

* Response latency
* Request size

Histogram expects `buckets` key in a metric spec. Buckets can be:

* a list of numbers in increasing order;
* `default`;
* `{linear, Start, Step, Count`;
* `{exponential, Start, Step, Count}`

Example:

```erlang
-module(example_instrumenter).

setup() ->
    prometheus_histogram:declare([{name, http_request_duration_milliseconds},
                                  {labels, [method]},
                                  {buckets, [100, 300, 500, 750, 1000]},
                                  {help, \"Http Request execution time.\"}]).

instrument(Time, Method) ->
    %% Time must be in native units, otherwise duration_unit must be false
    prometheus_histogram:observe(http_request_duration_milliseconds, [Method], Time).
```

The `prometheus_histogram:observe_n/3,4,5` adds limited support for the \"weighted\" histograms.
It accepts the extra integer argument \"Count\" to update the number of observations in the bucket
by adding that number. This allows for better accuracy in the case of irregular measurements,
assuming that the \"Count\" conveys the observation time interval (for example,
the number of time ticks when the recent value was observed).
""".

%%% metric
-export([
    new/1,
    declare/1,
    deregister/1,
    deregister/2,
    set_default/2,
    observe/2,
    observe/3,
    observe/4,
    observe_n/3,
    observe_n/4,
    observe_n/5,
    pobserve/6,
    observe_duration/2,
    observe_duration/3,
    observe_duration/4,
    remove/1,
    remove/2,
    remove/3,
    reset/1,
    reset/2,
    reset/3,
    value/1,
    value/2,
    value/3,
    buckets/1,
    buckets/2,
    buckets/3,
    values/2
]).

%%% collector
-export([
    deregister_cleanup/1,
    collect_mf/2,
    collect_metrics/2
]).

-include("prometheus.hrl").

-behaviour(prometheus_metric).
-behaviour(prometheus_collector).

-define(TABLE, ?PROMETHEUS_HISTOGRAM_TABLE).
-define(BOUNDS_POS, 2).
-define(ISUM_POS, 3).
-define(FSUM_POS, 4).
-define(BUCKETS_START, 5).
-define(WIDTH, 16).

%% ets row layout
%% {Key, NBounds, Sum, Bucket1, Bucket2, ...}
%% NBounds is a list of bounds possibly converted to native units

-doc """
Creates a histogram using `Spec`.

Raises:

* `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key is missing.
* `{invalid_metric_name, Name, Message}` error if metric `Name` is invalid.
* `{invalid_metric_help, Help, Message}` error if metric `Help` is invalid.
* `{invalid_metric_labels, Labels, Message}` error if `Labels` isn't a list.
* `{invalid_label_name, Name, Message}` error if `Name` isn't a valid label name.
* `{invalid_value_error, Value, Message}` error if `duration_unit` is unknown or doesn't match metric name.
* `{mf_already_exists, {Registry, Name}, Message}` error if a histogram with the same `Spec` already exists.

Histogram-specific errors:

Raises:

* `{no_buckets, Buckets}` error if `Buckets` are missing, not a list, empty list or not known buckets spec.
* `{invalid_buckets, Buckets, Message}` error if `Buckets` aren't in increasing order.
* `{invalid_bound, Bound}` error if `Bound` isn't a number.
""".
-spec new(prometheus_metric:spec()) -> ok.
new(Spec) ->
    Spec1 = validate_histogram_spec(Spec),
    prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec1).

-doc """
Creates a histogram using `Spec`. If a histogram with the same `Spec` exists returns `false`.

Raises:

* `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key is missing.
* `{invalid_metric_name, Name, Message}` error if metric `Name` is invalid.
* `{invalid_metric_help, Help, Message}` error if metric `Help` is invalid.
* `{invalid_metric_labels, Labels, Message}` error if `Labels` isn't a list.
* `{invalid_label_name, Name, Message}` error if `Name` isn't a valid label name.
* `{invalid_value_error, Value, MessagE}` error if `duration_unit` is unknown or doesn't match metric name.

Histogram-specific errors:

Raises:

* `{no_buckets, Buckets}` error if `Buckets` are missing, not a list, empty list or not known buckets spec.
* `{invalid_buckets, Buckets, Message}` error if `Buckets` aren't in increasing order.
* `{invalid_bound, Bound}` error if `Bound` isn't a number.
""".
-spec declare(prometheus_metric:spec()) -> boolean().
declare(Spec) ->
    Spec1 = validate_histogram_spec(Spec),
    prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec1).

-doc #{equiv => deregister(default, Name)}.
-spec deregister(prometheus_metric:name()) -> {boolean(), boolean()}.
deregister(Name) ->
    deregister(default, Name).

-doc """
Removes all histogram series with name `Name` and removes Metric Family from `Registry`.

After this call new/1 for `Name` and `Registry` will succeed.

Returns `{true, _}` if `Name` was a registered histogram. Otherwise returns `{false, _}`.
""".
-spec deregister(prometheus_registry:registry(), prometheus_metric:name()) ->
    {boolean(), boolean()}.
deregister(Registry, Name) ->
    try
        MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name),
        Buckets = prometheus_metric:mf_data(MF),
        prometheus_metric:deregister_mf(?TABLE, Registry, Name),
        Select = deregister_select(Registry, Name, Buckets),
        NumDeleted = ets:select_delete(?TABLE, Select),
        {true, NumDeleted > 0}
    catch
        _:_ -> {false, false}
    end.

-doc false.
-spec set_default(prometheus_registry:registry(), prometheus_metric:name()) -> boolean().
set_default(Registry, Name) ->
    insert_placeholders(Registry, Name, []).

-doc #{equiv => observe(default, Name, [], Value)}.
-spec observe(prometheus_metric:name(), number()) -> ok.
observe(Name, Value) ->
    observe(default, Name, [], Value).

-doc #{equiv => observe(default, Name, LabelValues, Value)}.
-spec observe(prometheus_metric:name(), list(), number()) -> ok.
observe(Name, LabelValues, Value) ->
    observe(default, Name, LabelValues, Value).

-doc """
Observes the given `Value`.

Raises:

* `{invalid_value, Value, Message}` if `Value` isn't a number.
* `{unknown_metric, Registry, Name}` error if histogram with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""".
-spec observe(prometheus_registry:registry(), prometheus_metric:name(), list(), number()) -> ok.
observe(Registry, Name, LabelValues, Value) when is_number(Value) ->
    observe_n(Registry, Name, LabelValues, Value, 1);
observe(_Registry, _Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "observe accepts only numbers"}).

-doc #{equiv => observe_n(default, Name, [], Value, Count)}.
-spec observe_n(prometheus_metric:name(), number(), integer()) -> ok.
observe_n(Name, Value, Count) ->
    observe_n(default, Name, [], Value, Count).

-doc #{equiv => observe_n(default, Name, LabelValues, Value, Count)}.
-spec observe_n(prometheus_metric:name(), list(), number(), integer()) -> ok.
observe_n(Name, LabelValues, Value, Count) ->
    observe_n(default, Name, LabelValues, Value, Count).

-doc """
Observes the given `Value`, `Count` times.

Raises:

* `{invalid_value, Value, Message}` if `Value` isn't a number.
* `{invalid_count, Count, Message}` if `Count` isn't integer.
* `{unknown_metric, Registry, Name}` error if histogram with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""".
-spec observe_n(
    prometheus_registry:registry(),
    prometheus_metric:name(),
    list(),
    number(),
    integer()
) -> ok.
observe_n(Registry, Name, LabelValues, Value, Count) when is_integer(Value), is_integer(Count) ->
    Key = key(Registry, Name, LabelValues),
    case ets:lookup(?TABLE, Key) of
        [Metric] ->
            BucketPosition = calculate_histogram_bucket_position(Metric, Value),
            ets:update_counter(
                ?TABLE,
                Key,
                [
                    {?ISUM_POS, Value * Count},
                    {?BUCKETS_START + BucketPosition, Count}
                ]
            );
        [] ->
            insert_metric(Registry, Name, LabelValues, Value, Count, fun observe_n/5)
    end,
    ok;
observe_n(Registry, Name, LabelValues, Value, Count) when is_number(Value), is_integer(Count) ->
    Key = key(Registry, Name, LabelValues),
    case ets:lookup(?TABLE, Key) of
        [Metric] ->
            fobserve_impl(Key, Metric, Value, Count);
        [] ->
            insert_metric(Registry, Name, LabelValues, Value, Count, fun observe_n/5)
    end;
observe_n(_Registry, _Name, _LabelValues, Value, Count) when is_number(Value) ->
    erlang:error({invalid_count, Count, "observe_n accepts only integer counts"});
observe_n(_Registry, _Name, _LabelValues, Value, _Count) ->
    erlang:error({invalid_value, Value, "observe_n accepts only number values"}).

-doc false.
-spec pobserve(
    prometheus_registry:registry(),
    prometheus_metric:name(),
    list(),
    list(),
    integer(),
    number()
) -> ok.
pobserve(Registry, Name, LabelValues, Buckets, BucketPos, Value) when is_integer(Value) ->
    Key = key(Registry, Name, LabelValues),
    try
        ets:update_counter(
            ?TABLE,
            Key,
            [{?ISUM_POS, Value}, {?BUCKETS_START + BucketPos, 1}]
        )
    catch
        error:badarg ->
            insert_metric(
                Registry,
                Name,
                LabelValues,
                Value,
                fun(_, _, _, _) ->
                    pobserve(
                        Registry,
                        Name,
                        LabelValues,
                        Buckets,
                        BucketPos,
                        Value
                    )
                end
            )
    end,
    ok;
pobserve(Registry, Name, LabelValues, Buckets, BucketPos, Value) when is_number(Value) ->
    Key = key(Registry, Name, LabelValues),
    case fobserve_impl(Key, Buckets, BucketPos, Value, 1) of
        0 ->
            insert_metric(
                Registry,
                Name,
                LabelValues,
                Value,
                fun(_, _, _, _) ->
                    fobserve_impl(Key, Buckets, BucketPos, Value, 1)
                end
            );
        1 ->
            ok
    end;
pobserve(_Registry, _Name, _LabelValues, _Buckets, _Pos, Value) ->
    erlang:error({invalid_value, Value, "pobserve accepts only numbers"}).

-doc #{equiv => observe_duration(default, Name, [], Fun)}.
-spec observe_duration(prometheus_metric:name(), fun(() -> term())) -> term().
observe_duration(Name, Fun) ->
    observe_duration(default, Name, [], Fun).

-doc #{equiv => observe_duration(default, Name, LabelValues, Fun)}.
-spec observe_duration(prometheus_metric:name(), list(), fun(() -> term())) -> term().
observe_duration(Name, LabelValues, Fun) ->
    observe_duration(default, Name, LabelValues, Fun).

-doc """
Tracks the amount of time spent executing `Fun`.

Raises:
* `{unknown_metric, Registry, Name}` error if histogram with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
* `{invalid_value, Value, Message}` if `Fun` isn't a function.
""".
-spec observe_duration(
    prometheus_registry:registry(), prometheus_metric:name(), list(), fun(() -> term())
) ->
    term().
observe_duration(Registry, Name, LabelValues, Fun) when is_function(Fun, 0) ->
    Start = erlang:monotonic_time(),
    try
        Fun()
    after
        observe(Registry, Name, LabelValues, erlang:monotonic_time() - Start)
    end;
observe_duration(_Regsitry, _Name, _LabelValues, Fun) ->
    erlang:error({invalid_value, Fun, "observe_duration accepts only functions"}).

-doc #{equiv => remove(default, Name, [])}.
-spec remove(prometheus_metric:name()) -> boolean().
remove(Name) ->
    remove(default, Name, []).

-doc #{equiv => remove(default, Name, LabelValues)}.
-spec remove(prometheus_metric:name(), list()) -> boolean().
remove(Name, LabelValues) ->
    remove(default, Name, LabelValues).

-doc """
Removes histogram series identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if histogram with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""".
-spec remove(prometheus_registry:registry(), prometheus_metric:name(), list()) -> boolean().
remove(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    case
        lists:flatten([
            ets:take(
                ?TABLE,
                {Registry, Name, LabelValues, Scheduler}
            )
         || Scheduler <- schedulers_seq()
        ])
    of
        [] -> false;
        _ -> true
    end.

-doc #{equiv => reset(default, Name, [])}.
-spec reset(prometheus_metric:name()) -> boolean().
reset(Name) ->
    reset(default, Name, []).

-doc #{equiv => reset(default, Name, LabelValues)}.
-spec reset(prometheus_metric:name(), list()) -> boolean().
reset(Name, LabelValues) ->
    reset(default, Name, LabelValues).

-doc """
Resets the value of the histogram identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if histogram with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""".
-spec reset(prometheus_registry:registry(), prometheus_metric:name(), list()) -> boolean().
reset(Registry, Name, LabelValues) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    Buckets = prometheus_metric:mf_data(MF),
    UpdateSpec = generate_update_spec(Buckets),

    case
        lists:usort([
            ets:update_element(
                ?TABLE,
                {Registry, Name, LabelValues, Scheduler},
                [{?ISUM_POS, 0}, {?FSUM_POS, 0}] ++ UpdateSpec
            )
         || Scheduler <- schedulers_seq()
        ])
    of
        [_, _] -> true;
        [true] -> true;
        _ -> false
    end.

-doc #{equiv => value(default, Name, [])}.
-spec value(prometheus_metric:name()) ->
    {number(), infinity | number()} | undefined.
value(Name) ->
    value(default, Name, []).

-doc #{equiv => value(default, Name, LabelValues)}.
-spec value(prometheus_metric:name(), list()) ->
    {number(), infinity | number()} | undefined.
value(Name, LabelValues) ->
    value(default, Name, LabelValues).

-doc """
Returns the value of the histogram identified by `Registry`, `Name` and `LabelValues`.
If there is no histogram for `LabelValues`, returns `undefined`.

If duration unit set, sum will be converted to the duration unit.
[Read more here](`m:prometheus_time`).

Raises:

* `{unknown_metric, Registry, Name}` error if histogram named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""".
-spec value(prometheus_registry:registry(), prometheus_metric:name(), list()) ->
    {number(), infinity | number()} | undefined.
value(Registry, Name, LabelValues) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),

    RawValues = [
        ets:lookup(?TABLE, {Registry, Name, LabelValues, Scheduler})
     || Scheduler <- schedulers_seq()
    ],
    case lists:flatten(RawValues) of
        [] -> undefined;
        Values -> {reduce_buckets_counters(Values), reduce_sum(MF, Values)}
    end.

-spec values(prometheus_registry:registry(), prometheus_metric:name()) -> [{list(), number()}].
values(Registry, Name) ->
    case prometheus_metric:check_mf_exists(?TABLE, Registry, Name) of
        false -> [];
        MF -> mf_values(Registry, Name, MF)
    end.

-doc #{equiv => buckets(default, Name, [])}.
-spec buckets(prometheus_metric:name()) -> [number()].
buckets(Name) ->
    buckets(default, Name, []).

-doc #{equiv => buckets(default, Name, LabelValues)}.
-spec buckets(prometheus_metric:name(), list()) -> [number()].
buckets(Name, LabelValues) ->
    buckets(default, Name, LabelValues).

-doc "Returns buckets of the histogram identified by `Registry`, `Name` and `LabelValues`.".
-spec buckets(
    prometheus_registry:registry(),
    prometheus_metric:name(),
    list()
) -> [number()].
buckets(Registry, Name, LabelValues) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    prometheus_metric:mf_data(MF).

%%====================================================================
%% Collector API
%%====================================================================

-doc false.
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(Registry) ->
    [
        delete_metrics(Registry, Buckets)
     || [_, _, _, _, Buckets] <- prometheus_metric:metrics(?TABLE, Registry)
    ],
    true = prometheus_metric:deregister_mf(?TABLE, Registry),
    ok.

-doc false.
-spec collect_mf(prometheus_registry:registry(), prometheus_collector:collect_mf_callback()) -> ok.
collect_mf(Registry, Callback) ->
    [
        Callback(create_histogram(Name, Help, {CLabels, Labels, Registry, DU, Buckets}))
     || [Name, {Labels, Help}, CLabels, DU, Buckets] <-
            prometheus_metric:metrics(?TABLE, Registry)
    ],
    ok.

-doc false.
-spec collect_metrics(prometheus_metric:name(), tuple()) ->
    [prometheus_model:'Metric'()].
collect_metrics(Name, {CLabels, Labels, Registry, DU, Bounds}) ->
    MFValues = load_all_values(Registry, Name, Bounds),
    LabelValuesMap = reduce_label_values(MFValues),
    maps:fold(
        fun(LabelValues, Stat, L) ->
            [create_histogram_metric(CLabels, Labels, DU, Bounds, LabelValues, Stat) | L]
        end,
        [],
        LabelValuesMap
    ).

%%====================================================================
%% Private Parts
%%====================================================================

validate_histogram_spec(Spec) ->
    Labels = prometheus_metric_spec:labels(Spec),
    validate_histogram_labels(Labels),
    RBuckets = prometheus_metric_spec:get_value(buckets, Spec, default),
    Buckets = prometheus_buckets:new(RBuckets),
    [{data, Buckets} | Spec].

validate_histogram_labels(Labels) ->
    [raise_error_if_le_label_found(Label) || Label <- Labels].

raise_error_if_le_label_found("le") ->
    erlang:error({invalid_metric_label_name, "le", "histogram cannot have a label named \"le\""});
raise_error_if_le_label_found(Label) ->
    Label.

insert_metric(Registry, Name, LabelValues, Value, CB) ->
    insert_placeholders(Registry, Name, LabelValues),
    CB(Registry, Name, LabelValues, Value).

insert_metric(Registry, Name, LabelValues, Value, Count, CB) ->
    insert_placeholders(Registry, Name, LabelValues),
    CB(Registry, Name, LabelValues, Value, Count).

fobserve_impl(Key, Metric, Value, Count) ->
    Buckets = metric_buckets(Metric),
    BucketPos = calculate_histogram_bucket_position(Metric, Value),
    fobserve_impl(Key, Buckets, BucketPos, Value, Count).

fobserve_impl(Key, Buckets, BucketPos, Value, Count) ->
    ets:select_replace(?TABLE, generate_select_replace(Key, Buckets, BucketPos, Value, Count)).

insert_placeholders(Registry, Name, LabelValues) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    MFBuckets = prometheus_metric:mf_data(MF),
    DU = prometheus_metric:mf_duration_unit(MF),
    Fun = fun(Bucket) ->
        prometheus_time:maybe_convert_to_native(DU, Bucket)
    end,
    BoundCounters = lists:duplicate(length(MFBuckets), 0),
    MetricSpec =
        [key(Registry, Name, LabelValues), lists:map(Fun, MFBuckets), 0, 0] ++
            BoundCounters,
    ets:insert_new(?TABLE, list_to_tuple(MetricSpec)).

calculate_histogram_bucket_position(Metric, Value) ->
    Buckets = metric_buckets(Metric),
    prometheus_buckets:position(Buckets, Value).

generate_select_replace(Key, Bounds, BucketPos, Value, Count) ->
    BoundPlaceholders = gen_query_bound_placeholders(Bounds),
    HistMatch = list_to_tuple([Key, '$2', '$3', '$4'] ++ BoundPlaceholders),
    BucketUpdate =
        lists:sublist(BoundPlaceholders, BucketPos) ++
            [{'+', gen_query_placeholder(?BUCKETS_START + BucketPos), Count}] ++
            lists:nthtail(BucketPos + 1, BoundPlaceholders),
    HistUpdate = list_to_tuple([{Key}, '$2', '$3', {'+', '$4', Value * Count}] ++ BucketUpdate),
    [{HistMatch, [], [{HistUpdate}]}].

buckets_seq(Buckets) ->
    lists:seq(?BUCKETS_START, ?BUCKETS_START + length(Buckets) - 1).

generate_update_spec(Buckets) ->
    [{Index, 0} || Index <- buckets_seq(Buckets)].

gen_query_placeholder(Index) ->
    list_to_atom("$" ++ integer_to_list(Index)).

gen_query_bound_placeholders(Buckets) ->
    [gen_query_placeholder(Index) || Index <- buckets_seq(Buckets)].

augment_counters([Start | Counters]) ->
    augment_counters(Counters, [Start], Start).

augment_counters([], LAcc, _CAcc) ->
    LAcc;
augment_counters([Counter | Counters], LAcc, CAcc) ->
    augment_counters(Counters, LAcc ++ [CAcc + Counter], CAcc + Counter).

metric_buckets(Metric) ->
    element(?BOUNDS_POS, Metric).

reduce_buckets_counters(Metrics) ->
    ABuckets =
        [
            sub_tuple_to_list(
                Metric,
                ?BUCKETS_START,
                ?BUCKETS_START + length(metric_buckets(Metric))
            )
         || Metric <- Metrics
        ],
    [lists:sum(Bucket) || Bucket <- transpose(ABuckets)].

transpose([[] | _]) -> [];
transpose(M) -> [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

reduce_sum(Metrics) ->
    lists:sum([
        element(?ISUM_POS, Metric) + element(?FSUM_POS, Metric)
     || Metric <- Metrics
    ]).

reduce_sum(MF, Metrics) ->
    DU = prometheus_metric:mf_duration_unit(MF),
    prometheus_time:maybe_convert_to_du(DU, reduce_sum(Metrics)).

create_histogram_metric(CLabels, Labels, DU, Bounds, LabelValues, [ISum, FSum | Buckets]) ->
    BCounters = augment_counters(Buckets),
    Bounds1 = lists:zipwith(
        fun(Bound, Bucket) ->
            {Bound, Bucket}
        end,
        Bounds,
        BCounters
    ),

    prometheus_model_helpers:histogram_metric(
        CLabels ++ lists:zip(Labels, LabelValues),
        Bounds1,
        lists:last(BCounters),
        prometheus_time:maybe_convert_to_du(DU, ISum + FSum)
    ).

load_all_values(Registry, Name, Bounds) ->
    BoundPlaceholders = gen_query_bound_placeholders(Bounds),
    QuerySpec = [{Registry, Name, '$1', '_'}, '_', '$3', '$4'] ++ BoundPlaceholders,

    ets:match(?TABLE, list_to_tuple(QuerySpec)).

deregister_select(Registry, Name, Buckets) ->
    BoundCounters = lists:duplicate(length(Buckets), '_'),
    MetricSpec = [{Registry, Name, '_', '_'}, '_', '_', '_'] ++ BoundCounters,
    [{list_to_tuple(MetricSpec), [], [true]}].

delete_metrics(Registry, Buckets) ->
    BoundCounters = lists:duplicate(length(Buckets), '_'),
    MetricSpec = [{Registry, '_', '_', '_'}, '_', '_', '_'] ++ BoundCounters,
    ets:match_delete(?TABLE, list_to_tuple(MetricSpec)).

sub_tuple_to_list(Tuple, Pos, Size) when Pos < Size ->
    [element(Pos, Tuple) | sub_tuple_to_list(Tuple, Pos + 1, Size)];
sub_tuple_to_list(_Tuple, _Pos, _Size) ->
    [].

schedulers_seq() ->
    lists:seq(0, ?WIDTH - 1).

key(Registry, Name, LabelValues) ->
    X = erlang:system_info(scheduler_id),
    Rnd = X band (?WIDTH - 1),
    {Registry, Name, LabelValues, Rnd}.

reduce_label_values(MFValues) ->
    lists:foldl(
        fun
            ([Labels | V], ResAcc) when is_map_key(Labels, ResAcc) ->
                PrevSum = maps:get(Labels, ResAcc),
                ResAcc#{Labels => [lists:sum(C) || C <- transpose([PrevSum, V])]};
            ([Labels | V], ResAcc) ->
                ResAcc#{Labels => V}
        end,
        #{},
        MFValues
    ).

mf_values(Registry, Name, MF) ->
    DU = prometheus_metric:mf_duration_unit(MF),
    Labels = prometheus_metric:mf_labels(MF),
    Bounds = prometheus_metric:mf_data(MF),

    MFValues = load_all_values(Registry, Name, Bounds),
    LabelValuesMap = reduce_label_values(MFValues),
    maps:fold(
        fun(LabelValues, [ISum, FSum | BCounters], L) ->
            Bounds1 = lists:zipwith(
                fun(Bound, Bucket) ->
                    {Bound, Bucket}
                end,
                Bounds,
                BCounters
            ),
            [
                {
                    lists:zip(Labels, LabelValues),
                    Bounds1,
                    prometheus_time:maybe_convert_to_du(DU, ISum + FSum)
                }
                | L
            ]
        end,
        [],
        LabelValuesMap
    ).

create_histogram(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, histogram, ?MODULE, Data).
