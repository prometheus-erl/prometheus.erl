-module(prometheus_histogram).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
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
""").

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
%% {Key, NBounds, ISum, FSum, Bucket1, Bucket2, ...}
%% NBounds is a list of bounds possibly converted to native units

?DOC("""
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
""").
-spec new(prometheus_metric:spec()) -> ok.
new(Spec) ->
    Spec1 = validate_histogram_spec(Spec),
    prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec1).

?DOC("""
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
""").
-spec declare(prometheus_metric:spec()) -> boolean().
declare(Spec) ->
    Spec1 = validate_histogram_spec(Spec),
    prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec1).

?DOC(#{equiv => deregister(default, Name)}).
-spec deregister(prometheus_metric:name()) -> {boolean(), boolean()}.
deregister(Name) ->
    deregister(default, Name).

?DOC("""
Removes all histogram series with name `Name` and removes Metric Family from `Registry`.

After this call new/1 for `Name` and `Registry` will succeed.

Returns `{true, _}` if `Name` was a registered histogram. Otherwise returns `{false, _}`.
""").
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

?DOC(false).
-spec set_default(prometheus_registry:registry(), prometheus_metric:name()) -> boolean().
set_default(Registry, Name) ->
    insert_placeholders(Registry, Name, []).

?DOC(#{equiv => observe(default, Name, [], Value)}).
-spec observe(prometheus_metric:name(), number()) -> ok.
observe(Name, Value) ->
    observe(default, Name, [], Value).

?DOC(#{equiv => observe(default, Name, LabelValues, Value)}).
-spec observe(prometheus_metric:name(), prometheus_metric:label_values(), number()) -> ok.
observe(Name, LabelValues, Value) ->
    observe(default, Name, LabelValues, Value).

?DOC("""
Observes the given `Value`.

Raises:

* `{invalid_value, Value, Message}` if `Value` isn't a number.
* `{unknown_metric, Registry, Name}` error if histogram with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec observe(Registry, Name, LabelValues, Value) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Value :: number().
observe(Registry, Name, LabelValues, Value) when is_number(Value) ->
    observe_n(Registry, Name, LabelValues, Value, 1);
observe(_Registry, _Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "observe accepts only numbers"}).

?DOC(#{equiv => observe_n(default, Name, [], Value, Count)}).
-spec observe_n(prometheus_metric:name(), number(), integer()) -> ok.
observe_n(Name, Value, Count) ->
    observe_n(default, Name, [], Value, Count).

?DOC(#{equiv => observe_n(default, Name, LabelValues, Value, Count)}).
-spec observe_n(prometheus_metric:name(), prometheus_metric:label_values(), number(), integer()) ->
    ok.
observe_n(Name, LabelValues, Value, Count) ->
    observe_n(default, Name, LabelValues, Value, Count).

?DOC("""
Observes the given `Value`, `Count` times.

Raises:

* `{invalid_value, Value, Message}` if `Value` isn't a number.
* `{invalid_count, Count, Message}` if `Count` isn't integer.
* `{unknown_metric, Registry, Name}` error if histogram with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec observe_n(Registry, Name, LabelValues, Value, Count) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Value :: number(),
    Count :: integer().
observe_n(Registry, Name, LabelValues, Value, Count) when is_integer(Value), is_integer(Count) ->
    Key = key(Registry, Name, LabelValues),
    case ets:lookup_element(?TABLE, Key, ?BOUNDS_POS, undefined) of
        undefined ->
            insert_metric(Registry, Name, LabelValues, Value, Count, fun observe_n/5);
        Buckets when is_tuple(Buckets) ->
            BucketPosition = prometheus_buckets:position(Buckets, Value),
            Spec = [{?ISUM_POS, Value * Count}, {?BUCKETS_START + BucketPosition, Count}],
            ets:update_counter(?TABLE, Key, Spec)
    end,
    ok;
observe_n(Registry, Name, LabelValues, Value, Count) when is_float(Value), is_integer(Count) ->
    Key = key(Registry, Name, LabelValues),
    case ets:lookup_element(?TABLE, Key, ?BOUNDS_POS, undefined) of
        undefined ->
            insert_metric(Registry, Name, LabelValues, Value, Count, fun observe_n/5);
        Buckets when is_tuple(Buckets) ->
            BucketPosition = prometheus_buckets:position(Buckets, Value),
            fobserve_impl(Key, Buckets, BucketPosition, Value, Count)
    end;
observe_n(_Registry, _Name, _LabelValues, Value, Count) when is_number(Value) ->
    erlang:error({invalid_count, Count, "observe_n accepts only integer counts"});
observe_n(_Registry, _Name, _LabelValues, Value, _Count) ->
    erlang:error({invalid_value, Value, "observe_n accepts only number values"}).

?DOC("""
Observes the given `Value`, `Count` times, in the bucket position of the bucket list.

Useful when the bucket position is known in advance, as it avoids having to compute such value
during the observation.
""").
-spec pobserve(Registry, Name, LabelValues, Buckets, BucketPos, Value) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Buckets :: prometheus_buckets:buckets(),
    BucketPos :: integer(),
    Value :: number().
pobserve(Registry, Name, LabelValues, Buckets, BucketPos, Value) when is_integer(Value) ->
    Key = key(Registry, Name, LabelValues),
    Spec = [{?ISUM_POS, Value}, {?BUCKETS_START + BucketPos, 1}],
    try
        ets:update_counter(?TABLE, Key, Spec)
    catch
        error:badarg ->
            Fun = fun(_, _, _, _) ->
                pobserve(Registry, Name, LabelValues, Buckets, BucketPos, Value)
            end,
            insert_metric(Registry, Name, LabelValues, Value, Fun)
    end,
    ok;
pobserve(Registry, Name, LabelValues, Buckets, BucketPos, Value) when is_float(Value) ->
    Key = key(Registry, Name, LabelValues),
    case fobserve_impl(Key, Buckets, BucketPos, Value, 1) of
        0 ->
            Fun = fun(_, _, _, _) -> fobserve_impl(Key, Buckets, BucketPos, Value, 1) end,
            insert_metric(Registry, Name, LabelValues, Value, Fun);
        1 ->
            ok
    end;
pobserve(_Registry, _Name, _LabelValues, _Buckets, _Pos, Value) ->
    erlang:error({invalid_value, Value, "pobserve accepts only numbers"}).

?DOC(#{equiv => observe_duration(default, Name, [], Fun)}).
-spec observe_duration(prometheus_metric:name(), fun(() -> dynamic())) -> dynamic().
observe_duration(Name, Fun) ->
    observe_duration(default, Name, [], Fun).

?DOC(#{equiv => observe_duration(default, Name, LabelValues, Fun)}).
-spec observe_duration(Name, LabelValues, Fun) -> dynamic() when
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Fun :: fun(() -> dynamic()).
observe_duration(Name, LabelValues, Fun) ->
    observe_duration(default, Name, LabelValues, Fun).

?DOC("""
Tracks the amount of time spent executing `Fun`.

Raises:
* `{unknown_metric, Registry, Name}` error if histogram with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
* `{invalid_value, Value, Message}` if `Fun` isn't a function.
""").
-spec observe_duration(Registry, Name, LabelValues, Fun) -> dynamic() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Fun :: fun(() -> dynamic()).
observe_duration(Registry, Name, LabelValues, Fun) when is_function(Fun, 0) ->
    Start = erlang:monotonic_time(),
    try
        Fun()
    after
        observe(Registry, Name, LabelValues, erlang:monotonic_time() - Start)
    end;
observe_duration(_Regsitry, _Name, _LabelValues, Fun) ->
    erlang:error({invalid_value, Fun, "observe_duration accepts only functions"}).

?DOC(#{equiv => remove(default, Name, [])}).
-spec remove(prometheus_metric:name()) -> boolean().
remove(Name) ->
    remove(default, Name, []).

?DOC(#{equiv => remove(default, Name, LabelValues)}).
-spec remove(prometheus_metric:name(), prometheus_metric:label_values()) -> boolean().
remove(Name, LabelValues) ->
    remove(default, Name, LabelValues).

?DOC("""
Removes histogram series identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if histogram with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec remove(Registry, Name, LabelValues) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
remove(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    List = lists:flatten([
        ets:take(?TABLE, {Registry, Name, LabelValues, Scheduler})
     || Scheduler <- schedulers_seq()
    ]),
    case List of
        [] -> false;
        _ -> true
    end.

?DOC(#{equiv => reset(default, Name, [])}).
-spec reset(prometheus_metric:name()) -> boolean().
reset(Name) ->
    reset(default, Name, []).

?DOC(#{equiv => reset(default, Name, LabelValues)}).
-spec reset(prometheus_metric:name(), prometheus_metric:label_values()) -> boolean().
reset(Name, LabelValues) ->
    reset(default, Name, LabelValues).

?DOC("""
Resets the value of the histogram identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if histogram with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec reset(Registry, Name, LabelValues) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
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

?DOC(#{equiv => value(default, Name, [])}).
-spec value(prometheus_metric:name()) ->
    {number(), infinity | number()} | undefined.
value(Name) ->
    value(default, Name, []).

?DOC(#{equiv => value(default, Name, LabelValues)}).
-spec value(prometheus_metric:name(), prometheus_metric:label_values()) ->
    {number(), infinity | number()} | undefined.
value(Name, LabelValues) ->
    value(default, Name, LabelValues).

?DOC("""
Returns the value of the histogram identified by `Registry`, `Name` and `LabelValues`.
If there is no histogram for `LabelValues`, returns `undefined`.

If duration unit set, sum will be converted to the duration unit.
[Read more here](`m:prometheus_time`).

Raises:

* `{unknown_metric, Registry, Name}` error if histogram named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec value(Registry, Name, LabelValues) -> {number(), infinity | number()} | undefined when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
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

?DOC(#{equiv => buckets(default, Name, [])}).
-spec buckets(prometheus_metric:name()) -> [number()].
buckets(Name) ->
    buckets(default, Name, []).

?DOC(#{equiv => buckets(default, Name, LabelValues)}).
-spec buckets(prometheus_metric:name(), prometheus_metric:label_values()) -> [number()].
buckets(Name, LabelValues) ->
    buckets(default, Name, LabelValues).

?DOC("Returns buckets of the histogram identified by `Registry`, `Name` and `LabelValues`.").
-spec buckets(Registry, Name, LabelValues) -> [number()] when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
buckets(Registry, Name, LabelValues) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    prometheus_metric:mf_data(MF).

%%====================================================================
%% Collector API
%%====================================================================

?DOC(false).
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(Registry) ->
    [
        delete_metrics(Registry, Buckets)
     || [_, _, _, _, Buckets] <- prometheus_metric:metrics(?TABLE, Registry)
    ],
    true = prometheus_metric:deregister_mf(?TABLE, Registry),
    ok.

?DOC(false).
-spec collect_mf(prometheus_registry:registry(), prometheus_collector:collect_mf_callback()) -> ok.
collect_mf(Registry, Callback) ->
    [
        Callback(create_histogram(Name, Help, {CLabels, Labels, Registry, DU, Buckets}))
     || [Name, {Labels, Help}, CLabels, DU, Buckets] <- prometheus_metric:metrics(?TABLE, Registry)
    ],
    ok.

?DOC(false).
-spec collect_metrics(prometheus_metric:name(), tuple()) ->
    [prometheus_model:'Metric'()].
collect_metrics(Name, {CLabels, Labels, Registry, DU, Bounds}) ->
    MFValues = load_all_values(Registry, Name, Bounds),
    LabelValuesMap = reduce_label_values(MFValues),
    Fun = fun(LabelValues, Stat, L) ->
        [create_histogram_metric(CLabels, Labels, DU, Bounds, LabelValues, Stat) | L]
    end,
    maps:fold(Fun, [], LabelValuesMap).

%%====================================================================
%% Private Parts
%%====================================================================

validate_histogram_spec(Spec) ->
    Labels = prometheus_metric_spec:labels(Spec),
    validate_histogram_labels(Labels),
    RBuckets = prometheus_metric_spec:get_value(buckets, Spec, default),
    Buckets = prometheus_buckets:new(RBuckets),
    prometheus_metric_spec:add_value(data, Buckets, Spec).

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
    Buckets = list_to_tuple(lists:map(Fun, MFBuckets)),
    MetricSpec =
        [key(Registry, Name, LabelValues), Buckets, 0, 0 | BoundCounters],
    ets:insert_new(?TABLE, list_to_tuple(MetricSpec)).

generate_select_replace(Key, Bounds, BucketPos, Value, Count) ->
    BoundPlaceholders = gen_query_bound_placeholders(Bounds),
    HistMatch = list_to_tuple([Key, '$2', '$3', '$4' | BoundPlaceholders]),
    BucketUpdate =
        lists:sublist(BoundPlaceholders, BucketPos) ++
            [
                {'+', gen_query_placeholder(?BUCKETS_START + BucketPos), Count}
                | lists:nthtail(BucketPos + 1, BoundPlaceholders)
            ],
    HistUpdate = list_to_tuple([{Key}, '$2', '$3', {'+', '$4', Value * Count} | BucketUpdate]),
    [{HistMatch, [], [{HistUpdate}]}].

buckets_seq(Buckets) when is_list(Buckets) ->
    lists:seq(?BUCKETS_START, ?BUCKETS_START + length(Buckets) - 1);
buckets_seq(Buckets) when is_tuple(Buckets) ->
    lists:seq(?BUCKETS_START, ?BUCKETS_START + tuple_size(Buckets) - 1).

generate_update_spec(Buckets) ->
    [{Index, 0} || Index <- buckets_seq(Buckets)].

gen_query_placeholder(Index) ->
    list_to_atom([$$ | integer_to_list(Index)]).

gen_query_bound_placeholders(Buckets) ->
    [gen_query_placeholder(Index) || Index <- buckets_seq(Buckets)].

augment_counters([Start | Counters]) ->
    augment_counters(Counters, [Start], Start).

augment_counters([], LAcc, _CAcc) ->
    lists:reverse(LAcc);
augment_counters([Counter | Counters], LAcc, CAcc) ->
    augment_counters(Counters, [CAcc + Counter | LAcc], CAcc + Counter).

metric_buckets(Metric) ->
    element(?BOUNDS_POS, Metric).

reduce_buckets_counters(Metrics) ->
    ABuckets =
        [
            sub_tuple_to_list(
                Metric,
                ?BUCKETS_START,
                ?BUCKETS_START + tuple_size(metric_buckets(Metric))
            )
         || Metric <- Metrics
        ],
    [lists:sum(Bucket) || Bucket <- transpose(ABuckets)].

transpose([[] | _]) ->
    [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

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
    Bounds1 = lists:zip(Bounds, BCounters),
    Labels1 = CLabels ++ lists:zip(Labels, LabelValues),
    Sum = prometheus_time:maybe_convert_to_du(DU, ISum + FSum),
    prometheus_model_helpers:histogram_metric(Labels1, Bounds1, lists:last(BCounters), Sum).

load_all_values(Registry, Name, Bounds) ->
    BoundPlaceholders = gen_query_bound_placeholders(Bounds),
    QuerySpec = [{Registry, Name, '$1', '_'}, '_', '$3', '$4' | BoundPlaceholders],
    ets:match(?TABLE, list_to_tuple(QuerySpec)).

deregister_select(Registry, Name, Buckets) ->
    BoundCounters = lists:duplicate(length(Buckets), '_'),
    MetricSpec = [{Registry, Name, '_', '_'}, '_', '_', '_' | BoundCounters],
    [{list_to_tuple(MetricSpec), [], [true]}].

delete_metrics(Registry, Buckets) ->
    BoundCounters = lists:duplicate(length(Buckets), '_'),
    MetricSpec = [{Registry, '_', '_', '_'}, '_', '_', '_' | BoundCounters],
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
    Fun = fun
        ([Labels | V], ResAcc) when is_map_key(Labels, ResAcc) ->
            PrevSum = maps:get(Labels, ResAcc),
            ResAcc#{Labels => [lists:sum(C) || C <- transpose([PrevSum, V])]};
        ([Labels | V], ResAcc) ->
            ResAcc#{Labels => V}
    end,
    lists:foldl(Fun, #{}, MFValues).

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
