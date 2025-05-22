-module(prometheus_counter).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Counter is a Metric that represents a single numerical value that only ever goes up.
That implies that it cannot be used to count items whose number can also go down, e. g.
the number of currently running processes. Those \"counters\" are represented by `m:prometheus_gauge`.

A Counter is typically used to count requests served, tasks completed, errors occurred, etc.

Examople use cases for Counters:

* Number of requests processed
* Number of items that were inserted into a queue
* Total amount of data a system has processed

Use the [rate()](https://prometheus.io/docs/querying/functions/#rate())/
[irate()](https://prometheus.io/docs/querying/functions/#irate())
functions in Prometheus to calculate the rate of increase of a Counter.
By convention, the names of Counters are suffixed by `_total`.

To create a counter use either `new/1` or `declare/1`, the difference is that `new/1` will raise
`{mf_already_exists, {Registry, Name}, Message}` error if counter with the same
`Registry`, `Name` and `Labels` combination already exists.
Both accept `Spec` `t:proplists:proplist/0` with the same set of keys:

* `Registry`: optional, default is `default`;
* `Name`: required, can be an atom or a string;
* `Help`: required, must be a string;
* `Labels`: optional, default is `[]`.

Example:

```erlang
-module(my_service_instrumenter).

-export([setup/0, inc/1]).

setup() ->
    prometheus_counter:declare([{name, my_service_requests_total},
                                {help, \"Requests count\"},
                                {labels, caller}]).

inc(Caller) ->
    prometheus_counter:inc(my_service_requests_total, [Caller]).
```
""").

%%% metric
-export([
    new/1,
    declare/1,
    deregister/1,
    deregister/2,
    set_default/2,
    inc/1,
    inc/2,
    inc/3,
    inc/4,
    remove/1,
    remove/2,
    remove/3,
    reset/1,
    reset/2,
    reset/3,
    value/1,
    value/2,
    value/3,
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

%% Records will be triplets `{Key, ISum, FSum}`
%%  where `ISum` are integer sums, and `FSum` are float sums
%% Keys use the scheduler index, because counters by definition aggregate
%%  even after parallel updates. Hence we can run updates without locks.
-define(TABLE, ?PROMETHEUS_COUNTER_TABLE).
-define(ISUM_POS, 2).
-define(FSUM_POS, 3).
-define(WIDTH, 16).

?DOC("""
Creates a counter using `Spec`.

Raises:

* `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key is missing.
* `{invalid_metric_name, Name, Message}` error if metric `Name` is invalid.
* `{invalid_metric_help, Help, Message}` error if metric `Help` is invalid.
* `{invalid_metric_labels, Labels, Message}` error if `Labels` isn't a list.
* `{invalid_label_name, Name, Message}` error if `Name` isn't a valid label name.
* `{mf_already_exists, {Registry, Name}, Message}` error if a counter with the same `Spec` already exists.
""").
-spec new(prometheus_metric:spec()) -> ok.
new(Spec) ->
    prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

?DOC("""
Creates a counter using `Spec`, if a counter with the same `Spec` exists returns `false`.

Raises:

* `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key is missing.
* `{invalid_metric_name, Name, Message}` error if metric `Name` is invalid.
* `{invalid_metric_help, Help, Message}` error if metric `Help` is invalid.
* `{invalid_metric_labels, Labels, Message}` error if `Labels` isn't a list.
* `{invalid_label_name, Name, Message}` error if `Name` isn't a valid label name.
""").
-spec declare(prometheus_metric:spec()) -> boolean().
declare(Spec) ->
    prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec).

?DOC(#{equiv => deregister(default, Name)}).
-spec deregister(prometheus_metric:name()) -> {boolean(), boolean()}.
deregister(Name) ->
    deregister(default, Name).

?DOC("""
Removes all counter series with name `Name` and removes Metric Family from `Registry`.

After this call new/1 for `Name` and `Registry` will succeed.

Returns `{true, _}` if `Name` was a registered counter. Otherwise returns `{true, _}`.
""").
-spec deregister(prometheus_registry:registry(), prometheus_metric:name()) ->
    {boolean(), boolean()}.
deregister(Registry, Name) ->
    MFR = prometheus_metric:deregister_mf(?TABLE, Registry, Name),
    NumDeleted = ets:select_delete(?TABLE, deregister_select(Registry, Name)),
    {MFR, NumDeleted > 0}.

?DOC(false).
-spec set_default(prometheus_registry:registry(), prometheus_metric:name()) -> boolean().
set_default(Registry, Name) ->
    ets:insert_new(?TABLE, {key(Registry, Name, []), 0, 0}).

?DOC(#{equiv => inc(default, Name, [], 1)}).
-spec inc(prometheus_metric:name()) -> ok.
inc(Name) ->
    inc(default, Name, [], 1).

?DOC("""
If the second argument is a list, equivalent to [inc(default, Name, LabelValues,
1)](`inc/4`) otherwise equivalent to [inc(default, Name, [], Value)](`inc/4`).
""").
-spec inc(prometheus_metric:name(), prometheus_metric:label_values() | non_neg_integer()) -> ok.
inc(Name, LabelValues) when is_list(LabelValues) ->
    inc(default, Name, LabelValues, 1);
inc(Name, Value) ->
    inc(default, Name, [], Value).

?DOC(#{equiv => inc(default, Name, LabelValues, Value)}).
-spec inc(prometheus_metric:name(), prometheus_metric:label_values(), non_neg_integer()) -> ok.
inc(Name, LabelValues, Value) ->
    inc(default, Name, LabelValues, Value).

?DOC("""
Increments the counter identified by `Registry`, `Name` and `LabelValues` by `Value`.

Raises:

* `{invalid_value, Value, Message}` if `Value` isn't a positive number.
* `{unknown_metric, Registry, Name}` error if counter with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec inc(Registry, Name, LabelValues, Value) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Value :: non_neg_integer() | float().
inc(Registry, Name, LabelValues, Value) when is_integer(Value), Value >= 0 ->
    Key = key(Registry, Name, LabelValues),
    Spec = {?ISUM_POS, Value},
    try
        ets:update_counter(?TABLE, Key, Spec)
    catch
        error:badarg ->
            insert_metric_int(Registry, Name, LabelValues, Value, fun inc/4)
    end,
    ok;
inc(Registry, Name, LabelValues, Value) when is_float(Value), Value >= 0 ->
    Key = key(Registry, Name, LabelValues),
    Spec = [{{Key, '$1', '$2'}, [], [{{{Key}, '$1', {'+', '$2', Value}}}]}],
    case ets:select_replace(?TABLE, Spec) of
        0 ->
            insert_metric_float(Registry, Name, LabelValues, Value, fun inc/4);
        1 ->
            ok
    end;
inc(_Registry, _Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "inc accepts only non-negative numbers"}).

?DOC(#{equiv => remove(default, Name, [])}).
?DOC("Equivalent to [remove(default, Name, [])](`remove/3`).").
-spec remove(prometheus_metric:name()) -> boolean().
remove(Name) ->
    remove(default, Name, []).

?DOC(#{equiv => remove(default, Name, LabelValues)}).
?DOC("Equivalent to [remove(default, Name, LabelValues)](`remove/3`).").
-spec remove(prometheus_metric:name(), prometheus_metric:label_values()) -> boolean().
remove(Name, LabelValues) ->
    remove(default, Name, LabelValues).

?DOC("""
Removes counter series identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if counter with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec remove(Registry, Name, LabelValues) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
remove(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    List = [
        ets:take(?TABLE, {Registry, Name, LabelValues, Scheduler})
     || Scheduler <- schedulers_seq()
    ],
    case lists:flatten(List) of
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
Resets the value of the counter identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if counter with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec reset(Registry, Name, LabelValues) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
reset(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    Spec = [{?ISUM_POS, 0}, {?FSUM_POS, 0}],
    List = [
        ets:update_element(?TABLE, {Registry, Name, LabelValues, Scheduler}, Spec)
     || Scheduler <- schedulers_seq()
    ],
    case lists:usort(List) of
        [_, _] -> true;
        [true] -> true;
        _ -> false
    end.

?DOC(#{equiv => value(default, Name, [])}).
-spec value(prometheus_metric:name()) -> number() | undefined.
value(Name) ->
    value(default, Name, []).

?DOC(#{equiv => value(default, Name, LabelValues)}).
-spec value(prometheus_metric:name(), prometheus_metric:label_values()) -> number() | undefined.
value(Name, LabelValues) ->
    value(default, Name, LabelValues).

?DOC("""
Returns the value of the counter identified by `Registry`, `Name` and `LabelValues`.

If there is no counter for `LabelValues`, returns `undefined`.

Raises:
* `{unknown_metric, Registry, Name}` error if counter named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec value(Registry, Name, LabelValues) -> number() | undefined when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
value(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    Spec = [{{{Registry, Name, LabelValues, '_'}, '$1', '$2'}, [], [{'+', '$1', '$2'}]}],
    case ets:select(?TABLE, Spec) of
        [] -> undefined;
        List -> lists:sum(List)
    end.

-spec values(prometheus_registry:registry(), prometheus_metric:name()) -> [{list(), number()}].
values(Registry, Name) ->
    case prometheus_metric:check_mf_exists(?TABLE, Registry, Name) of
        false ->
            [];
        MF ->
            Labels = prometheus_metric:mf_labels(MF),
            MFValues = load_all_values(Registry, Name),
            LabelValues = reduce_label_values(MFValues),
            Fun = fun(VLabels, Value) -> {VLabels, Value} end,
            serialize_label_values(Fun, Labels, LabelValues)
    end.

%%====================================================================
%% Collector API
%%====================================================================

?DOC(false).
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(Registry) ->
    prometheus_metric:deregister_mf(?TABLE, Registry),
    true = ets:match_delete(?TABLE, {{Registry, '_', '_', '_'}, '_', '_'}),
    ok.

?DOC(false).
-spec collect_mf(prometheus_registry:registry(), prometheus_collector:collect_mf_callback()) -> ok.
collect_mf(Registry, Callback) ->
    [
        Callback(create_counter(Name, Help, {CLabels, Labels, Registry}))
     || [Name, {Labels, Help}, CLabels, _, _] <- prometheus_metric:metrics(?TABLE, Registry)
    ],
    ok.

?DOC(false).
-spec collect_metrics(prometheus_metric:name(), tuple()) ->
    [prometheus_model:'Metric'()].
collect_metrics(Name, {CLabels, Labels, Registry}) ->
    MFValues = load_all_values(Registry, Name),
    LabelValues = reduce_label_values(MFValues),
    Fun = fun(VLabels, Value) ->
        prometheus_model_helpers:counter_metric(CLabels ++ VLabels, Value)
    end,
    serialize_label_values(Fun, Labels, LabelValues).

%%====================================================================
%% Private Parts
%%====================================================================

deregister_select(Registry, Name) ->
    [{{{Registry, Name, '_', '_'}, '_', '_'}, [], [true]}].

insert_metric_int(Registry, Name, LabelValues, Value, ConflictCB) ->
    Counter = {key(Registry, Name, LabelValues), Value, 0},
    insert_metric(Registry, Name, LabelValues, Value, ConflictCB, Counter).

insert_metric_float(Registry, Name, LabelValues, Value, ConflictCB) ->
    Counter = {key(Registry, Name, LabelValues), 0, Value},
    insert_metric(Registry, Name, LabelValues, Value, ConflictCB, Counter).

insert_metric(Registry, Name, LabelValues, Value, ConflictCB, Counter) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    case ets:insert_new(?TABLE, Counter) of
        %% some sneaky process already inserted
        false ->
            ConflictCB(Registry, Name, LabelValues, Value);
        true ->
            ok
    end.

load_all_values(Registry, Name) ->
    ets:match(?TABLE, {{Registry, Name, '$1', '_'}, '$2', '$3'}).

schedulers_seq() ->
    lists:seq(0, ?WIDTH - 1).

key(Registry, Name, LabelValues) ->
    X = erlang:system_info(scheduler_id),
    Rnd = X band (?WIDTH - 1),
    {Registry, Name, LabelValues, Rnd}.

reduce_label_values(MFValues) ->
    Fold = fun([Labels, I, F], ResAcc) ->
        PrevSum = maps:get(Labels, ResAcc, 0),
        ResAcc#{Labels => PrevSum + I + F}
    end,
    lists:foldl(Fold, #{}, MFValues).

serialize_label_values(Fun, Labels, Values) ->
    Fold = fun(LabelValues, Value, L) ->
        [Fun(lists:zip(Labels, LabelValues), Value) | L]
    end,
    maps:fold(Fold, [], Values).

create_counter(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, counter, ?MODULE, Data).
