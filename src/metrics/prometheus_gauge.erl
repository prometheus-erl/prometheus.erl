-module(prometheus_gauge).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Gauge metric, to report instantaneous values.

Gauge is a metric that represents a single numerical value that can arbitrarily go up and down.

A Gauge is typically used for measured values like temperatures or current memory usage,
but also \"counts\" that can go up and down, like the number of running processes.

It can take the value of undefined when the value is not known.
In that case, increments and decrements will not be possible
and explicitly setting to a new value will be necessary.

Example use cases for Gauges:

* In-progress requests
* Number of items in a queue
* Free memory
* Total memory
* Temperature

Example:

```erlang
-module(my_pool_instrumenter).

-export([setup/0, set_size/1]).

setup() ->
    prometheus_gauge:declare([{name, my_pool_size},
                              {help, \"Pool size.\"}]),
    prometheus_gauge:declare([{name, my_pool_checked_out},
                              {help, \"Number of checked out sockets\"}]).

set_size(Size) ->
    prometheus_gauge:set(my_pool_size, Size)

track_checked_out_sockets(CheckoutFun) ->
    prometheus_gauge:track_inprogress(my_pool_checked_out, CheckoutFun)..
```
""").

%%% metric
-export([
    new/1,
    declare/1,
    deregister/1,
    deregister/2,
    set_default/2,
    set/2,
    set/3,
    set/4,
    inc/1,
    inc/2,
    inc/3,
    inc/4,
    dec/1,
    dec/2,
    dec/3,
    dec/4,
    set_to_current_time/1,
    set_to_current_time/2,
    set_to_current_time/3,
    track_inprogress/2,
    track_inprogress/3,
    track_inprogress/4,
    set_duration/2,
    set_duration/3,
    set_duration/4,
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
%%  where `IGauge` are integer gauges, and `FGauge` are float gauges
-define(TABLE, ?PROMETHEUS_GAUGE_TABLE).
-define(IGAUGE_POS, 2).
-define(FGAUGE_POS, 3).

%%====================================================================
%% Metric API
%%====================================================================

?DOC("""
Creates a gauge using `Spec`.

Raises:

* `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key is missing.
* `{invalid_metric_name, Name, Message}` error if metric `Name` is invalid.
* `{invalid_metric_help, Help, Message}` error if metric `Help` is invalid.
* `{invalid_metric_labels, Labels, Message}` error if `Labels` isn't a list.
* `{invalid_label_name, Name, Message}` error if `Name` isn't a valid label name.
* `{invalid_value_error, Value, Message}` error if `duration_unit` is unknown or doesn't match metric name.
* `{mf_already_exists, {Registry, Name}, Message}` error if a gauge with the same `Spec` already exists.
""").
-spec new(prometheus_metric:spec()) -> ok.
new(Spec) ->
    prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

?DOC("""
Creates a gauge using `Spec`. If a gauge with the same `Spec` exists returns `false`.

Raises:

* `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key is missing.
* `{invalid_metric_name, Name, Message}` error if metric `Name` is invalid.
* `{invalid_metric_help, Help, Message}` error if metric `Help` is invalid.
* `{invalid_metric_labels, Labels, Message}` error if `Labels` isn't a list.
* `{invalid_label_name, Name, Message}` error if `Name` isn't a valid label name.
* `{invalid_value_error, Value, MessagE}` error if `duration_unit` is unknown or doesn't match metric name.
""").
-spec declare(prometheus_metric:spec()) -> boolean().
declare(Spec) ->
    prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec).

?DOC(#{equiv => deregister(default, Name)}).
-spec deregister(prometheus_metric:name()) -> {boolean(), boolean()}.
deregister(Name) ->
    deregister(default, Name).

?DOC("""
Removes all gauge series with name `Name` and removes Metric Family from `Registry`.

After this call new/1 for `Name` and `Registry` will succeed.

Returns `{true, _}` if `Name` was a registered gauge. Otherwise returns `{false, _}`.
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
    ets:insert_new(?TABLE, {{Registry, Name, []}, 0, 0}).

?DOC(#{equiv => set(default, Name, [], Value)}).
-spec set(prometheus_metric:name(), number()) -> ok.
set(Name, Value) ->
    set(default, Name, [], Value).

?DOC(#{equiv => set(default, Name, LabelValues, Value)}).
-spec set(prometheus_metric:name(), prometheus_metric:labels(), number()) -> ok.
set(Name, LabelValues, Value) ->
    set(default, Name, LabelValues, Value).

?DOC("""
Sets the gauge identified by `Registry`, `Name` and `LabelValues` to `Value`.

Raises:

* `{invalid_value, Value, Message}` if `Value` isn't a number or `undefined`.
* `{unknown_metric, Registry, Name}` error if gauge with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec set(Registry, Name, LabelValues, Value) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:labels(),
    Value :: undefined | number().
set(Registry, Name, LabelValues, Value) when is_integer(Value) ->
    Update = [{?IGAUGE_POS, Value}, {?FGAUGE_POS, 0}],
    case ets:update_element(?TABLE, {Registry, Name, LabelValues}, Update) of
        false ->
            insert_metric_int(Registry, Name, LabelValues, Value, fun set/4);
        true ->
            ok
    end;
set(Registry, Name, LabelValues, Value) when is_float(Value) ->
    Update = [{?IGAUGE_POS, 0}, {?FGAUGE_POS, Value}],
    case ets:update_element(?TABLE, {Registry, Name, LabelValues}, Update) of
        false ->
            insert_metric_float(Registry, Name, LabelValues, Value, fun set/4);
        true ->
            ok
    end;
set(Registry, Name, LabelValues, undefined) ->
    Update = [{?IGAUGE_POS, undefined}, {?FGAUGE_POS, undefined}],
    case ets:update_element(?TABLE, {Registry, Name, LabelValues}, Update) of
        false ->
            insert_metric_undefined(Registry, Name, LabelValues, undefined, fun set/4);
        true ->
            ok
    end;
set(_, _, _, Value) ->
    erlang:error({invalid_value, Value, "set accepts only numbers and 'undefined'"}).

?DOC(#{equiv => inc(default, Name, [], 1)}).
-spec inc(prometheus_metric:name()) -> ok.
inc(Name) ->
    inc(default, Name, [], 1).

?DOC("""
If the second argument is a list, equivalent to [inc(default, Name, LabelValues, 1)](`inc/4`)
otherwise equivalent to [inc(default, Name, [], Value)](`inc/4`).
""").
-spec inc(prometheus_metric:name(), prometheus_metric:labels() | non_neg_integer()) -> ok.
inc(Name, LabelValues) when is_list(LabelValues) ->
    inc(default, Name, LabelValues, 1);
inc(Name, Value) ->
    inc(default, Name, [], Value).

?DOC(#{equiv => inc(default, Name, LabelValues, Value)}).
-spec inc(prometheus_metric:name(), prometheus_metric:labels(), non_neg_integer()) -> ok.
inc(Name, LabelValues, Value) ->
    inc(default, Name, LabelValues, Value).

?DOC("""
Increments the gauge identified by `Registry`, `Name` and `LabelValues` by `Value`.

Raises:

* `{invalid_value, Value, Message}` if `Value` isn't an integer.
* `{unknown_metric, Registry, Name}` error if gauge with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec inc(Registry, Name, LabelValues, Value) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:labels(),
    Value :: number().
inc(Registry, Name, LabelValues, Value) when is_integer(Value) ->
    Key = key(Registry, Name, LabelValues),
    Spec = {?IGAUGE_POS, Value},
    try
        ets:update_counter(?TABLE, Key, Spec)
    catch
        error:badarg ->
            maybe_insert_metric_for_inc_int(Registry, Name, LabelValues, Value)
    end,
    ok;
inc(Registry, Name, LabelValues, Value) when is_float(Value) ->
    Key = key(Registry, Name, LabelValues),
    Spec = [{{Key, '$1', '$2'}, [{is_integer, '$1'}], [{{{Key}, '$1', {'+', '$2', Value}}}]}],
    case ets:select_replace(?TABLE, Spec) of
        0 ->
            maybe_insert_metric_for_inc_float(Registry, Name, LabelValues, Value);
        1 ->
            ok
    end;
inc(_Registry, _Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "inc accepts only numbers"}).

?DOC(#{equiv => inc(default, Name, [], -1)}).
-spec dec(prometheus_metric:name()) -> ok.
dec(Name) ->
    inc(default, Name, [], -1).

?DOC("""
If the second argument is a list, equivalent to [inc(default, Name, LabelValues, -1)](`inc/4`)
otherwise equivalent to [inc(default, Name, [], -Value)](`inc/4`).
""").
-spec dec(prometheus_metric:name(), prometheus_metric:labels() | number()) -> ok.
dec(Name, LabelValues) when is_list(LabelValues) ->
    inc(default, Name, LabelValues, -1);
dec(Name, Value) when is_number(Value) ->
    inc(default, Name, [], -Value);
dec(_Name, Value) ->
    erlang:error({invalid_value, Value, "dec accepts only numbers"}).

?DOC(#{equiv => inc(default, Name, LabelValues, -Value)}).
-spec dec(prometheus_metric:name(), prometheus_metric:labels(), number()) -> ok.
dec(Name, LabelValues, Value) when is_number(Value) ->
    inc(default, Name, LabelValues, -Value);
dec(_Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "dec accepts only numbers"}).

?DOC(#{equiv => inc(Registry, Name, LabelValues, -Value)}).
-spec dec(Registry, Name, LabelValues, Value) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:labels(),
    Value :: number().
dec(Registry, Name, LabelValues, Value) when is_number(Value) ->
    inc(Registry, Name, LabelValues, -Value);
dec(_Registry, _Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "dec accepts only numbers"}).

?DOC(#{equiv => set_to_current_time(default, Name, [])}).
-spec set_to_current_time(prometheus_metric:name()) -> ok.
set_to_current_time(Name) ->
    set_to_current_time(default, Name, []).

?DOC(#{equiv => set_to_current_time(default, Name, LabelValues)}).
-spec set_to_current_time(prometheus_metric:name(), prometheus_metric:labels()) -> ok.
set_to_current_time(Name, LabelValues) ->
    set_to_current_time(default, Name, LabelValues).

?DOC("""
Sets the gauge identified by `Registry`, `Name` and `LabelValues` to the current unixtime.

Raises:

* `{unknown_metric, Registry, Name}` error if gauge with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec set_to_current_time(Registry, Name, LabelValues) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:labels().
set_to_current_time(Registry, Name, LabelValues) ->
    set(Registry, Name, LabelValues, os:system_time(seconds)).

?DOC(#{equiv => track_inprogress(default, Name, [], Fun)}).
-spec track_inprogress(prometheus_metric:name(), fun(() -> any())) -> any().
track_inprogress(Name, Fun) ->
    track_inprogress(default, Name, [], Fun).

?DOC(#{equiv => track_inprogress(default, Name, LabelValues, Fun)}).
-spec track_inprogress(prometheus_metric:name(), prometheus_metric:labels(), fun(() -> any())) ->
    any().
track_inprogress(Name, LabelValues, Fun) ->
    track_inprogress(default, Name, LabelValues, Fun).

?DOC("""
Sets the gauge identified by `Registry`, `Name` and `LabelValues` to the number of
currently executing `Fun`s.

Raises:

* `{unknown_metric, Registry, Name}` error if gauge with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
* `{invalid_value, Value, Message}` if `Fun` isn't a function.
""").
-spec track_inprogress(Registry, Name, LabelValues, Fun) -> any() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:labels(),
    Fun :: fun(() -> any()).
track_inprogress(Registry, Name, LabelValues, Fun) when is_function(Fun, 0) ->
    inc(Registry, Name, LabelValues, 1),
    try
        Fun()
    after
        dec(Registry, Name, LabelValues, 1)
    end;
track_inprogress(_Registry, _Name, _LabelValues, Fun) ->
    erlang:error({invalid_value, Fun, "track_inprogress accepts only functions"}).

?DOC(#{equiv => set_duration(default, Name, [], Fun)}).
-spec set_duration(prometheus_metric:name(), fun(() -> any())) -> any().
set_duration(Name, Fun) ->
    set_duration(default, Name, [], Fun).

?DOC(#{equiv => set_duration(default, Name, LabelValues, Fun)}).
-spec set_duration(prometheus_metric:name(), prometheus_metric:labels(), fun(() -> any())) -> any().
set_duration(Name, LabelValues, Fun) ->
    set_duration(default, Name, LabelValues, Fun).

?DOC("""
Sets the gauge identified by `Registry`, `Name` and `LabelValues` to the the amount of time
spent executing `Fun`.

Raises:

* `{unknown_metric, Registry, Name}` error if gauge with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
* `{invalid_value, Value, Message}` if `Fun` isn't a function.
""").
-spec set_duration(Registry, Name, LabelValues, Fun) -> any() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:labels(),
    Fun :: fun(() -> any()).
set_duration(Registry, Name, LabelValues, Fun) when is_function(Fun, 0) ->
    Start = erlang:monotonic_time(),
    try
        Fun()
    after
        set(Registry, Name, LabelValues, erlang:monotonic_time() - Start)
    end;
set_duration(_Registry, _Name, _LabelValues, Fun) ->
    erlang:error({invalid_value, Fun, "set_duration accepts only functions"}).

?DOC(#{equiv => remove(default, Name, [])}).
-spec remove(prometheus_metric:name()) -> boolean().
remove(Name) ->
    remove(default, Name, []).

?DOC(#{equiv => remove(default, Name, LabelValues)}).
-spec remove(prometheus_metric:name(), prometheus_metric:labels()) -> boolean().
remove(Name, LabelValues) ->
    remove(default, Name, LabelValues).

?DOC("""
Removes gauge series identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if gauge with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec remove(prometheus_registry:registry(), prometheus_metric:name(), prometheus_metric:labels()) ->
    boolean().
remove(Registry, Name, LabelValues) ->
    prometheus_metric:remove_labels(?TABLE, Registry, Name, LabelValues).

?DOC(#{equiv => reset(default, Name, [])}).
-spec reset(prometheus_metric:name()) -> boolean().
reset(Name) ->
    reset(default, Name, []).

?DOC(#{equiv => reset(default, Name, LabelValues)}).
-spec reset(prometheus_metric:name(), prometheus_metric:labels()) -> boolean().
reset(Name, LabelValues) ->
    reset(default, Name, LabelValues).

?DOC("""
Resets the value of the gauge identified by `Registry`, `Name` and `LabelValues`.

Raises:

* `{unknown_metric, Registry, Name}` error if gauge with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec reset(prometheus_registry:registry(), prometheus_metric:name(), prometheus_metric:labels()) ->
    boolean().
reset(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    ets:update_element(?TABLE, {Registry, Name, LabelValues}, [{?IGAUGE_POS, 0}, {?FGAUGE_POS, 0}]).

?DOC(#{equiv => value(default, Name, [])}).
-spec value(prometheus_metric:name()) -> number() | undefined.
value(Name) ->
    value(default, Name, []).

?DOC(#{equiv => value(default, Name, LabelValues)}).
-spec value(prometheus_metric:name(), prometheus_metric:labels()) -> number() | undefined.
value(Name, LabelValues) ->
    value(default, Name, LabelValues).

?DOC("""
Returns the value of the gauge identified by `Registry`, `Name` and `LabelValues`.
If there is no gauge for `LabelValues`, returns `undefined`.

If duration unit set, value will be converted to the duration unit.
[Read more here.](`m:prometheus_time`)

Raises:

* `{unknown_metric, Registry, Name}` error if gauge named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec value(prometheus_registry:registry(), prometheus_metric:name(), prometheus_metric:labels()) ->
    number() | undefined.
value(Registry, Name, LabelValues) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    DU = prometheus_metric:mf_duration_unit(MF),
    case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
        [{_Key, IValue, FValue}] -> prometheus_time:maybe_convert_to_du(DU, sum(IValue, FValue));
        [] -> undefined
    end.

-spec values(prometheus_registry:registry(), prometheus_metric:name()) ->
    [{list(), infinity | number()}].
values(Registry, Name) ->
    case prometheus_metric:check_mf_exists(?TABLE, Registry, Name) of
        false ->
            [];
        MF ->
            Labels = prometheus_metric:mf_labels(MF),
            DU = prometheus_metric:mf_duration_unit(MF),
            [
                {
                    lists:zip(Labels, LabelValues),
                    prometheus_time:maybe_convert_to_du(DU, sum(IValue, FValue))
                }
             || [LabelValues, IValue, FValue] <- load_all_values(Registry, Name)
            ]
    end.

%%====================================================================
%% Collector API
%%====================================================================

?DOC(false).
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(Registry) ->
    prometheus_metric:deregister_mf(?TABLE, Registry),
    true = ets:match_delete(?TABLE, {{Registry, '_', '_'}, '_', '_'}),
    ok.

?DOC(false).
-spec collect_mf(prometheus_registry:registry(), prometheus_collector:collect_mf_callback()) -> ok.
collect_mf(Registry, Callback) ->
    [
        Callback(create_gauge(Name, Help, {CLabels, Labels, Registry, DU}))
     || [Name, {Labels, Help}, CLabels, DU, _] <- prometheus_metric:metrics(?TABLE, Registry)
    ],
    ok.

?DOC(false).
-spec collect_metrics(prometheus_metric:name(), tuple()) ->
    [prometheus_model:'Metric'()].
collect_metrics(Name, {CLabels, Labels, Registry, DU}) ->
    [
        prometheus_model_helpers:gauge_metric(
            CLabels ++ lists:zip(Labels, LabelValues),
            prometheus_time:maybe_convert_to_du(DU, sum(IValue, FValue))
        )
     || [LabelValues, IValue, FValue] <- load_all_values(Registry, Name)
    ].

%%====================================================================
%% Private Parts
%%====================================================================

key(Registry, Name, LabelValues) ->
    {Registry, Name, LabelValues}.

maybe_insert_metric_for_inc_int(Registry, Name, LabelValues, Value) ->
    case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
        [{_Key, undefined, undefined}] ->
            erlang:error({invalid_operation, 'inc/dec', "Can't inc/dec undefined"});
        _ ->
            insert_metric_int(Registry, Name, LabelValues, Value, fun inc/4)
    end.

maybe_insert_metric_for_inc_float(Registry, Name, LabelValues, Value) ->
    case ets:lookup(?TABLE, {Registry, Name, LabelValues}) of
        [{_Key, undefined, undefined}] ->
            erlang:error({invalid_operation, 'inc/dec', "Can't inc/dec undefined"});
        _ ->
            insert_metric_float(Registry, Name, LabelValues, Value, fun inc/4)
    end.

deregister_select(Registry, Name) ->
    [{{{Registry, Name, '_'}, '_', '_'}, [], [true]}].

insert_metric_int(Registry, Name, LabelValues, Value, ConflictCB) ->
    Counter = {key(Registry, Name, LabelValues), Value, 0},
    insert_metric(Registry, Name, LabelValues, Value, ConflictCB, Counter).

insert_metric_float(Registry, Name, LabelValues, Value, ConflictCB) ->
    Counter = {key(Registry, Name, LabelValues), 0, Value},
    insert_metric(Registry, Name, LabelValues, Value, ConflictCB, Counter).

insert_metric_undefined(Registry, Name, LabelValues, Value, ConflictCB) ->
    Counter = {key(Registry, Name, LabelValues), undefined, undefined},
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
    ets:match(?TABLE, {{Registry, Name, '$1'}, '$2', '$3'}).

sum(undefined, _FValue) ->
    undefined;
sum(IValue, FValue) ->
    IValue + FValue.

create_gauge(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).
