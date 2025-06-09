-module(prometheus_quantile_summary).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Summary metric, to track the size of events and report quantiles Based on prometheus_summary

Example use cases for Summaries:
* Response latency;
* Request size;
* Response size.

Example:

```erlang
-module(my_proxy_instrumenter).

setup() ->
    prometheus_quantile_summary:declare([{name, request_size_bytes},
                                         {help, \"Request size in bytes.\"}]),
    prometheus_quantile_summary:declare([{name, response_size_bytes},
                                         {help, \"Response size in bytes.\"}]).

observe_request(Size) ->
    prometheus_quantile_summary:observe(request_size_bytes, Size).

observe_response(Size) ->
    prometheus_quantile_summary:observe(response_size_bytes, Size).

```

Reports:

```text
request_size_bytes_size
request_size_bytes_count
request_size_bytes\{quantile=\"0.5\"\}
request_size_bytes\{quantile=\"0.9\"\}
request_size_bytes\{quantile=\"0.95\"\}
```

### Configuration
See `t:ddskerl_ets:opts/0` for configuration options. These need to be passed as a proplist.
""").

-define(WIDTH, 16).

%%% metric
-export([
    new/1,
    declare/1,
    set_default/2,
    deregister/1,
    deregister/2,
    observe/2,
    observe/3,
    observe/4,
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

-define(TABLE, ?PROMETHEUS_QUANTILE_SUMMARY_TABLE).

?DOC("""
Creates a summary using `Spec`.

Raises:
* `{missing_metric_spec_key, Key, Spec}` error if required `Spec` key is missing.
* `{invalid_metric_name, Name, Message}` error if metric `Name` is invalid.
* `{invalid_metric_help, Help, Message}` error if metric `Help` is invalid.
* `{invalid_metric_labels, Labels, Message}` error if `Labels` isn't a list.
* `{invalid_label_name, Name, Message}` error if `Name` isn't a valid label name.
* `{invalid_value_error, Value, Message}` error if `duration_unit` is unknown or doesn't match metric name.
* `{mf_already_exists, {Registry, Name}, Message}` error if a summary with the same `Spec` already exists.
""").
-spec new(prometheus_metric:spec()) -> ok.
new(Spec) ->
    Spec1 = validate_summary_spec(Spec),
    prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec1).

?DOC("""
Creates a summary using `Spec`. If a summary with the same `Spec` exists returns `false`.

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
    Spec1 = validate_summary_spec(Spec),
    prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec1).

?DOC(false).
-spec set_default(prometheus_registry:registry(), prometheus_metric:name()) -> boolean().
set_default(Registry, Name) ->
    #{error := Error, bound := Bound} = get_configuration(Registry, Name),
    Key = key(Registry, Name, []),
    ddskerl_ets:new(?TABLE, Key, Error, Bound).

?DOC(#{equiv => deregister(default, Name)}).
-spec deregister(prometheus_metric:name()) -> {boolean(), boolean()}.
deregister(Name) ->
    deregister(default, Name).

?DOC("""
Removes all summary series with name `Name` and removes Metric Family from `Registry`.

After this call new/1 for `Name` and `Registry` will succeed.

Returns `{true, _}` if `Name` was a registered summary. Otherwise returns `{false, _}`.
""").
-spec deregister(prometheus_registry:registry(), prometheus_metric:name()) ->
    {boolean(), boolean()}.
deregister(Registry, Name) ->
    MFR = prometheus_metric:deregister_mf(?TABLE, Registry, Name),
    NumDeleted = ets:select_delete(?TABLE, deregister_select(Registry, Name)),
    {MFR, NumDeleted > 0}.

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
* `{invalid_value, Value, Message}` if `Value` isn't an integer.
* `{unknown_metric, Registry, Name}` error if summary with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec observe(Registry, Name, LabelValues, Value) -> ok when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Value :: number().
observe(Registry, Name, LabelValues, Value) when is_number(Value) ->
    Key = key(Registry, Name, LabelValues),
    case ets:member(?TABLE, Key) of
        true ->
            ddskerl_ets:insert(?TABLE, Key, Value);
        false ->
            insert_metric(Registry, Name, LabelValues, Key),
            observe(Registry, Name, LabelValues, Value)
    end,
    ok;
observe(_Registry, _Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "observe accepts only numbers"}).

?DOC(#{equiv => observe_duration(default, Name, [], Fun)}).
-spec observe_duration(prometheus_metric:name(), fun(() -> dynamic())) -> dynamic().
observe_duration(Name, Fun) ->
    observe_duration(default, Name, [], Fun).

?DOC(#{equiv => observe_duration(default, Name, LabelValues, Fun)}).
-spec observe_duration(Name, LabelValues, Value) -> dynamic() when
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Value :: fun(() -> dynamic()).
observe_duration(Name, LabelValues, Fun) ->
    observe_duration(default, Name, LabelValues, Fun).

?DOC("""
Tracks the amount of time spent executing `Fun`.

Raises:
* `{unknown_metric, Registry, Name}` error if summary with named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
* `{invalid_value, Value, Message}` if `Fun` isn't a function.
""").
-spec observe_duration(Registry, Name, LabelValues, Value) -> dynamic() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Value :: fun(() -> dynamic()).
observe_duration(Registry, Name, LabelValues, Fun) when is_function(Fun) ->
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
Removes summary series identified by `Registry`, `Name` and `LabelValues`.

Raises:
* `{unknown_metric, Registry, Name}` error if summary with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec remove(Registry, Name, LabelValues) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
remove(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    List = lists:flatten([
        ets:take(?TABLE, {Registry, Name, LabelValues, SId})
     || SId <- schedulers_seq()
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
Resets the value of the summary identified by `Registry`, `Name` and `LabelValues`.

Raises:
* `{unknown_metric, Registry, Name}` error if summary with name `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec reset(Registry, Name, LabelValues) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values().
reset(Registry, Name, LabelValues) ->
    _ = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    [
        catch ddskerl_ets:reset(?TABLE, {Registry, Name, LabelValues, SId})
     || SId <- schedulers_seq()
    ],
    true.

?DOC(#{equiv => value(default, Name, [])}).
-spec value(prometheus_metric:name()) ->
    {non_neg_integer(), number(), [{float(), float()}]} | undefined.
value(Name) ->
    value(default, Name, []).

?DOC(#{equiv => value(default, Name, LabelValues)}).
-spec value(prometheus_metric:name(), prometheus_metric:label_values()) ->
    {non_neg_integer(), number(), [{float(), float()}]} | undefined.
value(Name, LabelValues) ->
    value(default, Name, LabelValues).

?DOC("""
Returns the value of the summary identified by `Registry`, `Name` and `LabelValues`.
If there is no summary for `LabelValues`, returns `undefined`.

If duration unit set, sum will be converted to the duration unit.
[Read more here](`m:prometheus_time`).

Raises:
* `{unknown_metric, Registry, Name}` error if summary named `Name` can't be found in `Registry`.
* `{invalid_metric_arity, Present, Expected}` error if labels count mismatch.
""").
-spec value(Registry, Name, LabelValues) -> Result when
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    LabelValues :: prometheus_metric:label_values(),
    Result :: {non_neg_integer(), number(), [{float(), float()}]} | undefined.
value(Registry, Name, LabelValues) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    case
        lists:any(
            fun(SId) -> ets:member(?TABLE, {Registry, Name, LabelValues, SId}) end, schedulers_seq()
        )
    of
        false ->
            undefined;
        true ->
            DU = prometheus_metric:mf_duration_unit(MF),
            #{quantiles := QNs} = prometheus_metric:mf_data(MF),
            Total = make_ref(),
            insert_metric(Registry, Name, LabelValues, Total),
            [
                catch ddskerl_ets:merge(?TABLE, Total, ?TABLE, {Registry, Name, LabelValues, SId})
             || SId <- schedulers_seq()
            ],
            case ddskerl_ets:total(?TABLE, Total) of
                0 ->
                    ets:delete(?TABLE, Total),
                    {0, 0, []};
                Count ->
                    Sum = ddskerl_ets:sum(?TABLE, Total),
                    DuSum = prometheus_time:maybe_convert_to_du(DU, Sum),
                    Values = [
                        {QN,
                            prometheus_time:maybe_convert_to_du(
                                DU,
                                ddskerl_ets:quantile(?TABLE, Total, QN)
                            )}
                     || QN <- QNs
                    ],
                    ets:delete(?TABLE, Total),
                    {Count, DuSum, Values}
            end
    end.

-spec default_quantiles() -> [float()].
default_quantiles() ->
    [0.5, 0.90, 0.95].

-spec values(prometheus_registry:registry(), prometheus_metric:name()) ->
    [{[{atom(), dynamic()}], non_neg_integer(), infinity | number(), [{float(), float()}]}].
values(Registry, Name) ->
    case prometheus_metric:check_mf_exists(?TABLE, Registry, Name) of
        false ->
            [];
        MF ->
            DU = prometheus_metric:mf_duration_unit(MF),
            Labels = prometheus_metric:mf_labels(MF),
            loop_through_keys(
                Registry, Name, DU, Labels, sets:new([{version, 2}]), [], ets:first(?TABLE)
            )
    end.

loop_through_keys(_, _, _, _, _, Acc, '$end_of_table') ->
    Acc;
loop_through_keys(
    Registry, Name, DU, Labels, Set, Acc, {Registry, Name, LabelValues, _} = CurrentKey
) ->
    Key = {Registry, Name, LabelValues},
    case sets:is_element(Key, Set) of
        true ->
            NextKey = ets:next(?TABLE, CurrentKey),
            loop_through_keys(Registry, Name, DU, Labels, Set, Acc, NextKey);
        false ->
            {Count, Sum, QNs} = value(Registry, Name, LabelValues),
            Value = {
                lists:zip(Labels, LabelValues),
                Count,
                prometheus_time:maybe_convert_to_du(DU, Sum),
                QNs
            },
            NextKey = ets:next(?TABLE, CurrentKey),
            NewSet = sets:add_element(Key, Set),
            loop_through_keys(Registry, Name, DU, Labels, NewSet, [Value | Acc], NextKey)
    end;
loop_through_keys(Registry, Name, DU, Labels, Set, Acc, CurrentKey) ->
    NextKey = ets:next(?TABLE, CurrentKey),
    loop_through_keys(Registry, Name, DU, Labels, Set, Acc, NextKey).

%%====================================================================
%% Collector API
%%====================================================================

?DOC(false).
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(Registry) ->
    prometheus_metric:deregister_mf(?TABLE, Registry),
    ets:select_delete(?TABLE, clean_registry_select(Registry)),
    ok.

?DOC(false).
-spec collect_mf(prometheus_registry:registry(), prometheus_collector:collect_mf_callback()) -> ok.
collect_mf(Registry, Callback) ->
    Metrics = prometheus_metric:metrics(?TABLE, Registry),
    [
        Callback(create_summary(Name, Help, {CLabels, Labels, Registry, DU, Data}))
     || [Name, {Labels, Help}, CLabels, DU, Data] <- Metrics
    ],
    ok.

?DOC(false).
-spec collect_metrics(prometheus_metric:name(), tuple()) ->
    [prometheus_model:'Metric'()].
collect_metrics(Name, {CLabels, Labels, Registry, DU, _Configuration}) ->
    loop_through_keys(
        Name, CLabels, Labels, Registry, DU, sets:new([{version, 2}]), [], ets:first(?TABLE)
    ).

loop_through_keys(_, _, _, _, _, _, Acc, '$end_of_table') ->
    Acc;
loop_through_keys(
    Name, CLabels, Labels, Registry, DU, Set, Acc, {Registry, Name, LabelValues, _} = CurrentKey
) ->
    Key = {Registry, Name, LabelValues},
    case sets:is_element(Key, Set) of
        true ->
            NextKey = ets:next(?TABLE, CurrentKey),
            loop_through_keys(Name, CLabels, Labels, Registry, DU, Set, Acc, NextKey);
        false ->
            {Count, Sum, QNs} = value(Registry, Name, LabelValues),
            Value = prometheus_model_helpers:summary_metric(
                CLabels ++ lists:zip(Labels, LabelValues), Count, Sum, QNs
            ),
            NextKey = ets:next(?TABLE, CurrentKey),
            NewSet = sets:add_element(Key, Set),
            loop_through_keys(Name, CLabels, Labels, Registry, DU, NewSet, [Value | Acc], NextKey)
    end;
loop_through_keys(Name, CLabels, Labels, Registry, DU, Set, Acc, CurrentKey) ->
    NextKey = ets:next(?TABLE, CurrentKey),
    loop_through_keys(Name, CLabels, Labels, Registry, DU, Set, Acc, NextKey).

%%====================================================================
%% Private Parts
%%====================================================================

clean_registry_select(Registry) ->
    [
        {'$1',
            [
                {'is_tuple', {element, 1, '$1'}},
                {'==', Registry, {element, 1, {element, 1, '$1'}}}
            ],
            [true]}
    ].

deregister_select(Registry, Name) ->
    [
        {'$1',
            [
                {'is_tuple', {element, 1, '$1'}},
                {'==', Registry, {element, 1, {element, 1, '$1'}}},
                {'==', Name, {element, 2, {element, 1, '$1'}}}
            ],
            [true]}
    ].

validate_summary_spec(Spec) ->
    QNs = prometheus_metric_spec:get_value(quantiles, Spec, default_quantiles()),
    Error = prometheus_metric_spec:get_value(error, Spec, 0.01),
    Bound = prometheus_metric_spec:get_value(bound, Spec, 2184),
    validate_error(Error),
    validate_bound(Bound),
    [
        {data, #{
            ets_table => ?TABLE,
            quantiles => QNs,
            error => Error,
            bound => Bound
        }}
        | Spec
    ].

insert_metric(Registry, Name, LabelValues, Key) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    Configuration0 = prometheus_metric:mf_data(MF),
    Configuration = Configuration0#{name => Key},
    ddskerl_ets:new(Configuration).

get_configuration(Registry, Name) ->
    MF = prometheus_metric:check_mf_exists(?TABLE, Registry, Name),
    prometheus_metric:mf_data(MF).

key(Registry, Name, LabelValues) ->
    X = erlang:system_info(scheduler_id),
    Rnd = X band (?WIDTH - 1),
    {Registry, Name, LabelValues, Rnd}.

schedulers_seq() ->
    lists:seq(0, ?WIDTH - 1).

create_summary(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, summary, ?MODULE, Data).

validate_error(Error) when is_number(Error), 0.0 < Error, Error < 100.0 ->
    ok;
validate_error(Error) ->
    erlang:error({invalid_error, Error, "Error should be a percentage point in (0,100)"}).

validate_bound(Bound) when is_integer(Bound), 0 < Bound ->
    ok;
validate_bound(Bound) ->
    erlang:error({invalid_bound, Bound, "Bound should be a positive integer"}).
