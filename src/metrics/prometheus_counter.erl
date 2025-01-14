%% @doc
%% Counter is a Metric that represents a single numerical value that only ever
%% goes up. That implies that it cannot be used to count items whose number can
%% also go down, e.g. the number of currently running processes. Those
%% "counters" are represented by {@link prometheus_gauge}.
%%
%% A Counter is typically used to count requests served, tasks completed, errors
%% occurred, etc.
%%
%% Examople use cases for Counters:
%% <ul>
%%   <li>Number of requests processed</li>
%%   <li>Number of items that were inserted into a queue</li>
%%   <li>Total amount of data a system has processed</li>
%% </ul>
%%
%% Use the
%% <a href="https://prometheus.io/docs/querying/functions/#rate()">rate()</a>/<a
%% href="https://prometheus.io/docs/querying/functions/#irate()">irate()</a>
%% functions in Prometheus to calculate the rate of increase of a Counter.
%% By convention, the names of Counters are suffixed by `_total'.
%%
%% To create a counter use either {@link new/1} or {@link declare/1},
%% the difference is that {@link new/1} will raise
%% {:mf_already_exists, {Registry, Name}, Message} error if counter with
%% the same `Registry', `Name' and `Labels' combination already exists.
%% Both accept `Spec' [proplist](http://erlang.org/doc/man/proplists.html)
%% with the same set of keys:
%%
%%  - `Registry' - optional, default is `default';
%%  - `Name' - required, can be an atom or a string;
%%  - `Help' - required, must be a string;
%%  - `Labels' - optional, default is `[]'.
%%
%% Example:
%% <pre lang="erlang">
%% -module(my_service_instrumenter).
%%
%% -export([setup/0,
%%          inc/1]).
%%
%% setup() ->
%%   prometheus_counter:declare([{name, my_service_requests_total},
%%                               {help, "Requests count"},
%%                               {labels, caller}]).
%%
%% inc(Caller) ->
%%   prometheus_counter:inc(my_service_requests_total, [Caller]).
%%
%% </pre>
%% @end

-module(prometheus_counter).

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

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?PROMETHEUS_COUNTER_TABLE).
-define(ISUM_POS, 2).
-define(FSUM_POS, 3).
-define(WIDTH, 16).

%%====================================================================
%% Metric API
%%====================================================================

%% @doc Creates a counter using `Spec'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Spec' key
%% is missing.<br/>
%% Raises `{invalid_metric_name, Name, Message}' error if metric `Name'
%% is invalid.<br/>
%% Raises `{invalid_metric_help, Help, Message}' error if metric `Help'
%% is invalid.<br/>
%% Raises `{invalid_metric_labels, Labels, Message}' error if `Labels'
%% isn't a list.<br/>
%% Raises `{invalid_label_name, Name, Message}' error if `Name' isn't a valid
%% label name.<br/>
%% Raises `{mf_already_exists, {Registry, Name}, Message}' error if a counter
%% with the same `Spec' already exists.
%% @end
-spec new(prometheus_metric_spec:spec()) -> ok.
new(Spec) ->
    prometheus_metric:insert_new_mf(?TABLE, ?MODULE, Spec).

%% @doc Creates a counter using `Spec', if a counter with the same `Spec' exists
%% returns `false'.
%%
%% Raises `{missing_metric_spec_key, Key, Spec}' error if required `Spec' key
%% is missing.<br/>
%% Raises `{invalid_metric_name, Name, Message}' error if metric `Name'
%% is invalid.<br/>
%% Raises `{invalid_metric_help, Help, Message}' error if metric `Help'
%% is invalid.<br/>
%% Raises `{invalid_metric_labels, Labels, Message}' error if `Labels'
%% isn't a list.<br/>
%% Raises `{invalid_label_name, Name, Message}' error if `Name' isn't a valid
%% label name.
%% @end
-spec declare(prometheus_metric_spec:spec()) -> boolean().
declare(Spec) ->
    prometheus_metric:insert_mf(?TABLE, ?MODULE, Spec).

%% @equiv deregister(default, Name)
-spec deregister(prometheus_metric:name()) -> {boolean(), boolean()}.
deregister(Name) ->
    deregister(default, Name).

%% @doc
%% Removes all counter series with name `Name' and
%% removes Metric Family from `Registry'.
%%
%% After this call new/1 for `Name' and `Registry' will succeed.
%%
%% Returns `{true, _}' if `Name' was a registered counter.
%% Otherwise returns `{true, _}'.
%% @end
-spec deregister(prometheus_registry:registry(), prometheus_metric:name()) ->
    {boolean(), boolean()}.
deregister(Registry, Name) ->
    MFR = prometheus_metric:deregister_mf(?TABLE, Registry, Name),
    NumDeleted = ets:select_delete(?TABLE, deregister_select(Registry, Name)),
    {MFR, NumDeleted > 0}.

%% @private
-spec set_default(prometheus_registry:registry(), prometheus_metric:name()) -> boolean().
set_default(Registry, Name) ->
    ets:insert_new(?TABLE, {key(Registry, Name, []), 0, 0}).

%% @equiv inc(default, Name, [], 1)
-spec inc(prometheus_metric:name()) -> ok.
inc(Name) ->
    inc(default, Name, [], 1).

%% @doc If the second argument is a list, equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, LabelValues, 1)</tt></a>
%% otherwise equivalent to
%% <a href="#inc-4"><tt>inc(default, Name, [], Value)</tt></a>.
-spec inc(prometheus_metric:name(), list() | non_neg_integer()) -> ok.
inc(Name, LabelValues) when is_list(LabelValues) ->
    inc(default, Name, LabelValues, 1);
inc(Name, Value) ->
    inc(default, Name, [], Value).

%% @equiv inc(default, Name, LabelValues, Value)
-spec inc(prometheus_metric:name(), list(), non_neg_integer()) -> ok.
inc(Name, LabelValues, Value) ->
    inc(default, Name, LabelValues, Value).

%% @doc Increments the counter identified by `Registry', `Name'
%% and `LabelValues' by `Value'.
%%
%% Raises `{invalid_value, Value, Message}' if `Value'
%% isn't a positive number.<br/>
%% Raises `{unknown_metric, Registry, Name}' error if counter with named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
-spec inc(
    prometheus_registry:registry(),
    prometheus_metric:name(),
    list(),
    non_neg_integer()
) -> ok.
inc(Registry, Name, LabelValues, Value) when is_integer(Value), Value >= 0 ->
    try
        ets:update_counter(
            ?TABLE,
            key(Registry, Name, LabelValues),
            {?ISUM_POS, Value}
        )
    catch
        error:badarg ->
            insert_metric(Registry, Name, LabelValues, Value, fun inc/4)
    end,
    ok;
inc(Registry, Name, LabelValues, Value) when is_number(Value), Value >= 0 ->
    Key = key(Registry, Name, LabelValues),
    case
        ets:select_replace(
            ?TABLE,
            [{{Key, '$1', '$2'}, [], [{{{Key}, '$1', {'+', '$2', Value}}}]}]
        )
    of
        0 ->
            insert_metric(Registry, Name, LabelValues, Value, fun inc/4);
        1 ->
            ok
    end;
inc(_Registry, _Name, _LabelValues, Value) ->
    erlang:error({invalid_value, Value, "inc accepts only non-negative numbers"}).

%% @equiv remove(default, Name, [])
-spec remove(prometheus_metric:name()) -> boolean().
remove(Name) ->
    remove(default, Name, []).

%% @equiv remove(default, Name, LabelValues)
-spec remove(prometheus_metric:name(), list()) -> boolean().
remove(Name, LabelValues) ->
    remove(default, Name, LabelValues).

%% @doc Removes counter series identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if counter with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
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

%% @equiv reset(default, Name, [])
-spec reset(prometheus_metric:name()) -> boolean().
reset(Name) ->
    reset(default, Name, []).

%% @equiv reset(default, Name, LabelValues)
-spec reset(prometheus_metric:name(), list()) -> boolean().
reset(Name, LabelValues) ->
    reset(default, Name, LabelValues).

%% @doc Resets the value of the counter identified by `Registry', `Name'
%% and `LabelValues'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if counter with name `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
-spec reset(prometheus_registry:registry(), prometheus_metric:name(), list()) -> boolean().
reset(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    case
        lists:usort([
            ets:update_element(
                ?TABLE,
                {Registry, Name, LabelValues, Scheduler},
                [{?ISUM_POS, 0}, {?FSUM_POS, 0}]
            )
         || Scheduler <- schedulers_seq()
        ])
    of
        [_, _] -> true;
        [true] -> true;
        _ -> false
    end.

%% @equiv value(default, Name, [])
-spec value(prometheus_metric:name()) -> number() | undefined.
value(Name) ->
    value(default, Name, []).

%% @equiv value(default, Name, LabelValues)
-spec value(prometheus_metric:name(), list()) -> number() | undefined.
value(Name, LabelValues) ->
    value(default, Name, LabelValues).

%% @doc Returns the value of the counter identified by `Registry', `Name'
%% and `LabelValues'. If there is no counter for `LabelValues',
%% returns `undefined'.
%%
%% Raises `{unknown_metric, Registry, Name}' error if counter named `Name'
%% can't be found in `Registry'.<br/>
%% Raises `{invalid_metric_arity, Present, Expected}' error if labels count
%% mismatch.
%% @end
-spec value(prometheus_registry:registry(), prometheus_metric:name(), list()) ->
    number() | undefined.
value(Registry, Name, LabelValues) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    case
        ets:select(?TABLE, [
            {{{Registry, Name, LabelValues, '_'}, '$1', '$2'}, [], [{'+', '$1', '$2'}]}
        ])
    of
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
            serialize_label_values(
                fun(VLabels, Value) -> {VLabels, Value} end, Labels, LabelValues
            )
    end.

%%====================================================================
%% Collector API
%%====================================================================

%% @private
-spec deregister_cleanup(prometheus_registry:registry()) -> ok.
deregister_cleanup(Registry) ->
    prometheus_metric:deregister_mf(?TABLE, Registry),
    true = ets:match_delete(?TABLE, {{Registry, '_', '_', '_'}, '_', '_'}),
    ok.

%% @private
-spec collect_mf(prometheus_registry:registry(), prometheus_collector:collect_mf_callback()) -> ok.
collect_mf(Registry, Callback) ->
    [
        Callback(create_counter(Name, Help, {CLabels, Labels, Registry}))
     || [Name, {Labels, Help}, CLabels, _, _] <- prometheus_metric:metrics(
            ?TABLE,
            Registry
        )
    ],
    ok.

%% @private
-spec collect_metrics(prometheus_metric:name(), prometheus_collector:collect_mf_callback()) ->
    [prometheus_model:'Metric'()].
collect_metrics(Name, {CLabels, Labels, Registry}) ->
    MFValues = load_all_values(Registry, Name),
    LabelValues = reduce_label_values(MFValues),
    serialize_label_values(
        fun(VLabels, Value) ->
            prometheus_model_helpers:counter_metric(
                CLabels ++ VLabels, Value
            )
        end,
        Labels,
        LabelValues
    ).

%%====================================================================
%% Private Parts
%%====================================================================

deregister_select(Registry, Name) ->
    [{{{Registry, Name, '_', '_'}, '_', '_'}, [], [true]}].

insert_metric(Registry, Name, LabelValues, Value, ConflictCB) ->
    prometheus_metric:check_mf_exists(?TABLE, Registry, Name, LabelValues),
    Counter = {key(Registry, Name, LabelValues), 0, Value},
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
    lists:foldl(
        fun([Labels, I, F], ResAcc) ->
            PrevSum = maps:get(Labels, ResAcc, 0),
            ResAcc#{Labels => PrevSum + I + F}
        end,
        #{},
        MFValues
    ).

serialize_label_values(Fun, Labels, Values) ->
    maps:fold(
        fun(LabelValues, Value, L) ->
            [Fun(lists:zip(Labels, LabelValues), Value) | L]
        end,
        [],
        Values
    ).

create_counter(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, counter, ?MODULE, Data).
