-module(prometheus_model_helpers).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Helpers for working with Prometheus data model. For advanced users.

Probably will be used with `m:prometheus_collector`.
""").

-export([
    metric_name/1,
    create_mf/4,
    create_mf/5,
    gauge_metrics/1,
    gauge_metric/1,
    gauge_metric/2,
    untyped_metrics/1,
    untyped_metric/1,
    untyped_metric/2,
    boolean_metrics/1,
    boolean_metric/1,
    boolean_metric/2,
    boolean_value/1,
    counter_metrics/1,
    counter_metric/1,
    counter_metric/2,
    summary_metrics/1,
    summary_metric/1,
    summary_metric/2,
    summary_metric/3,
    summary_metric/4,
    histogram_metrics/1,
    histogram_metric/1,
    histogram_metric/3,
    histogram_metric/4,
    label_pairs/1,
    label_pair/1
]).

-ifdef(TEST).
-export([
    filter_undefined_metrics/1,
    ensure_mf_type/1,
    ensure_binary_or_string/1
]).
-endif.

-include("prometheus_model.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

?DOC("""
If `Name` is a list, looks for atoms and converts them to binaries. Why iolists do not support atoms?
""").
-spec metric_name(Name) -> binary() when
    Name :: atom() | binary() | list(char() | iolist() | binary() | atom()).
metric_name(Name) ->
    case Name of
        _ when is_atom(Name) ->
            atom_to_binary(Name, utf8);
        _ when is_list(Name) ->
            lists:foldl(
                fun
                    (A, Acc) when is_atom(A) ->
                        <<Acc/binary, (atom_to_binary(A, utf8))/binary>>;
                    (C, Acc) when is_integer(C) ->
                        <<Acc/binary, C:8>>;
                    (Str, Acc) ->
                        <<Acc/binary, (iolist_to_binary(Str))/binary>>
                end,
                <<>>,
                Name
            );
        _ when is_binary(Name) ->
            Name
    end.

?DOC("""
Create Metric Family of `Type`, `Name` and `Help`.

`Collector:collect_metrics/2` callback will be called and expected to return individual metrics list.
""").
-spec create_mf(Name, Help, Type, Metrics) -> MetricFamily when
    Name :: prometheus_metric:name(),
    Help :: prometheus_metric:help(),
    Type :: atom(),
    Metrics ::
        [prometheus_model:'Metric'()]
        | prometheus_model:'Metric'()
        | prometheus:metrics(),
    MetricFamily :: prometheus_model:'MetricFamily'().
create_mf(Name, Help, Type, Metrics0) ->
    Metrics = metrics_from_tuples(Type, Metrics0),
    #'MetricFamily'{
        name = ensure_binary_or_string(Name),
        help = ensure_binary_or_string(Help),
        type = ensure_mf_type(Type),
        metric = Metrics
    }.

?DOC("""
Create Metric Family of `Type`, `Name` and `Help`.

`Collector:collect_metrics/2` callback will be called and expected to return individual metrics list.
""").
-spec create_mf(Name, Help, Type, Collector, CollectorData) -> MetricFamily when
    Name :: prometheus_metric:name(),
    Help :: prometheus_metric:help(),
    Type :: atom(),
    Collector :: prometheus_collector:collector(),
    CollectorData :: prometheus_collector:data(),
    MetricFamily :: prometheus_model:'MetricFamily'().
create_mf(Name, Help, Type, Collector, CollectorData) ->
    create_mf(Name, Help, Type, Collector:collect_metrics(Name, CollectorData)).

?DOC(#{equiv => lists:map(fun gauge_metrics/1, Values)}).
-spec gauge_metrics(Values) -> [prometheus_model:'Metric'()] when
    Values :: [prometheus:gauge()].
gauge_metrics(Values) ->
    lists:map(fun gauge_metric/1, Values).

?DOC(#{equiv => gauge_metric(Labels, Value)}).
-spec gauge_metric(Gauge) -> prometheus_model:'Metric'() when
    Gauge :: prometheus:gauge().
gauge_metric({Labels, Value}) -> gauge_metric(Labels, Value);
gauge_metric({Value}) -> gauge_metric([], Value);
gauge_metric(Value) -> gauge_metric([], Value).

?DOC("Creates gauge metric with `Labels` and `Value`.").
-spec gauge_metric(Labels, Value) -> prometheus_model:'Metric'() when
    Labels :: prometheus:labels(),
    Value :: prometheus:value().
gauge_metric(Labels, Value) ->
    #'Metric'{
        label = label_pairs(Labels),
        gauge = #'Gauge'{value = Value}
    }.

?DOC(#{equiv => lists:map(fun untyped_metric/1, Values)}).
-spec untyped_metrics(Values) -> [prometheus_model:'Metric'()] when
    Values :: [prometheus:untyped()].
untyped_metrics(Values) ->
    lists:map(fun untyped_metric/1, Values).

?DOC(#{equiv => untyped_metric(Labels, Value)}).
-spec untyped_metric(Untyped) -> prometheus_model:'Metric'() when
    Untyped :: prometheus:untyped().
untyped_metric({Labels, Value}) -> untyped_metric(Labels, Value);
untyped_metric({Value}) -> untyped_metric([], Value);
untyped_metric(Value) -> untyped_metric([], Value).

?DOC("Creates untyped metric with `Labels` and `Value`.").
-spec untyped_metric(Labels, Value) -> prometheus_model:'Metric'() when
    Labels :: prometheus:labels(),
    Value :: prometheus:value().
untyped_metric(Labels, Value) ->
    #'Metric'{
        label = label_pairs(Labels),
        untyped = #'Untyped'{value = Value}
    }.

?DOC(#{equiv => lists:map(fun boolean_metric/1, Values)}).
-spec boolean_metrics(Values) -> [prometheus_model:'Metric'()] when
    Values :: [prometheus:pbool()].
boolean_metrics(Values) ->
    lists:map(fun boolean_metric/1, Values).

?DOC(#{equiv => boolean_metric(Labels, Value)}).
-spec boolean_metric(Boolean) -> prometheus_model:'Metric'() when
    Boolean :: prometheus:pbool().
boolean_metric({Labels, Value}) -> boolean_metric(Labels, Value);
boolean_metric({Value}) -> boolean_metric([], Value);
boolean_metric(Value) -> boolean_metric([], Value).

?DOC("Creates boolean metric with `Labels` and `Value`.").
-spec boolean_metric(Labels, Value) -> prometheus_model:'Metric'() when
    Labels :: prometheus:labels(),
    Value :: prometheus:prometheus_boolean().
boolean_metric(Labels, Value0) ->
    Value = boolean_value(Value0),
    untyped_metric(Labels, Value).

?DOC(false).
-spec boolean_value(Value) -> RealValue when
    Value :: prometheus:prometheus_boolean(),
    RealValue :: undefined | 0 | 1.
boolean_value(true) -> 1;
boolean_value(false) -> 0;
boolean_value(undefined) -> undefined;
boolean_value(1) -> 1;
boolean_value(0) -> 0;
boolean_value([]) -> 0;
boolean_value([_ | _]) -> 1;
boolean_value(Value) when is_number(Value), Value > 0 -> 1;
boolean_value(Value) -> erlang:error({invalid_value, Value, "value is not boolean"}).

?DOC(#{equiv => lists:map(fun counter_metric/1, Values)}).
-spec counter_metrics(Specs) -> [prometheus_model:'Metric'()] when
    Specs :: [prometheus:counter()].
counter_metrics(Specs) ->
    lists:map(fun counter_metric/1, Specs).

?DOC(#{equiv => counter_metric(Labels, Value)}).
-spec counter_metric(Spec) -> prometheus_model:'Metric'() when
    Spec :: prometheus:counter().
counter_metric({Labels, Value}) -> counter_metric(Labels, Value);
counter_metric({Value}) -> counter_metric([], Value);
counter_metric(Value) -> counter_metric([], Value).

?DOC("Creates counter metric with `Labels` and `Value`.").
-spec counter_metric(Labels, Value) -> prometheus_model:'Metric'() when
    Labels :: prometheus:labels(),
    Value :: prometheus:value().
counter_metric(Labels, Value) ->
    #'Metric'{
        label = label_pairs(Labels),
        counter = #'Counter'{value = Value}
    }.

?DOC(#{equiv => lists:map(fun summary_metric/1, Specs)}).
-spec summary_metrics(Specs) -> [prometheus_model:'Metric'()] when
    Specs :: [prometheus:summary()].
summary_metrics(Specs) ->
    lists:map(fun summary_metric/1, Specs).

?DOC(#{equiv => summary_metric(Labels, Count, Sum)}).
-spec summary_metric(Summary) -> prometheus_model:'Metric'() when
    Summary :: prometheus:summary().
summary_metric({Labels, Count, Sum, Quantiles}) when is_list(Quantiles) ->
    summary_metric(Labels, Count, Sum, Quantiles);
summary_metric({Count, Sum, Quantiles}) when is_list(Quantiles) ->
    summary_metric([], Count, Sum, Quantiles);
summary_metric({Labels, Count, Sum}) ->
    summary_metric(Labels, Count, Sum);
summary_metric({Count, Sum}) ->
    summary_metric([], Count, Sum).

?DOC(#{equiv => summary_metric([], Count, Sum)}).
-spec summary_metric(Count, Sum) -> prometheus_model:'Metric'() when
    Count :: non_neg_integer(),
    Sum :: prometheus:value().
summary_metric(Count, Sum) ->
    summary_metric([], Count, Sum).

?DOC(#{equiv => summary_metric([], Count, Sum, [])}).
-spec summary_metric(Labels, Count, Sum) -> prometheus_model:'Metric'() when
    Labels :: prometheus:labels(),
    Count :: non_neg_integer(),
    Sum :: prometheus:value().
summary_metric(Labels, Count, Sum) ->
    summary_metric(Labels, Count, Sum, []).

?DOC("Creates summary metric with `Labels`, `Count` and `Sum`.").
-spec summary_metric(Labels, Count, Sum, Quantiles) -> prometheus_model:'Metric'() when
    Labels :: prometheus:labels(),
    Count :: non_neg_integer(),
    Sum :: prometheus:value(),
    Quantiles :: list().
summary_metric(Labels, Count, Sum, Quantiles) ->
    #'Metric'{
        label = label_pairs(Labels),
        summary = #'Summary'{
            sample_count = Count,
            sample_sum = Sum,
            quantile = [#'Quantile'{quantile = QN, value = QV} || {QN, QV} <- Quantiles]
        }
    }.

?DOC(#{equiv => lists:map(fun histogram_metric/1, Specs)}).
-spec histogram_metrics(Specs) -> [prometheus_model:'Metric'()] when
    Specs :: [prometheus:histogram()].
histogram_metrics(Specs) ->
    lists:map(fun histogram_metric/1, Specs).

?DOC(#{equiv => histogram_metric(Labels, Buckets, Count, Sum)}).
-spec histogram_metric(Histogram) -> prometheus_model:'Metric'() when
    Histogram :: prometheus:histogram().
histogram_metric({Labels, Buckets, Count, Sum}) ->
    histogram_metric(Labels, Buckets, Count, Sum);
histogram_metric({Buckets, Count, Sum}) ->
    histogram_metric([], Buckets, Count, Sum).

?DOC(#{equiv => histogram_metric([], Buckets, Count, Sum)}).
-spec histogram_metric(Buckets, Count, Sum) -> prometheus_model:'Metric'() when
    Buckets :: prometheus:buckets(),
    Count :: non_neg_integer(),
    Sum :: prometheus:value().
histogram_metric(Buckets, Count, Sum) ->
    histogram_metric([], Buckets, Count, Sum).

?DOC("Creates histogram metric with `Labels`, `Buckets`, `Count` and `Sum`.").
-spec histogram_metric(Labels, Buckets, Count, Sum) -> Metric when
    Labels :: prometheus:labels(),
    Buckets :: [{Bound, Count}],
    Bound :: prometheus_buckets:bucket_bound(),
    Count :: non_neg_integer(),
    Sum :: prometheus:value(),
    Metric :: prometheus_model:'Metric'().
histogram_metric(Labels, Buckets, Count, Sum) ->
    Label = label_pairs(Labels),
    Bucket = histogram_buckets(Buckets),
    #'Metric'{
        label = Label,
        histogram = #'Histogram'{
            sample_count = Count,
            sample_sum = Sum,
            bucket = Bucket
        }
    }.

?DOC("""
Label pairs sequentially.

NB `is_binary' clause here is for a special optimization for text
format only: client code can pre-generate final labels string,
e.g. when it knows when character escaping is not needed. This
avoids direct performance cost of character escaping, and also
reduces garabage collection pressure, as intermediate lists of
tuples/records are not created at all. This optimization is used by
RabbitMQ prometheus plugin (which calls `create_mf/5', and it ends
here).

WARNING Works only for text format, protobuf format export will
fail with an error.
""").
?DOC(#{equiv => lists:map(fun label_pair/1, Labels)}).
-spec label_pairs(Labels) -> [prometheus_model:'LabelPair'()] when
    Labels :: prometheus:labels().
label_pairs(B) when is_binary(B) ->
    B;
label_pairs(Labels) when is_list(Labels) ->
    lists:map(fun label_pair/1, Labels);
label_pairs(Labels) when is_map(Labels) ->
    lists:map(fun label_pair/1, maps:to_list(Labels)).

?DOC("""
Creates `prometheus_model:`LabelPair'()' from \{Name, Value\} tuple.
""").
-spec label_pair(prometheus:label()) -> prometheus_model:'LabelPair'().
label_pair({Name, Value}) ->
    #'LabelPair'{
        name = ensure_binary_or_string(Name),
        value = ensure_binary_or_string(Value)
    }.

%%%===================================================================
%%% Private Parts
%%%===================================================================

?DOC(#{equiv => lists:map(fun histogram_bucket/1, Specs)}).
histogram_buckets(Specs) ->
    lists:map(fun histogram_bucket/1, Specs).

-spec histogram_bucket({Bound, Count}) -> Buckets when
    Bound :: prometheus_buckets:bucket_bound(),
    Count :: non_neg_integer(),
    Buckets :: prometheus_model:'Bucket'().
histogram_bucket({Bound, Count}) ->
    #'Bucket'{
        upper_bound = Bound,
        cumulative_count = Count
    }.

metrics_from_tuples(Type, Metrics) ->
    [
        metric_from_tuple(Type, Metric)
     || Metric <- filter_undefined_metrics(ensure_list(Metrics))
    ].

metric_from_tuple(_, Metric) when is_record(Metric, 'Metric') ->
    Metric;
metric_from_tuple(gauge, Metric) ->
    gauge_metric(Metric);
metric_from_tuple(counter, Metric) ->
    counter_metric(Metric);
metric_from_tuple(boolean, Metric) ->
    boolean_metric(Metric);
metric_from_tuple(summary, Metric) ->
    summary_metric(Metric);
metric_from_tuple(histogram, Metric) ->
    histogram_metric(Metric);
metric_from_tuple(untyped, Metric) ->
    untyped_metric(Metric).

-spec ensure_list(Val :: term()) -> list().
ensure_list(Val) when is_list(Val) -> Val;
ensure_list(Val) -> [Val].

?DOC(false).
-spec filter_undefined_metrics([undefined | T]) -> [T].
filter_undefined_metrics(Metrics) ->
    lists:filter(fun not_undefined/1, Metrics).

not_undefined(undefined) -> false;
not_undefined(_) -> true.

?DOC(false).
-spec ensure_binary_or_string(Val :: term()) -> binary() | string().
ensure_binary_or_string(Val) when is_atom(Val) -> atom_to_binary(Val, utf8);
%% FIXME: validate utf8
ensure_binary_or_string(Val) when is_list(Val) -> Val;
ensure_binary_or_string(Val) when is_binary(Val) -> Val;
ensure_binary_or_string(Val) -> io_lib:format("~p", [Val]).

?DOC(false).
-spec ensure_mf_type(atom()) -> atom().
ensure_mf_type(gauge) -> 'GAUGE';
ensure_mf_type(counter) -> 'COUNTER';
ensure_mf_type(summary) -> 'SUMMARY';
ensure_mf_type(histogram) -> 'HISTOGRAM';
ensure_mf_type(untyped) -> 'UNTYPED';
ensure_mf_type(boolean) -> 'UNTYPED';
ensure_mf_type(Type) -> erlang:error({invalid_metric_type, Type}).
