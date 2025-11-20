-module(prometheus_text_format).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Serializes Prometheus registry using the latest [text format](https://bit.ly/2cxSuJP).

Example output:

```text
# TYPE http_request_duration_milliseconds histogram
# HELP http_request_duration_milliseconds Http Request execution time
http_request_duration_milliseconds_bucket{method=\"post\",le=\"100\"} 0
http_request_duration_milliseconds_bucket{method=\"post\",le=\"300\"} 1
http_request_duration_milliseconds_bucket{method=\"post\",le=\"500\"} 3
http_request_duration_milliseconds_bucket{method=\"post\",le=\"750\"} 4
http_request_duration_milliseconds_bucket{method=\"post\",le=\"1000\"} 5
http_request_duration_milliseconds_bucket{method=\"post\",le=\"+Inf\"} 6
http_request_duration_milliseconds_count{method=\"post\"} 6
http_request_duration_milliseconds_sum{method=\"post\"} 4350
```
""").

-export([content_type/0, format/0, format/1, render_labels/1, escape_label_value/1]).

-ifdef(TEST).
-export([escape_metric_help/1]).
-endif.

-include("prometheus_model.hrl").

-behaviour(prometheus_format).
-compile({inline, [render_label_pair/1]}).

?DOC("""
Returns content type of the latest \[text format](https://bit.ly/2cxSuJP).
""").
-spec content_type() -> binary().
content_type() ->
    <<"text/plain; version=0.0.4">>.

?DOC(#{equiv => format(default)}).
?DOC("""
Equivalent to [format(default)](`format/1`).

Formats `default` registry using the latest text format.
""").
-spec format() -> binary().
format() ->
    format(default).

?DOC("Formats `Registry` using the latest text format.").
-spec format(Registry :: prometheus_registry:registry()) -> binary().
format(Registry) ->
    {ok, Fd} = ram_file:open("", [write, read, binary]),
    Callback = fun(_, Collector) ->
        registry_collect_callback(Fd, Registry, Collector)
    end,
    prometheus_registry:collect(Registry, Callback),
    file:write(Fd, "\n"),
    {ok, Size} = ram_file:get_size(Fd),
    {ok, Str} = file:pread(Fd, 0, Size),
    ok = file:close(Fd),
    Str.

?DOC("""
Escapes the backslash (\\), double-quote (\"), and line feed (\\n) characters
""").
-spec escape_label_value(binary() | iolist()) -> binary().
escape_label_value(LValue) when is_binary(LValue) ->
    case has_special_char(LValue) of
        true ->
            escape_string(fun escape_label_char/1, LValue);
        false ->
            LValue
    end;
escape_label_value(LValue) when is_list(LValue) ->
    escape_label_value(iolist_to_binary(LValue));
escape_label_value(Value) ->
    erlang:error({invalid_value, Value}).

registry_collect_callback(Fd, Registry, Collector) ->
    Callback = fun(#'MetricFamily'{name = Name0, help = Help, type = Type, metric = Metrics}) ->
        %% eagerly convert the name to a binary so we can copy more efficiently
        %% in `render_metrics/3`
        Name = iolist_to_binary(Name0),
        Prologue = <<
            "# TYPE ",
            Name/binary,
            " ",
            (string_type(Type))/binary,
            "\n# HELP ",
            Name/binary,
            " ",
            (escape_metric_help(Help))/binary,
            "\n"
        >>,
        %% file:write/2 is an expensive operation, as it goes through a port driver.
        %% Instead a large chunk of bytes is being collected here, in a
        %% way that triggers binary append optimization in ERTS.
        file:write(Fd, render_metrics(Name, Metrics, Prologue))
    end,
    prometheus_collector:collect_mf(Registry, Collector, Callback).

render_metrics(Bytes, _Name, []) ->
    Bytes;
render_metrics(Bytes, Name, [Metric | Rest]) ->
    render_metrics(render_metric(Bytes, Name, Metric), Name, Rest).

render_metric(Bytes0, Name, #'Metric'{label = Labels, counter = #'Counter'{value = Value}}) ->
    render_series(Bytes0, Name, render_labels(Labels), Value);
render_metric(Bytes0, Name, #'Metric'{label = Labels, gauge = #'Gauge'{value = Value}}) ->
    render_series(Bytes0, Name, render_labels(Labels), Value);
render_metric(Bytes0, Name, #'Metric'{label = Labels, untyped = #'Untyped'{value = Value}}) ->
    render_series(Bytes0, Name, render_labels(Labels), Value);
render_metric(Bytes0, Name, #'Metric'{
    label = Labels,
    summary = #'Summary'{
        sample_count = Count,
        sample_sum = Sum,
        quantile = Quantiles
    }
}) ->
    LString = render_labels(Labels),
    Bytes1 = render_series(Bytes0, <<Name/binary, "_count">>, LString, Count),
    Bytes2 = render_series(Bytes1, <<Name/binary, "_sum">>, LString, Sum),
    lists:foldl(
        fun(#'Quantile'{quantile = QN, value = QV}, Blob) ->
            render_series(
                Blob,
                Name,
                render_labels(
                    [
                        LString,
                        #'LabelPair'{
                            name = "quantile",
                            value = io_lib:format("~p", [QN])
                        }
                    ]
                ),
                QV
            )
        end,
        Bytes2,
        Quantiles
    );
render_metric(Bytes0, Name, #'Metric'{
    label = Labels,
    histogram = #'Histogram'{
        sample_count = Count,
        sample_sum = Sum,
        bucket = Buckets
    }
}) ->
    %% StringLabels = labels_stringify(Labels),
    LString = render_labels(Labels),
    Bytes1 = lists:foldl(
        fun(Bucket, Blob) ->
            emit_histogram_bucket(Blob, Name, LString, Bucket)
        end,
        Bytes0,
        Buckets
    ),
    Bytes2 = render_series(Bytes1, <<Name/binary, "_count">>, LString, Count),
    render_series(Bytes2, <<Name/binary, "_sum">>, LString, Sum).

emit_histogram_bucket(Bytes0, Name, LString, #'Bucket'{
    cumulative_count = BCount, upper_bound = BBound
}) ->
    BLValue = bound_to_label_value(BBound),
    render_series(
        Bytes0,
        <<Name/binary, "_bucket">>,
        render_labels([LString, #'LabelPair'{name = "le", value = BLValue}]),
        BCount
    ).

string_type('COUNTER') ->
    <<"counter">>;
string_type('GAUGE') ->
    <<"gauge">>;
string_type('SUMMARY') ->
    <<"summary">>;
string_type('HISTOGRAM') ->
    <<"histogram">>;
string_type('UNTYPED') ->
    <<"untyped">>.

%% binary() in spec means 0 or more already rendered labels (name,
%% escaped value), joined with "," in between
-spec render_labels(binary() | [prometheus_model:'LabelPair'() | binary()]) -> binary().
-dialyzer({no_match, render_labels/1}).
render_labels([]) ->
    <<>>;
%% This clause is the reason for `-dialyzer` attr. It's an
%% optimization, but it slightly violates the types automatically
%% generated from protobufs.
render_labels(B) when is_binary(B) ->
    B;
render_labels([<<>> | Labels]) ->
    render_labels(Labels);
render_labels([FirstLabel | Labels]) ->
    Start = <<(render_label_pair(FirstLabel))/binary>>,
    B = lists:foldl(
        fun
            (<<>>, Acc) ->
                Acc;
            (Label, Acc) ->
                <<Acc/binary, ",", (render_label_pair(Label))/binary>>
        end,
        Start,
        Labels
    ),
    <<B/binary>>.

-spec render_label_pair(prometheus_model:'LabelPair'() | binary()) -> binary().
render_label_pair(B) when is_binary(B) ->
    B;
render_label_pair(#'LabelPair'{name = Name, value = Value}) ->
    <<(iolist_to_binary(Name))/binary, "=\"", (escape_label_value(Value))/binary, "\"">>.

render_series(Bytes, Name, <<>>, Value) ->
    render_value(<<Bytes/binary, Name/binary, " ">>, Value);
render_series(Bytes, Name, LString, Value) ->
    render_value(<<Bytes/binary, Name/binary, "{", LString/binary, "} ">>, Value).

render_value(Bytes, undefined) ->
    <<Bytes/binary, "NaN\n">>;
render_value(Bytes, Value) when is_integer(Value) ->
    <<Bytes/binary, (integer_to_binary(Value))/binary, "\n">>;
render_value(Bytes, Value) when is_float(Value) ->
    %% TODO: should bound_to_label_value/1 and render_value/2 use the same
    %% options in float_to_binary/2? io_lib uses [short] but
    %% bound_to_label_value/1 has always used [{decimals,10},compact].
    <<Bytes/binary, (float_to_binary(Value, [short]))/binary, "\n">>;
render_value(Bytes, Value) ->
    <<Bytes/binary, (iolist_to_binary(io_lib:format("~p", [Value])))/binary, "\n">>.

?DOC(false).
-spec escape_metric_help(iodata()) -> binary().
escape_metric_help(Help) ->
    escape_string(fun escape_help_char/1, Help).

?DOC(false).
escape_help_char($\\ = X) ->
    <<X, X>>;
escape_help_char($\n) ->
    <<$\\, $n>>;
escape_help_char(X) ->
    <<X>>.

bound_to_label_value(Bound) when is_integer(Bound) ->
    integer_to_binary(Bound);
bound_to_label_value(Bound) when is_float(Bound) ->
    float_to_binary(Bound, [{decimals, 10}, compact]);
bound_to_label_value(infinity) ->
    <<"+Inf">>.

?DOC(false).
escape_label_char($\\ = X) ->
    <<X, X>>;
escape_label_char($\n) ->
    <<$\\, $n>>;
escape_label_char($" = X) ->
    <<$\\, X>>;
escape_label_char(X) ->
    <<X>>.

?DOC(false).
-spec has_special_char(binary()) -> boolean().
has_special_char(Subject) when is_binary(Subject) ->
    %% See prometheus_sup:setup_persistent_terms/0.
    %% Pattern checks for backslash, linefeed or double quote characters.
    Pattern = persistent_term:get(prometheus_text_format_escape_pattern),
    binary:match(Subject, Pattern) /= nomatch.

?DOC(false).
escape_string(Fun, Str) when is_binary(Str) ->
    <<<<(Fun(X))/binary>> || <<X:8>> <= Str>>;
escape_string(Fun, Str) ->
    escape_string(Fun, iolist_to_binary(Str)).
