%% @doc
%%
%% Serializes Prometheus registry using the latest
%% [text format](http://bit.ly/2cxSuJP).
%%
%% Example output:
%% <pre>
%%   # TYPE http_request_duration_milliseconds histogram
%%   # HELP http_request_duration_milliseconds Http Request execution time
%%   http_request_duration_milliseconds_bucket{method="post",le="100"} 0
%%   http_request_duration_milliseconds_bucket{method="post",le="300"} 1
%%   http_request_duration_milliseconds_bucket{method="post",le="500"} 3
%%   http_request_duration_milliseconds_bucket{method="post",le="750"} 4
%%   http_request_duration_milliseconds_bucket{method="post",le="1000"} 5
%%   http_request_duration_milliseconds_bucket{method="post",le="+Inf"} 6
%%   http_request_duration_milliseconds_count{method="post"} 6
%%   http_request_duration_milliseconds_sum{method="post"} 4350
%% </pre>
%% @end

-module(prometheus_text_format).
-export([content_type/0,
         format/0,
         format/1,
         render_labels/1,
         escape_label_value/1
        ]).

-ifdef(TEST).
-export([escape_metric_help/1,
         emit_mf_prologue/2,
         emit_mf_metrics/2
        ]).
-endif.

-include("prometheus_model.hrl").

-behaviour(prometheus_format).
-compile({inline, [add_brackets/1, render_label_pair/1]}).

%%====================================================================
%% Macros
%%====================================================================

%%====================================================================
%% Format API
%%====================================================================

-spec content_type() -> binary().
%% @doc
%% Returns content type of the latest [text format](http://bit.ly/2cxSuJP).
%% @end
content_type() ->
  <<"text/plain; version=0.0.4">>.

%% @equiv format(default)
-spec format() -> binary().
%% @doc
%% Formats `default' registry using the latest text format.
%% @end
format() ->
  format(default).

-spec format(Registry :: prometheus_registry:registry()) -> binary().
%% @doc
%% Formats `Registry' using the latest text format.
%% @end
format(Registry) ->
  {ok, Fd} = ram_file:open("", [write, read, binary]),
  Callback = fun (_, Collector) ->
                 registry_collect_callback(Fd, Registry, Collector)
             end,
  prometheus_registry:collect(Registry, Callback),
  file:write(Fd, "\n"),
  {ok, Size} = ram_file:get_size(Fd),
  {ok, Str} = file:pread(Fd, 0, Size),
  ok = file:close(Fd),
  Str.

-spec escape_label_value(binary() | iolist()) -> binary().
%% @doc
%% Escapes the backslash (\), double-quote ("), and line feed (\n) characters
%% @end
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

%%====================================================================
%% Private Parts
%%====================================================================

registry_collect_callback(Fd, Registry, Collector) ->
  Callback = fun (MF) ->
                 emit_mf_prologue(Fd, MF),
                 emit_mf_metrics(Fd, MF)
             end,
  prometheus_collector:collect_mf(Registry, Collector, Callback).

%% @private
emit_mf_prologue(Fd, #'MetricFamily'{name=Name, help=Help, type=Type}) ->
  Bytes = ["# TYPE ", Name, " ", string_type(Type), "\n# HELP ",
           Name, " ", escape_metric_help(Help), "\n"],
  file:write(Fd, Bytes).

%% @private
emit_mf_metrics(Fd, #'MetricFamily'{name=Name, metric = Metrics}) ->
    %% file:write/2 is an expensive operation, as it goes through a port driver.
    %% Instead a large chunk of bytes is being collected here, in a
    %% way that triggers binary append optimization in ERTS.
    Bytes = lists:foldl(fun (Metric, Blob) ->
                            <<Blob/binary, (render_metric(Name, Metric))/binary>>
                        end, <<>>, Metrics),
    file:write(Fd, Bytes).

render_metric(Name, #'Metric'{label=Labels,
                                counter=#'Counter'{value=Value}}) ->
  render_series(Name, render_labels(Labels), Value);
render_metric(Name, #'Metric'{label=Labels,
                                gauge=#'Gauge'{value=Value}}) ->
  render_series(Name, render_labels(Labels), Value);
render_metric(Name, #'Metric'{label=Labels,
                                untyped=#'Untyped'{value=Value}}) ->
  render_series(Name, render_labels(Labels), Value);
render_metric(Name, #'Metric'{label=Labels,
                                summary=#'Summary'{sample_count=Count,
                                                   sample_sum=Sum,
                                                   quantile=Quantiles}}) ->
  LString = render_labels(Labels),
  Bytes1 = render_series([Name, "_count"], LString, Count),
  Bytes2 = <<Bytes1/binary, (render_series([Name, "_sum"], LString, Sum))/binary>>,
  Bytes3 = lists:foldl(fun (#'Quantile'{quantile = QN, value = QV}, Blob) ->
                           Val = render_series(
                                   [Name],
                                   render_labels(
                                     [LString,
                                      #'LabelPair'{name="quantile",
                                                   value=io_lib:format("~p", [QN])}]),
                                   QV),
                           <<Blob/binary, Val/binary>>
                       end, Bytes2, Quantiles),
  Bytes3;
render_metric(Name, #'Metric'{label=Labels,
                                histogram=#'Histogram'{sample_count=Count,
                                                       sample_sum=Sum,
                                                       bucket=Buckets}}) ->
  %% StringLabels = labels_stringify(Labels),
  LString = render_labels(Labels),
  Bytes1 = lists:foldl(fun (Bucket, Blob) ->
                               <<Blob/binary, (emit_histogram_bucket(Name, LString, Bucket))/binary>>
                       end, << >>, Buckets),
  Bytes2 = <<Bytes1/binary, (render_series([Name, "_count"], LString, Count))/binary>>,
  Bytes3 = <<Bytes2/binary, (render_series([Name, "_sum"], LString, Sum))/binary>>,
  Bytes3.

emit_histogram_bucket(Name, LString, #'Bucket'{cumulative_count=BCount,
                                               upper_bound=BBound}) ->
  BLValue = bound_to_label_value(BBound),
  render_series([Name, "_bucket"],
              render_labels([LString, #'LabelPair'{name="le", value=BLValue}]),
              BCount).

string_type('COUNTER') ->
  "counter";
string_type('GAUGE') ->
  "gauge";
string_type('SUMMARY') ->
  "summary";
string_type('HISTOGRAM') ->
  "histogram";
string_type('UNTYPED') ->
  "untyped".

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
render_labels([<<>>|Labels]) ->
  render_labels(Labels);
render_labels([FirstLabel|Labels]) ->
    Start = << (render_label_pair(FirstLabel))/binary >>,
    B = lists:foldl(fun
                      (<<>>, Acc) ->
                        Acc;
                      (Label, Acc) ->
                        <<Acc/binary, ",", (render_label_pair(Label))/binary>>
                    end, Start, Labels),
    <<B/binary>>.

-spec render_label_pair(prometheus_model:'LabelPair'() | binary()) -> binary().
render_label_pair(B) when is_binary(B) ->
  B;
render_label_pair(#'LabelPair'{name=Name, value=Value}) ->
  << (iolist_to_binary(Name))/binary, "=\"", (escape_label_value(Value))/binary, "\"" >>.

add_brackets(<<>>) ->
  <<>>;
add_brackets(LString) ->
  <<"{", LString/binary, "}">>.

render_series(Name, LString, undefined) ->
  <<(iolist_to_binary(Name))/binary, (add_brackets(LString))/binary, " NaN\n">>;
render_series(Name, LString, Value) when is_integer(Value) ->
  <<(iolist_to_binary(Name))/binary,
    (add_brackets(LString))/binary,
    " ",
    (integer_to_binary(Value))/binary , "\n">>;
render_series(Name, LString, Value) ->
  <<(iolist_to_binary(Name))/binary,
    (add_brackets(LString))/binary,
    " ",
    (iolist_to_binary(io_lib:format("~p", [Value])))/binary , "\n">>.

%% @private
escape_metric_help(Help) ->
  escape_string(fun escape_help_char/1, Help).

%% @private
escape_help_char($\\ = X) ->
  <<X, X>>;
escape_help_char($\n) ->
  <<$\\, $n>>;
escape_help_char(X) ->
  <<X>>.

bound_to_label_value(Bound) when is_integer(Bound) ->
  integer_to_list(Bound);
bound_to_label_value(Bound) when is_float(Bound) ->
  float_to_list(Bound, [{decimals, 10}, compact]);
bound_to_label_value(infinity) ->
  "+Inf".

%% @private
escape_label_char($\\ = X) ->
  <<X, X>>;
escape_label_char($\n) ->
  <<$\\, $n>>;
escape_label_char($" = X) ->
  <<$\\, X>>;
escape_label_char(X) ->
  <<X>>.

%% @perivate
-spec has_special_char(binary()) -> boolean().
has_special_char(<<C:8, _/bitstring>>)
  when C =:= $\\;
       C =:= $\n;
       C =:= $" ->
  true;
has_special_char(<<_:8, Rest/bitstring>>) ->
  has_special_char(Rest);
has_special_char(<<>>) ->
    false.

%% @private
escape_string(Fun, Str) when is_binary(Str) ->
  << <<(Fun(X))/binary>> || <<X:8>> <= Str >>;
escape_string(Fun, Str) ->
  escape_string(Fun, iolist_to_binary(Str)).
