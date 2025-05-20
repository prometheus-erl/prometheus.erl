-module(prometheus).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-behaviour(application).

-type label_name() :: term().
-type label_value() :: term().
-type label() :: {label_name(), label_value()}.
-type pre_rendered_labels() :: binary().
-type labels() :: [label()] | #{label_name() => label_value()} | pre_rendered_labels().
-type value() :: float() | integer() | undefined | infinity.
-type prometheus_boolean() :: boolean() | number() | list() | undefined.
-type gauge() :: value() | {value()} | {labels(), value()}.
-type counter() :: value() | {value()} | {labels(), value()}.
-type untyped() :: value() | {value()} | {labels(), value()}.
-type summary() ::
    {non_neg_integer(), value()}
    | {labels(), non_neg_integer(), value()}.
-type buckets() :: nonempty_list({prometheus_buckets:bucket_bound(), non_neg_integer()}).
-type histogram() ::
    {buckets(), non_neg_integer(), value()}
    | {labels(), buckets(), non_neg_integer(), value()}.
-type pbool() ::
    prometheus_boolean()
    | {prometheus_boolean()}
    | {labels(), prometheus_boolean()}.
-type tmetric() ::
    gauge()
    | counter()
    | untyped()
    | summary()
    | histogram()
    | pbool().
-type metrics() :: tmetric() | [tmetric()].

-export_type([
    label/0,
    labels/0,
    label_name/0,
    label_value/0,
    value/0,
    gauge/0,
    counter/0,
    summary/0,
    histogram/0,
    buckets/0,
    metrics/0,
    untyped/0,
    pbool/0,
    prometheus_boolean/0
]).

-export([start/2, stop/1]).
-export([start/0, stop/0]).

-spec start(application:start_type(), term()) -> supervisor:startlink_ret().
start(_StartType, _StartArgs) ->
    prometheus_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

?DOC(false).
-spec start() -> ok | {error, term()}.
start() ->
    application:start(?MODULE).

?DOC(false).
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?MODULE).
