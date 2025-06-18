-module(prometheus_buckets).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-export([new/0, new/1, position/2, default/0]).

-export([exponential/3, linear/3, ddsketch/2]).

-type bucket_bound() :: number() | infinity.
-type buckets() :: [bucket_bound(), ...].

?DOC("""
Histogram buckets configuration.

Setting the `buckets` key of a histogram to
- `default`: will use the default buckets
- `linear`: will use `Start`, `Step`, and `Count` to generate the buckets as in `linear/3`
- `exponential`: will use `Start`, `Factor`, and `Count` to generate the buckets as in `exponential/3`

You can also specify your own buckets if desired instead.
""").
-type config() ::
    undefined
    | default
    | {linear, number(), number(), pos_integer()}
    | {exponential, number(), number(), pos_integer()}
    | {ddsketch, float(), pos_integer()}
    | buckets().

-export_type([bucket_bound/0, buckets/0, config/0]).

?DOC("Histogram buckets constructor, returns `default/0` plus `infinity`").
-spec new() -> buckets().
new() ->
    default() ++ [infinity].

?DOC("Histogram buckets constructor").
-spec new(config()) -> buckets().
new([]) ->
    erlang:error({no_buckets, []});
new(undefined) ->
    erlang:error({no_buckets, undefined});
new(default) ->
    default() ++ [infinity];
new({ddsketch, Error, Bound}) ->
    ddsketch(Error, Bound) ++ [infinity];
new({linear, Start, Step, Count}) ->
    linear(Start, Step, Count) ++ [infinity];
new({exponential, Start, Factor, Count}) ->
    exponential(Start, Factor, Count) ++ [infinity];
new(RawBuckets) when is_list(RawBuckets) ->
    Buckets = lists:map(fun validate_bound/1, RawBuckets),
    case lists:sort(Buckets) of
        Buckets ->
            case lists:last(Buckets) of
                infinity -> Buckets;
                _ -> Buckets ++ [infinity]
            end;
        _ ->
            erlang:error({invalid_buckets, Buckets, "buckets not sorted"})
    end;
new(Buckets) ->
    erlang:error({invalid_buckets, Buckets, "not a list"}).

validate_bound(Bound) when is_number(Bound) ->
    Bound;
validate_bound(infinity) ->
    infinity;
validate_bound(Bound) ->
    erlang:error({invalid_bound, Bound}).

?DOC("""
Default histogram buckets.

```erlang
1> prometheus_buckets:default().
[0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]

```

Please note these buckets are floats and represent seconds so you'll have to use
`prometheus_histogramdobserve/3` or configure `duration_unit` as `seconds`.
""").
-spec default() -> buckets().
default() -> [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10].

?DOC("""
Creates preallocated buckets according to the DDSketch algorithm.

For example, if you measure microseconds and you expect no operation to take more than a day,
for a desired error of 1%, 1260 buckets is sufficient.

```erlang
3> prometheus_buckets:ddsketch(0.01, 1260).
['TODO']
```

The function raises `{invalid_value, Value, Message}` error if `Error` isn't positive,
or if `Bound` is less than or equals to 1.
""").
-spec ddsketch(float(), pos_integer()) -> buckets().
ddsketch(Error, _Bound) when Error < 0; 1 < Error ->
    erlang:error({invalid_value, Error, "Buckets error should be a valid percentage point"});
ddsketch(_Error, Bound) when Bound < 1 ->
    erlang:error({invalid_value, Bound, "Buckets count should be positive"});
ddsketch(Error, Bound) ->
    Gamma = (1 + Error) / (1 - Error),
    ddsketch(lists:seq(0, Bound), Gamma, []).

?DOC("""
Creates `Count` buckets, where the lowest bucket has an upper bound of `Start` and each following
bucket's upper bound is `Factor` times the previous bucket's upper bound.
The returned list is meant to be used for the `buckets` key of histogram constructors options.

```erlang
3> prometheus_buckets:exponential(100, 1.2, 3).
[100, 120, 144]
```

The function raises `{invalid_value, Value, Message}` error if `Count` isn't positive,
if `Start` isn't positive, or if `Factor` is less than or equals to 1.
""").
-spec exponential(number(), number(), pos_integer()) -> buckets().
exponential(_Start, _Factor, Count) when Count < 1 ->
    erlang:error({invalid_value, Count, "Buckets count should be positive"});
exponential(Start, _Factor, _Count) when Start =< 0 ->
    erlang:error({invalid_value, Start, "Buckets start should be positive"});
exponential(_Start, Factor, _Count) when Factor =< 1 ->
    erlang:error({invalid_value, Factor, "Buckets factor should be greater than 1"});
exponential(Start, Factor, Count) ->
    [
        try_to_maintain_integer_bounds(Start * math:pow(Factor, I))
     || I <- lists:seq(0, Count - 1)
    ].

?DOC("""
Creates `Count` buckets, each `Width` wide, where the lowest bucket has an upper bound of `Start`.

The returned list is meant to be used for the `buckets` key of histogram constructors options.

```erlang
2> prometheus_buckets:linear(10, 5, 6).
[10, 15, 20, 25, 30, 35]
```

The function raises `{invalid_value, Value, Message}` error if `Count` is zero or negative.
""").
-spec linear(number(), number(), pos_integer()) -> buckets().
linear(_Start, _Step, Count) when Count < 1 ->
    erlang:error({invalid_value, Count, "Buckets count should be positive"});
linear(Start, Step, Count) ->
    linear(Start, Step, Count, []).

?DOC("""
Find the first index that is greater than or equal to the given value.
""").
-spec position(buckets() | tuple(), number()) -> pos_integer().
position(Buckets, Value) when is_list(Buckets), is_number(Value) ->
    find_position(Buckets, Value, 0);
position(Buckets, Value) when is_tuple(Buckets), 1 < tuple_size(Buckets), is_number(Value) ->
    find_position_in_tuple(Buckets, Value, 1, tuple_size(Buckets)).

ddsketch([], _, Acc) ->
    lists:reverse(Acc);
ddsketch([I | Rest], Gamma, Acc) ->
    LowerBound = math:pow(Gamma, I),
    ddsketch(Rest, Gamma, [LowerBound | Acc]).

linear(_Current, _Step, 0, Acc) ->
    lists:reverse(Acc);
linear(Current, Step, Count, Acc) ->
    linear(
        try_to_maintain_integer_bounds(Current + Step),
        Step,
        Count - 1,
        [Current | Acc]
    ).

-spec try_to_maintain_integer_bounds
    (integer()) -> integer();
    (float()) -> integer() | float().
try_to_maintain_integer_bounds(Bound) when is_integer(Bound) -> Bound;
try_to_maintain_integer_bounds(Bound) when is_float(Bound) ->
    TBound = trunc(Bound),
    case TBound == Bound of
        true -> TBound;
        false -> Bound
    end.

%% Find the first index that is greater than or equal to the given value.
-spec find_position(buckets(), number(), non_neg_integer()) -> non_neg_integer().
find_position([], _Value, _Pos) ->
    0;
find_position([Bound | L], Value, Pos) ->
    case Value =< Bound of
        true ->
            Pos;
        false ->
            find_position(L, Value, Pos + 1)
    end.

%% Find the first index that is greater than or equal to the given value.
-spec find_position_in_tuple(tuple(), number(), non_neg_integer(), pos_integer()) ->
    non_neg_integer().
find_position_in_tuple(Tuple, Value, Low, High) when Low =< High ->
    Mid = Low + (High - Low) div 2,
    case element(Mid, Tuple) of
        Element when Element < Value ->
            find_position_in_tuple(Tuple, Value, Mid + 1, High);
        Element when Value =< Element ->
            find_position_in_tuple(Tuple, Value, Low, Mid - 1)
    end;
find_position_in_tuple(Tuple, _Value, Low, _High) ->
    case tuple_size(Tuple) < Low of
        true -> 0;
        false -> Low - 1
    end.
