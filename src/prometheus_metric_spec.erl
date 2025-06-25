-module(prometheus_metric_spec).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-export([
    add_value/3,
    get_value/2,
    get_value/3,
    fetch_value/2,
    registry/1,
    name/1,
    labels/1,
    help/1,
    constant_labels/1,
    duration_unit/1,
    extract_common_params/1
]).

-ifdef(TEST).
-export([
    validate_metric_name/1,
    validate_metric_label_names/1,
    validate_metric_help/1
]).
-endif.

?DOC(false).
-spec registry(Spec :: prometheus_metric:spec()) -> prometheus_registry:registry().
registry(Spec) ->
    get_value(registry, Spec, default).

?DOC(false).
-spec name(Spec :: prometheus_metric:spec()) -> prometheus_metric:name().
name(Spec) ->
    Name = fetch_value(name, Spec),
    validate_metric_name(Name).

?DOC(false).
-spec labels(Spec :: prometheus_metric:spec()) -> prometheus_metric:labels().
labels(Spec) ->
    Labels = get_value(labels, Spec, []),
    validate_metric_label_names(Labels).

?DOC(false).
-spec help(Spec :: prometheus_metric:spec()) -> prometheus_metric:help().
help(Spec) ->
    Help = fetch_value(help, Spec),
    validate_metric_help(Help).

?DOC(false).
-spec data(Spec :: prometheus_metric:spec()) -> dynamic().
data(Spec) ->
    get_value(data, Spec).

?DOC(false).
-spec constant_labels(Spec :: prometheus_metric:spec()) -> [{atom(), term()}].
constant_labels(Spec) ->
    case get_value(constant_labels, Spec, #{}) of
        CL when is_map(CL) ->
            validate_metric_label_names(maps:keys(CL)),
            maps:to_list(CL);
        CL ->
            erlang:error({invalid_value, CL, "constant labels is not a map"})
    end.

?DOC(false).
-spec duration_unit(Spec :: prometheus_metric:spec()) -> prometheus_time:maybe_duration_unit().
duration_unit(Spec) ->
    Name = to_string(name(Spec)),
    NameDU = prometheus_time:duration_unit_from_string(Name),
    case duration_unit_from_spec(Spec) of
        false ->
            undefined;
        undefined ->
            NameDU;
        NameDU ->
            NameDU;
        DU ->
            case NameDU of
                undefined -> DU;
                _ -> erlang:error({invalid_value, DU, "duration unit doesn't match metric name"})
            end
    end.

duration_unit_from_spec(Spec) ->
    SDU = get_value(duration_unit, Spec, undefined),
    prometheus_time:validate_duration_unit(SDU).

?DOC(false).
-spec extract_common_params(Spec :: prometheus_metric:spec()) -> Return when
    Return :: {Registry, Name, Labels, Help, CallTimeout, DurationUnit, Data},
    Registry :: prometheus_registry:registry(),
    Name :: prometheus_metric:name(),
    Labels :: prometheus_metric:labels(),
    Help :: prometheus_metric:help(),
    CallTimeout :: [{atom(), term()}],
    DurationUnit :: prometheus_time:maybe_duration_unit(),
    Data :: dynamic().
extract_common_params(Spec) ->
    Registry = registry(Spec),
    Name = name(Spec),
    Labels = labels(Spec),
    Help = help(Spec),
    Data = data(Spec),
    CallTimeout = constant_labels(Spec),
    DurationUnit = duration_unit(Spec),
    {Registry, Name, Labels, Help, CallTimeout, DurationUnit, Data}.

?DOC(false).
?DOC(#{equiv => get_value(Key, Spec, undefined)}).
-spec add_value(Key :: atom(), Value :: dynamic(), Spec :: prometheus_metric:spec()) ->
    prometheus_metric:spec().
add_value(Key, Value, Spec) when is_list(Spec) ->
    [{Key, Value} | Spec];
add_value(Key, Value, Spec) when is_map(Spec) ->
    Spec#{Key => Value}.

?DOC(false).
?DOC(#{equiv => get_value(Key, Spec, undefined)}).
-spec get_value(Key :: atom(), Spec :: prometheus_metric:spec()) -> dynamic().
get_value(Key, Spec) ->
    get_value(Key, Spec, undefined).

?DOC(false).
-spec get_value(Key :: atom(), Spec :: prometheus_metric:spec(), Default :: dynamic()) -> dynamic().
get_value(Key, Spec, Default) when is_list(Spec) ->
    proplists:get_value(Key, Spec, Default);
get_value(Key, Spec, Default) when is_map(Spec) ->
    maps:get(Key, Spec, Default).

?DOC(false).
-spec fetch_value(Key :: atom(), Spec :: prometheus_metric:spec()) -> dynamic() | no_return().
fetch_value(Key, Spec) when is_list(Spec) ->
    case proplists:get_value(Key, Spec) of
        undefined ->
            erlang:error({missing_metric_spec_key, Key, Spec});
        Value ->
            Value
    end;
fetch_value(Key, Spec) ->
    case maps:get(Key, Spec, undefined) of
        undefined ->
            erlang:error({missing_metric_spec_key, Key, Spec});
        Value ->
            Value
    end.

%%====================================================================
%% Private Parts
%%===================================================================

-spec validate_metric_name
    (atom()) -> atom();
    (string()) -> string();
    (binary()) -> binary().
validate_metric_name(RawName) when is_atom(RawName) ->
    validate_metric_name(RawName, atom_to_list(RawName));
validate_metric_name(RawName) when is_binary(RawName) ->
    validate_metric_name(RawName, binary_to_list(RawName));
validate_metric_name(RawName) when is_list(RawName) ->
    validate_metric_name(RawName, RawName);
validate_metric_name(RawName) ->
    erlang:error({invalid_metric_name, RawName, "metric name is not a string"}).

-spec validate_metric_name
    (atom(), list()) -> atom();
    (string(), list()) -> string();
    (binary(), list()) -> binary().
validate_metric_name(RawName, ListName) ->
    case io_lib:printable_unicode_list(ListName) of
        true ->
            Regex = "^[a-zA-Z_:][a-zA-Z0-9_:]*$",
            case re:run(ListName, Regex) of
                {match, _} ->
                    RawName;
                nomatch ->
                    erlang:error(
                        {invalid_metric_name, RawName, "metric name doesn't match regex " ++ Regex}
                    )
            end;
        false ->
            erlang:error({invalid_metric_name, RawName, "metric name is invalid string"})
    end.

-spec validate_metric_label_names(list()) -> prometheus_metric:labels().
validate_metric_label_names(RawLabels) when is_list(RawLabels) ->
    lists:foreach(fun validate_metric_label_name/1, RawLabels),
    RawLabels;
validate_metric_label_names(RawLabels) ->
    erlang:error({invalid_metric_labels, RawLabels, "not list"}).

validate_metric_label_name(RawName) when is_atom(RawName) ->
    validate_metric_label_name(atom_to_list(RawName));
validate_metric_label_name(RawName) when is_binary(RawName) ->
    validate_metric_label_name(binary_to_list(RawName));
validate_metric_label_name(RawName) when is_list(RawName) ->
    case io_lib:printable_unicode_list(RawName) of
        true ->
            validate_metric_label_name_content(RawName);
        false ->
            erlang:error({invalid_metric_label_name, RawName, "metric label is invalid string"})
    end;
validate_metric_label_name(RawName) ->
    erlang:error({invalid_metric_label_name, RawName, "metric label is not a string"}).

validate_metric_label_name_content("__" ++ _Rest = RawName) ->
    erlang:error({invalid_metric_label_name, RawName, "metric label can't start with __"});
validate_metric_label_name_content(RawName) ->
    Regex = "^[a-zA-Z_][a-zA-Z0-9_]*$",
    case re:run(RawName, Regex) of
        {match, _} ->
            RawName;
        nomatch ->
            erlang:error(
                {invalid_metric_label_name, RawName, "metric label doesn't match regex " ++ Regex}
            )
    end.

-spec validate_metric_help(binary() | string()) -> binary() | string().
validate_metric_help(RawHelp) when is_binary(RawHelp) ->
    iolist_to_binary(validate_metric_help(binary_to_list(RawHelp)));
validate_metric_help(RawHelp) when is_list(RawHelp) ->
    case io_lib:printable_unicode_list(RawHelp) of
        true -> RawHelp;
        false -> erlang:error({invalid_metric_help, RawHelp, "metric help is invalid string"})
    end;
validate_metric_help(RawHelp) ->
    erlang:error({invalid_metric_help, RawHelp, "metric help is not a string"}).

to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(Value) when is_list(Value) ->
    Value.
