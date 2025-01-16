-module(prometheus_metric_spec).
-compile({parse_transform, prometheus_pt}).
-moduledoc false.

-export([
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

-doc false.
-spec registry(Spec :: prometheus_metric:spec()) -> any().
registry(Spec) ->
    get_value(registry, Spec, default).

-doc false.
-spec name(Spec :: prometheus_metric:spec()) -> any().
name(Spec) ->
    Name = fetch_value(name, Spec),
    validate_metric_name(Name).

-doc false.
-spec labels(Spec :: prometheus_metric:spec()) -> list().
labels(Spec) ->
    Labels = get_value(labels, Spec, []),
    validate_metric_label_names(Labels).

-doc false.
-spec help(Spec :: prometheus_metric:spec()) -> string().
help(Spec) ->
    Help = fetch_value(help, Spec),
    validate_metric_help(Help).

-doc false.
data(Spec) ->
    get_value(data, Spec).

-doc false.
-spec constant_labels(Spec :: prometheus_metric:spec()) -> list().
constant_labels(Spec) ->
    case get_value(constant_labels, Spec, #{}) of
        CL when is_map(CL) ->
            validate_metric_label_names(maps:keys(CL)),
            maps:to_list(CL);
        CL ->
            erlang:error({invalid_value, CL, "constant labels is not a map"})
    end.

-doc false.
-spec duration_unit(Spec :: prometheus_metric:spec()) -> any().
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

-doc false.
-spec extract_common_params(Spec :: prometheus_metric:spec()) ->
    {any(), any(), list(), string(), list(), any(), any()}.
extract_common_params(Spec) ->
    Registry = registry(Spec),
    Name = name(Spec),
    Labels = labels(Spec),
    Help = help(Spec),
    Data = data(Spec),
    CallTimeout = constant_labels(Spec),
    DurationUnit = duration_unit(Spec),
    {Registry, Name, Labels, Help, CallTimeout, DurationUnit, Data}.

-doc false.
-doc #{equiv => get_value(Key, Spec, undefined)}.
-spec get_value(Key :: atom(), Spec :: prometheus_metric:spec()) -> any().
get_value(Key, Spec) ->
    get_value(Key, Spec, undefined).

-doc false.
-spec get_value(Key :: atom(), Spec :: prometheus_metric:spec(), Default :: any()) -> any().
get_value(Key, Spec, Default) ->
    proplists:get_value(Key, Spec, Default).

-doc false.
-spec fetch_value(Key :: atom(), Spec :: prometheus_metric:spec()) -> any() | no_return().
fetch_value(Key, Spec) ->
    case proplists:get_value(Key, Spec) of
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
    (list()) -> list();
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
    (list(), list()) -> list();
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

-spec validate_metric_label_names(list()) -> list().
validate_metric_label_names(RawLabels) when is_list(RawLabels) ->
    lists:map(fun validate_metric_label_name/1, RawLabels);
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
    validate_metric_help(binary_to_list(RawHelp));
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
