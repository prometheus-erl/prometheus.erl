-module(prometheus_metric).
-compile({parse_transform, prometheus_pt}).

-export([
    insert_new_mf/3,
    insert_mf/3,
    deregister_mf/2,
    deregister_mf/3,
    check_mf_exists/3,
    check_mf_exists/4,
    mf_labels/1,
    mf_constant_labels/1,
    mf_duration_unit/1,
    mf_data/1,
    metrics/2,
    remove_labels/4
]).

-export_type([name/0, value/0, labels/0, help/0, duration_unit/0, call_enabled/0, spec/0]).

-type name() :: atom() | binary() | nonempty_string() | iolist().
-type help() :: binary() | nonempty_string().
-type duration_unit() :: prometheus_time:duration_unit().
-type call_enabled() :: boolean().
-type counter_value() :: number().
-type gauge_value() :: number().
-type summary_value() :: {Count :: number(), Sum :: number()}.
-type histogram_value() :: {Buckets :: [number(), ...], Sum :: number()}.
-type value() ::
    counter_value()
    | gauge_value()
    | summary_value()
    | histogram_value()
    | undefined.
-type labels() :: [name()].
-type spec() :: proplists:proplist().

-callback new(Spec :: spec()) -> ok.
-callback declare(Spec :: spec()) -> boolean().
-callback set_default(Registry, Name) -> any() when
    Registry :: prometheus_registry:registry(),
    Name :: name().
-callback remove(Name :: name()) -> boolean() | no_return().
-callback remove(Name :: name(), LValues :: list()) -> boolean() | no_return().
-callback remove(Registry, Name, LValues) -> boolean() | no_return() when
    Registry :: prometheus_registry:registry(),
    Name :: name(),
    LValues :: list().
-callback reset(Name :: name()) -> boolean() | no_return().
-callback reset(Name :: name(), LValues :: list()) -> boolean() | no_return().
-callback reset(Registry, Name, LValues) -> boolean() | no_return() when
    Registry :: prometheus_registry:registry(),
    Name :: name(),
    LValues :: list().
-callback value(Name :: name()) -> value() | no_return().
-callback value(Name :: name(), LValues :: list()) -> value() | no_return().
-callback value(Registry, Name, LValues) -> value() | no_return() when
    Registry :: prometheus_registry:registry(),
    Name :: name(),
    LValues :: list().

-doc false.
-spec insert_new_mf(Table, Module, Spec) -> ok | no_return() when
    Table :: atom(),
    Module :: atom(),
    Spec :: spec().
insert_new_mf(Table, Module, Spec) ->
    case insert_mf(Table, Module, Spec) of
        true ->
            ok;
        false ->
            Registry = prometheus_metric_spec:registry(Spec),
            Name = prometheus_metric_spec:name(Spec),
            erlang:error({mf_already_exists, {Registry, Name}, "Consider using declare instead."})
    end.

-doc false.
-spec insert_mf(Table, Module, Spec) -> boolean() when
    Table :: atom(),
    Module :: atom(),
    Spec :: spec().
insert_mf(Table, Module, Spec) ->
    {Registry, Name, Labels, Help, CLabels, DurationUnit, Data} =
        prometheus_metric_spec:extract_common_params(Spec),
    prometheus_registry:register_collector(Registry, Module),
    Tuple = {{Registry, mf, Name}, {Labels, Help}, CLabels, DurationUnit, Data},
    case ets:insert_new(Table, Tuple) of
        true ->
            maybe_set_default(Module, Registry, Name, Labels),
            true;
        false ->
            false
    end.

-doc false.
-spec deregister_mf(Table, Registry) -> boolean() | no_return() when
    Table :: atom(),
    Registry :: prometheus_registry:registry().
deregister_mf(Table, Registry) ->
    ets:match_delete(Table, {{Registry, mf, '_'}, '_', '_', '_', '_'}).

-doc false.
-spec deregister_mf(Table, Registry, Name) -> boolean() | no_return() when
    Table :: atom(),
    Registry :: prometheus_registry:registry(),
    Name :: name().
deregister_mf(Table, Registry, Name) ->
    case ets:take(Table, {Registry, mf, Name}) of
        [] ->
            false;
        _ ->
            true
    end.

-doc false.
-spec check_mf_exists(Table, Registry, Name, LabelValues) -> any() | no_return() when
    Table :: atom(),
    Registry :: prometheus_registry:registry(),
    Name :: name(),
    LabelValues :: list().
check_mf_exists(Table, Registry, Name, LabelValues) ->
    case ets:lookup(Table, {Registry, mf, Name}) of
        [] ->
            erlang:error({unknown_metric, Registry, Name});
        [{_, {Labels, _}, _, _, _} = MF] ->
            LVLength = length(LabelValues),
            case length(Labels) of
                LVLength ->
                    MF;
                LabelsLength ->
                    erlang:error({invalid_metric_arity, LVLength, LabelsLength})
            end
    end.

-doc false.
-spec check_mf_exists(Table, Registry, Name) -> false | tuple() when
    Table :: atom(),
    Registry :: prometheus_registry:registry(),
    Name :: name().
check_mf_exists(Table, Registry, Name) ->
    case ets:lookup(Table, {Registry, mf, Name}) of
        [] ->
            false;
        [MF] ->
            MF
    end.

-doc false.
-spec mf_labels(tuple()) -> any().
mf_labels(MF) ->
    {Labels, _} = element(2, MF),
    Labels.

-doc false.
-spec mf_constant_labels(tuple()) -> any().
mf_constant_labels(MF) ->
    element(3, MF).

-doc false.
-spec mf_duration_unit(tuple()) -> any().
mf_duration_unit(MF) ->
    element(4, MF).

-doc false.
-spec mf_data(tuple()) -> any().
mf_data(MF) ->
    element(5, MF).

-doc false.
-spec metrics(term(), term()) -> any().
metrics(Table, Registry) ->
    ets:match(Table, {{Registry, mf, '$1'}, '$2', '$3', '$4', '$5'}).

%%====================================================================
%% Private Parts
%%===================================================================

-spec maybe_set_default(Module, Registry, Name, Labels) -> ok when
    Module :: atom(),
    Registry :: prometheus_registry:registry(),
    Name :: name(),
    Labels :: list().
maybe_set_default(Module, Registry, Name, []) ->
    Module:set_default(Registry, Name);
maybe_set_default(_, _, _, _) ->
    ok.

-doc false.
-spec remove_labels(Table, Registry, Name, LValues) -> boolean() | no_return() when
    Table :: atom(),
    Registry :: prometheus_registry:registry(),
    Name :: name(),
    LValues :: list().
remove_labels(Table, Registry, Name, LabelValues) ->
    check_mf_exists(Table, Registry, Name, LabelValues),
    case ets:take(Table, {Registry, Name, LabelValues}) of
        [] -> false;
        _ -> true
    end.
