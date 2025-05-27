-module(prometheus_registry).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
A registry of Collectors.

The majority of users should use the `default`, rather than their own.

Creating a registry other than the default is primarily useful for unit tests,
or pushing a subset of metrics to the [Pushgateway](https://github.com/prometheus/pushgateway)
from batch jobs.
""").

-export([
    collect/2,
    collectors/1,
    exists/1,
    register_collector/1,
    register_collector/2,
    register_collectors/1,
    register_collectors/2,
    deregister_collector/1,
    deregister_collector/2,
    clear/0,
    clear/1,
    collector_registeredp/1,
    collector_registeredp/2
]).

-export_type([
    registry/0,
    collect_callback/0
]).

-include("prometheus.hrl").

-type registry() :: atom().

-type collect_callback() ::
    fun((registry(), prometheus_collector:collector()) -> dynamic()).

-define(TABLE, ?PROMETHEUS_REGISTRY_TABLE).

?DOC("""
Tries to find registry with the `Name`.

Assumes that registry name is always an atom.
- If `Name` is an atom `ets:lookup/2` is used
- If `Name` is an iolist performs safe search (to avoid interning atoms) and returns atom or false.

This operation is O(n).
""").
-spec exists(Name) -> Result when
    Name :: atom() | iolist(),
    Result :: boolean() | atom().
exists(Name) when is_atom(Name) ->
    case ets:lookup(?PROMETHEUS_REGISTRY_TABLE, Name) of
        [] -> false;
        _ -> true
    end;
exists(Name) when is_list(Name) orelse is_binary(Name) ->
    try binary_to_existing_atom(iolist_to_binary(Name), utf8) of
        Atom ->
            case exists(Atom) of
                true -> Atom;
                false -> false
            end
    catch
        error:badarg ->
            false
    end.

?DOC("Calls `Callback` for each collector with two arguments: `Registry` and `Collector`.").
-spec collect(Registry, Callback) -> ok when
    Registry :: prometheus_registry:registry(),
    Callback :: collect_callback().
collect(Registry, Callback) ->
    [
        Callback(Registry, Collector)
     || {_, Collector} <- ets:lookup(?TABLE, Registry)
    ],
    ok.

?DOC("Returns collectors registered in `Registry`.").
-spec collectors(Registry :: prometheus_registry:registry()) ->
    [Collector :: prometheus_collector:collector()].
collectors(Registry) ->
    [Collector || {_, Collector} <- ets:lookup(?TABLE, Registry)].

?DOC(#{equiv => register_collector(default, Collector)}).
-spec register_collector(Collector :: prometheus_collector:collector()) -> ok.
register_collector(Collector) ->
    register_collector(default, Collector).

?DOC("Register a collector.").
-spec register_collector(Registry, Collector) -> ok when
    Registry :: prometheus_registry:registry(),
    Collector :: prometheus_collector:collector().
register_collector(Registry, Collector) ->
    ets:insert(?TABLE, {Registry, Collector}),
    ok.

-spec register_collectors(Collectors :: [prometheus_collector:collector()]) ->
    ok.
?DOC(#{equiv => register_collectors(default, Collectors)}).
register_collectors(Collectors) ->
    register_collectors(default, Collectors).

?DOC("Registers collectors list.").
-spec register_collectors(Registry, Collectors) -> ok when
    Registry :: prometheus_registry:registry(),
    Collectors :: [prometheus_collector:collector()].
register_collectors(Registry, Collectors) ->
    [register_collector(Registry, Collector) || Collector <- Collectors],
    ok.

?DOC(#{equiv => deregister_collector(default, Collector)}).
-spec deregister_collector(Collector :: prometheus_collector:collector()) -> ok.
deregister_collector(Collector) ->
    deregister_collector(default, Collector).

?DOC("Unregisters a collector.").
-spec deregister_collector(Registry, Collector) -> ok when
    Registry :: prometheus_registry:registry(),
    Collector :: prometheus_collector:collector().
deregister_collector(Registry, Collector) ->
    ets:delete_object(?TABLE, {Registry, Collector}),
    Collector:deregister_cleanup(Registry),
    ok.

?DOC(#{equiv => clear(default)}).
-spec clear() -> ok.
clear() ->
    clear(default).

?DOC("Unregisters all collectors.").
-spec clear(Registry :: prometheus_registry:registry()) -> ok.
clear(Registry) ->
    [
        Collector:deregister_cleanup(Registry)
     || {_, Collector} <- ets:take(?TABLE, Registry)
    ],
    ok.

?DOC(#{equiv => collector_registeredp(default, Collector)}).
-spec collector_registeredp(Collector) -> boolean() when
    Collector :: prometheus_collector:collector().
collector_registeredp(Collector) ->
    collector_registeredp(default, Collector).

?DOC("Checks whether `Collector` is registered.").
-spec collector_registeredp(Registry, Collector) -> boolean() when
    Registry :: prometheus_registry:registry(),
    Collector :: prometheus_collector:collector().
collector_registeredp(Registry, Collector) ->
    case ets:match(?TABLE, {Registry, Collector}) of
        [] -> false;
        _ -> true
    end.
