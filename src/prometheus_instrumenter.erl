-module(prometheus_instrumenter).
-compile({parse_transform, prometheus_pt}).

-export([enabled_instrumenters/0, setup/1]).

-export_type([instrumenter/0]).

-type instrumenter() :: atom().

-callback setup_instrumenter() -> ok.

-doc false.
-spec enabled_instrumenters() -> [instrumenter()].
enabled_instrumenters() ->
    case application:get_env(prometheus, instrumenters) of
        undefined -> all_known_instrumenters();
        {ok, Instrumenters} -> Instrumenters
    end.

-doc false.
-spec setup(Instrumenter) -> Result when
    Instrumenter :: instrumenter(),
    Result :: ok.
setup(Instrumenter) ->
    ok = Instrumenter:setup_instrumenter().

all_known_instrumenters() ->
    prometheus_misc:behaviour_modules(prometheus_instrumenter).
