-module(prometheus_instrumenter).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-export([enabled_instrumenters/0, setup/1]).

-export_type([instrumenter/0]).

-type instrumenter() :: atom().

-callback setup_instrumenter() -> ok.

?DOC(false).
-spec enabled_instrumenters() -> [instrumenter()].
enabled_instrumenters() ->
    case application:get_env(prometheus, instrumenters) of
        undefined -> all_known_instrumenters();
        {ok, Instrumenters} -> Instrumenters
    end.

?DOC(false).
-spec setup(Instrumenter :: instrumenter()) -> ok.
setup(Instrumenter) ->
    ok = Instrumenter:setup_instrumenter().

all_known_instrumenters() ->
    prometheus_misc:behaviour_modules(prometheus_instrumenter).
