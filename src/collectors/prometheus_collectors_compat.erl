-module(prometheus_collectors_compat).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Compatibility with previous collector metric names.

Up to version 6.0, some default collector metrics did not have `promtool` compliant names.
The issue was fixed in version 6.0. This module is a compatibility layer for the old names.

It is disabled by default, to configure you need to set
```erlang
{prometheus, [
    {collectors_compat, true}
]}
```

This will be supported only for one major version.
""").
?MODULEDOC(#{deprecated => ~"Kept for compatibility and scheduled to be removed on the next major"}).

-export([pre_promtool_compat/1]).
-deprecated([{'_', '_', next_major_release}]).

?DOC(false).
-spec pre_promtool_compat([dynamic()]) -> [dynamic()].
pre_promtool_compat(Metrics) ->
    case is_enabled() of
        true ->
            lists:map(fun pre_promtool_compat_fun/1, Metrics);
        _ ->
            Metrics
    end.

pre_promtool_compat_fun({failed_transactions_total, counter, Help, Fun}) ->
    {failed_transactions, counter, Help, Fun};
pre_promtool_compat_fun({committed_transactions_total, counter, Help, Fun}) ->
    {committed_transactions, counter, Help, Fun};
pre_promtool_compat_fun({logged_transactions_total, counter, Help, Fun}) ->
    {logged_transactions, counter, Help, Fun};
pre_promtool_compat_fun({restarted_transactions_total, counter, Help, Fun}) ->
    {restarted_transactions, counter, Help, Fun};
pre_promtool_compat_fun({atom_bytes, gauge, Help, Value}) ->
    {atom_bytes_total, gauge, Help, Value};
pre_promtool_compat_fun({bytes, gauge, Help, Value}) ->
    {bytes_total, gauge, Help, Value};
pre_promtool_compat_fun({processes_bytes, gauge, Help, Value}) ->
    {processes_bytes_total, gauge, Help, Value};
pre_promtool_compat_fun({system_bytes, gauge, Help, Value}) ->
    {system_bytes_total, gauge, Help, Value};
pre_promtool_compat_fun({context_switches_total, counter, Help, Value}) ->
    {context_switches, counter, Help, Value};
pre_promtool_compat_fun({garbage_collection_number_of_gcs_total, counter, Help, Value}) ->
    {garbage_collection_number_of_gcs, counter, Help, Value};
pre_promtool_compat_fun({garbage_collection_bytes_reclaimed_total, counter, Help, Value}) ->
    {garbage_collection_bytes_reclaimed, counter, Help, Value};
pre_promtool_compat_fun({garbage_collection_words_reclaimed_total, counter, Help, Value}) ->
    {garbage_collection_words_reclaimed, counter, Help, Value};
pre_promtool_compat_fun({runtime_seconds_total, counter, Help, Value}) ->
    {runtime_milliseconds, counter, Help, Value};
pre_promtool_compat_fun({wallclock_time_seconds_total, counter, Help, Value}) ->
    {wallclock_time_milliseconds, counter, Help, Value};
pre_promtool_compat_fun({ports, gauge, Help}) ->
    {port_count, gauge, Help};
pre_promtool_compat_fun({processes, gauge, Help}) ->
    {process_count, gauge, Help};
pre_promtool_compat_fun({atoms, gauge, Help}) ->
    {atom_count, gauge, Help};
pre_promtool_compat_fun(Metric) ->
    Metric.

-spec is_enabled() -> dynamic().
is_enabled() ->
    application:get_env(prometheus, collectors_compat, false).
