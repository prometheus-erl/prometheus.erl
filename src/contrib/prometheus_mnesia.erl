-module(prometheus_mnesia).
-compile({parse_transform, prometheus_pt}).
-moduledoc "Mnesia instrumentation helpers.".

-export([table_disk_size/1, table_disk_size/2, tm_info/0]).

-define(EXTS, [".DAT", ".TMP", ".DMP", ".DCD", ".DCL", ".LOGTMP"]).

-doc #{equiv => table_disk_size(mnesia:system_info(directory), Table)}.
-spec table_disk_size(Table) -> Size when
    Table :: file:name_all(),
    Size :: non_neg_integer().
table_disk_size(Table) ->
    table_disk_size(mnesia:system_info(directory), Table).

-doc """
Returns sum of all mnesia files for the given `Table` in bytes.
Mnesia can create different files for each table:
- .DAT: DETS files
- .TMP: temp files
- .DMP: dumped ets tables
- .DCD: disc copies data
- .DCL: disc copies log
- .LOGTMP: disc copies log

More on Mnesia files can be found in
[Mnesia System Information chapter](http://erlang.org/doc/apps/mnesia/Mnesia_chap7.html)
of Mnesia User's Guide.
""".
-spec table_disk_size(Dir, Table) -> Size when
    Dir :: file:name_all(),
    Table :: file:name_all(),
    Size :: non_neg_integer().
table_disk_size(Dir, Table) ->
    lists:sum(
        lists:map(
            fun(Ext) ->
                filelib:file_size(table_path(Dir, Table, Ext))
            end,
            ?EXTS
        )
    ).

-doc """
Returns `{PCount, CCount}` tuple, where `PCount` is a number of participant transactions
and `CCount` is a number of coordinator transactions.

Can return `{undefined, undefined}` occasionally.
""".
-spec tm_info() -> {Ps, Cs} | {undefined, undefined} when
    Ps :: non_neg_integer(),
    Cs :: non_neg_integer().
tm_info() ->
    case mnesia_tm:get_info(1000) of
        {info, Ps, Cs} -> {length(Ps), length(Cs)};
        _ -> {undefined, undefined}
    end.

table_path(Dir, Table, Ext) ->
    filename:join(Dir, [Table, Ext]).
