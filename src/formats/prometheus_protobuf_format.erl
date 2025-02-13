-module(prometheus_protobuf_format).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Serializes Prometheus registry using [protocol buffer format](http://bit.ly/2cxSuJP).
""").

-export([content_type/0, format/0, format/1]).

-behaviour(prometheus_format).

?DOC("Returns content type of the protocol buffer format.").
-spec content_type() -> binary().
content_type() ->
    <<
        "application/vnd.google.protobuf; "
        "proto=io.prometheus.client.MetricFamily; "
        "encoding=delimited"
    >>.

?DOC(#{equiv => format(default)}).
?DOC("Formats `default` registry using protocol buffer format.").
-spec format() -> binary().
format() ->
    format(default).

?DOC("Formats `Registry` using protocol buffer format.").
-spec format(Registry :: prometheus_registry:registry()) -> binary().
format(Registry) ->
    {ok, Fd} = ram_file:open("", [write, read, binary]),
    Callback = fun(_, Collector) ->
        registry_collect_callback(Fd, Registry, Collector)
    end,
    prometheus_registry:collect(Registry, Callback),
    {ok, Size} = ram_file:get_size(Fd),
    {ok, Buf} = file:pread(Fd, 0, Size),
    ok = file:close(Fd),
    Buf.

registry_collect_callback(Fd, Registry, Collector) ->
    Callback = fun(MF) -> file:write(Fd, delimited_encode_mf(MF)) end,
    prometheus_collector:collect_mf(Registry, Collector, Callback).

delimited_encode_mf(MF) ->
    IoRec = prometheus_model:encode_msg(MF),
    Size = iolist_size(IoRec),
    [e_varint(Size, <<>>), IoRec].

e_varint(N, Bin) when N =< 127 -> <<Bin/binary, N>>;
e_varint(N, Bin) ->
    Bin2 = <<Bin/binary, (N band 127 bor 128)>>,
    e_varint(N bsr 7, Bin2).
