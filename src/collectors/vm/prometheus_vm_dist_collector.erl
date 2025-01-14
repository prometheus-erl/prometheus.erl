%% @doc
%% Collects information about the sockets and processes involved
%% in the Erlang distribution mechanism.
%%
%% All metrics include a label 'peer' that indicates which
%% distributed connection the metric is about.
%%
%% ==Exported metrics==
%%
%% Metrics pertaining to processes may apply to three different types
%% of processes depending on the distribution transport:
%% `type="dist"', `type="tls_connection"' or `type="tls_sender"'.
%%
%% <ul>
%%   <li>
%%     `erlang_vm_dist_recv_bytes'<br/>
%%     Type: gauge.<br/>
%%     Number of bytes received by the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_recv_cnt'<br/>
%%     Type: gauge.<br/>
%%     Number of packets received by the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_recv_max_bytes'<br/>
%%     Type: gauge.<br/>
%%     Size of the largest packet, in bytes, received by the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_recv_avg_bytes'<br/>
%%     Type: gauge.<br/>
%%     Average size of packets, in bytes, received by the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_recv_dvi_bytes'<br/>
%%     Type: gauge.<br/>
%%     Average packet size deviation, in bytes, received by the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_send_bytes'<br/>
%%     Type: gauge.<br/>
%%     Number of bytes sent from the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_send_cnt'<br/>
%%     Type: gauge.<br/>
%%     Number of packets sent from the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_send_max_bytes'<br/>
%%     Type: gauge.<br/>
%%     Size of the largest packet, in bytes, sent from the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_send_avg_bytes'<br/>
%%     Type: gauge.<br/>
%%     Average size of packets, in bytes, sent from the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_send_pend_bytes'<br/>
%%     Type: gauge.<br/>
%%     Number of bytes waiting to be sent by the socket.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_port_input_bytes'<br/>
%%     Type: gauge.<br/>
%%     The total number of bytes read from the port.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_port_output_bytes'<br/>
%%     Type: gauge.<br/>
%%     The total number of bytes written to the port.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_port_memory_bytes'<br/>
%%     Type: gauge.<br/>
%%     The total number of bytes allocated for this port by the runtime system.
%%     The port itself can have allocated memory that is not included.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_port_queue_size_bytes'<br/>
%%     Type: gauge.<br/>
%%     The total number of bytes queued by the port using the ERTS driver queue implementation.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_memory_bytes'<br/>
%%     Type: gauge.<br/>
%%     The size in bytes of the process. This includes call stack, heap, and internal structures.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_heap_size_words'<br/>
%%     Type: gauge.<br/>
%%     The size in words of the youngest heap generation of the process.
%%     This generation includes the process stack. This information is
%%     highly implementation-dependent, and can change if the implementation changes.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_min_heap_size_words'<br/>
%%     Type: gauge.<br/>
%%     The minimum heap size for the process.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_min_bin_vheap_size_words'<br/>
%%     Type: gauge.<br/>
%%     The minimum binary virtual heap size for the process.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_stack_size_words'<br/>
%%     Type: gauge.<br/>
%%     The stack size, in words, of the process.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_total_heap_size_words'<br/>
%%     Type: gauge.<br/>
%%     The total size, in words, of all heap fragments of the process.
%%     This includes the process stack and any unreceived messages that
%%     are considered to be part of the heap.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_message_queue_len'<br/>
%%     Type: gauge.<br/>
%%     The number of messages currently in the message queue of the process.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_reductions'<br/>
%%     Type: gauge.<br/>
%%     The number of reductions executed by the process.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_proc_status'<br/>
%%     Type: gauge.<br/>
%%     The current status of the distribution process.<br/>
%%     The status is represented as a numerical value where `exiting=1',
%%     `suspended=2', `runnable=3', `garbage_collecting=4', `running=5'
%%     and `waiting=6'.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_node_state'<br/>
%%     Type: gauge.<br/>
%%     The current state of the distribution link.<br/>
%%     The state is represented as a numerical value where `pending=1',
%%     `up_pending=2' and `up=3'.
%%   </li>
%%   <li>
%%     `erlang_vm_dist_node_queue_size_bytes'<br/>
%%     Type: gauge.<br/>
%%     The number of bytes in the output distribution queue.<br/>
%%     This queue sits between the Erlang code and the port driver.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Metrics exported by this collector can be configured via
%% `vm_dist_collector_metrics' key of `prometheus' app environment.
%%
%% Available options:
%% <ul>
%%   <li>
%%     `recv_bytes' for `erlang_vm_dist_recv_bytes'.
%%   </li>
%%   <li>
%%     `recv_cnt' for `erlang_vm_dist_recv_cnt'.
%%   </li>
%%   <li>
%%     `recv_max_bytes' for `erlang_vm_dist_recv_max_bytes'.
%%   </li>
%%   <li>
%%     `recv_avg_bytes' for `erlang_vm_dist_recv_avg_bytes'.
%%   </li>
%%   <li>
%%     `recv_dvi_bytes' for `erlang_vm_dist_recv_dvi_bytes'.
%%   </li>
%%   <li>
%%     `send_bytes' for `erlang_vm_dist_send_bytes'.
%%   </li>
%%   <li>
%%     `send_cnt' for `erlang_vm_dist_send_cnt'.
%%   </li>
%%   <li>
%%     `send_max_bytes' for `erlang_vm_dist_send_max_bytes'.
%%   </li>
%%   <li>
%%     `send_avg_bytes' for `erlang_vm_dist_send_avg_bytes'.
%%   </li>
%%   <li>
%%     `send_pend_bytes' for `erlang_vm_dist_send_pend_bytes'.
%%   </li>
%%   <li>
%%     `port_input_bytes' for `erlang_vm_dist_port_input_bytes'.
%%   </li>
%%   <li>
%%     `port_output_bytes' for `erlang_vm_dist_port_output_bytes'.
%%   </li>
%%   <li>
%%     `port_memory_bytes' for `erlang_vm_dist_port_memory_bytes'.
%%   </li>
%%   <li>
%%     `port_queue_size_bytes' for `erlang_vm_dist_port_queue_size_bytes'.
%%   </li>
%%   <li>
%%     `proc_memory_bytes' for `erlang_vm_dist_proc_memory_bytes'.
%%   </li>
%%   <li>
%%     `proc_heap_size_words' for `erlang_vm_dist_proc_heap_size_words'.
%%   </li>
%%   <li>
%%     `proc_min_heap_size_words' for `erlang_vm_dist_proc_min_heap_size_words'.
%%   </li>
%%   <li>
%%     `proc_min_bin_vheap_size_words' for `erlang_vm_dist_proc_min_bin_vheap_size_words'.
%%   </li>
%%   <li>
%%     `proc_stack_size_words' for `erlang_vm_dist_proc_stack_size_words'.
%%   </li>
%%   <li>
%%     `proc_total_heap_size_words' for `erlang_vm_dist_proc_total_heap_size_words'.
%%   </li>
%%   <li>
%%     `proc_message_queue_len' for `erlang_vm_dist_proc_message_queue_len'.
%%   </li>
%%   <li>
%%     `proc_reductions' for `erlang_vm_dist_proc_reductions'.
%%   </li>
%%   <li>
%%     `proc_status' for `erlang_vm_dist_proc_status'.
%%   </li>
%%   <li>
%%     `node_state' for `erlang_vm_dist_node_state'.
%%   </li>
%%   <li>
%%     `node_queue_size_bytes' for `erlang_vm_dist_node_queue_size_bytes'.
%%   </li>
%% </ul>
%%
%% By default all metrics are enabled.
%% @end

-module(prometheus_vm_dist_collector).

-export([
    deregister_cleanup/1,
    collect_mf/2
]).

-import(prometheus_model_helpers, [create_mf/4]).

-include("prometheus.hrl").

-behaviour(prometheus_collector).

-dialyzer({nowarn_function, node_queue_size/1}).

%%====================================================================
%% Macros
%%====================================================================

-define(METRIC_NAME_PREFIX, "erlang_vm_dist_").

%%====================================================================
%% Collector API
%%====================================================================

%% @private
deregister_cleanup(_) -> ok.

-spec collect_mf(_Registry, Callback) -> ok when
    _Registry :: prometheus_registry:registry(),
    Callback :: prometheus_collector:callback().
%% @private
collect_mf(_Registry, Callback) ->
    Metrics = metrics(),
    EnabledMetrics = enabled_metrics(),
    [
        add_metric_family(Metric, Callback)
     || {Name, _, _, _} = Metric <- Metrics, metric_enabled(Name, EnabledMetrics)
    ],
    ok.

add_metric_family({Name, Type, Help, Metrics}, Callback) ->
    Callback(create_mf(?METRIC_NAME(Name), Help, Type, Metrics)).

%%====================================================================
%% Private Parts
%%====================================================================

metrics() ->
    try
        metrics1()
    catch
        _:_ ->
            []
    end.

metrics1() ->
    Data = dist_info(),
    [
        {recv_bytes, gauge, "Number of bytes received by the socket.",
            metric(inet, recv_oct, Data)},
        {recv_cnt, gauge, "Number of packets received by the socket.",
            metric(inet, recv_cnt, Data)},
        {recv_max_bytes, gauge, "Size of the largest packet, in bytes, received by the socket.",
            metric(inet, recv_max, Data)},
        {recv_avg_bytes, gauge, "Average size of packets, in bytes, received by the socket.",
            metric(inet, recv_avg, Data)},
        {recv_dvi_bytes, gauge, "Average packet size deviation, in bytes, received by the socket.",
            metric(inet, recv_dvi, Data)},
        {send_bytes, gauge, "Number of bytes sent from the socket.", metric(inet, send_oct, Data)},
        {send_cnt, gauge, "Number of packets sent from the socket.", metric(inet, send_cnt, Data)},
        {send_max_bytes, gauge, "Size of the largest packet, in bytes, sent from the socket.",
            metric(inet, send_max, Data)},
        {send_avg_bytes, gauge, "Average size of packets, in bytes, sent from the socket.",
            metric(inet, send_avg, Data)},
        {send_pend_bytes, gauge, "Number of bytes waiting to be sent by the socket.",
            metric(inet, send_pend, Data)},
        {port_input_bytes, gauge, "The total number of bytes read from the port.",
            metric(port, input, Data)},
        {port_output_bytes, gauge, "The total number of bytes written to the port.",
            metric(port, output, Data)},
        {port_memory_bytes, gauge,
            "The total number of bytes allocated for this port by the runtime system. "
            "The port itself can have allocated memory that is not included.",
            metric(port, memory, Data)},
        {port_queue_size_bytes, gauge,
            "The total number of bytes queued by the port using the ERTS driver queue implementation.",
            metric(port, queue_size, Data)},
        {proc_memory_bytes, gauge,
            "The size in bytes of the process. This includes call stack, heap, and internal structures.",
            metric(proc, memory, Data)},
        {proc_heap_size_words, gauge,
            "The size in words of the youngest heap generation of the process. "
            "This generation includes the process stack. This information is "
            "highly implementation-dependent, and can change if the implementation changes.",
            metric(proc, heap_size, Data)},
        {proc_min_heap_size_words, gauge, "The minimum heap size for the process.",
            metric(proc, min_heap_size, Data)},
        {proc_min_bin_vheap_size_words, gauge,
            "The minimum binary virtual heap size for the process.",
            metric(proc, min_bin_vheap_size, Data)},
        {proc_stack_size_words, gauge, "The stack size, in words, of the process.",
            metric(proc, stack_size, Data)},
        {proc_total_heap_size_words, gauge,
            "The total size, in words, of all heap fragments of the process. "
            "This includes the process stack and any unreceived messages that "
            "are considered to be part of the heap.", metric(proc, total_heap_size, Data)},
        {proc_message_queue_len, gauge,
            "The number of messages currently in the message queue of the process.",
            metric(proc, message_queue_len, Data)},
        {proc_reductions, gauge, "The number of reductions executed by the process.",
            metric(proc, reductions, Data)},
        {proc_status, gauge,
            "The current status of the distribution process. "
            "The status is represented as a numerical value where `exiting=1', "
            "`suspended=2', `runnable=3', `garbage_collecting=4', `running=5' "
            "and `waiting=6'.", metric_proc_status(Data)},
        {node_state, gauge,
            "The current state of the distribution link. "
            "The state is represented as a numerical value where `pending=1', "
            "`up_pending=2' and `up=3'.", metric_node_state(Data)},
        {node_queue_size_bytes, gauge,
            "The number of bytes in the output distribution queue. "
            "This queue sits between the Erlang code and the port driver.",
            metric_node_queue_size(Data)}
    ].

enabled_metrics() ->
    application:get_env(prometheus, vm_dist_collector_metrics, all).

metric_enabled(Name, Metrics) ->
    Metrics =:= all orelse lists:member(Name, Metrics).

-include_lib("kernel/include/net_address.hrl").

dist_info() ->
    {ok, NodesInfo} = net_kernel:nodes_info(),
    AllPorts = [{P, erlang:port_info(P)} || P <- erlang:ports()],
    [dist_info(NodeInfo, AllPorts) || NodeInfo <- NodesInfo].

dist_info({Node, Info}, AllPorts) ->
    DistPid = proplists:get_value(owner, Info),
    NodeState = proplists:get_value(state, Info),
    case proplists:get_value(address, Info, #net_address{}) of
        #net_address{address = undefined} ->
            {Node, #{
                dist_pid => DistPid,
                node_state => NodeState
            }};
        #net_address{address = SockName} ->
            dist_info(Node, AllPorts, DistPid, NodeState, SockName)
    end.

dist_info(Node, AllPorts, DistPid, NodeState, SockName) ->
    case
        [
            P
         || {P, I} <- AllPorts,
            I =/= undefined,
            proplists:get_value(name, I) =:= "tcp_inet",
            inet:peername(P) =:= {ok, SockName}
        ]
    of
        [] ->
            {Node, #{
                dist_pid => DistPid,
                node_state => NodeState
            }};
        [DistPort] ->
            {ok, InetStats} = inet:getstat(DistPort),
            Map =
                case erlang:port_info(DistPort, connected) of
                    {_, DistPid} -> #{};
                    {_, ConnectedPid} -> dist_tls_info(ConnectedPid)
                end,
            {Node, Map#{
                inet_stats => InetStats,
                dist_port => DistPort,
                dist_pid => DistPid,
                node_state => NodeState
            }}
    end.

dist_tls_info(MaybeTlsConnPid) ->
    {_, CDict} = process_info(MaybeTlsConnPid, dictionary),
    case lists:keyfind('$initial_call', 1, CDict) of
        %% This is the right process: add it to the map and try to find
        %% the tls_sender process as well.
        {_, {tls_connection, init, 1}} ->
            dist_tls_sender_info(
                MaybeTlsConnPid,
                #{tls_connection_pid => MaybeTlsConnPid}
            );
        _ ->
            #{}
    end.

dist_tls_sender_info(TlsConnPid, Map) ->
    case process_info(TlsConnPid, links) of
        {_, [MaybeTlsSenderPid | _]} when is_pid(MaybeTlsSenderPid) ->
            {_, SDict} = process_info(MaybeTlsSenderPid, dictionary),
            case lists:keyfind('$initial_call', 1, SDict) of
                {_, {tls_sender, init, 1}} ->
                    Map#{tls_sender_pid => MaybeTlsSenderPid};
                _ ->
                    Map
            end;
        _ ->
            Map
    end.

metric(inet, Key, Data) ->
    [
        {[{peer, Node}], element(2, lists:keyfind(Key, 1, Stats))}
     || {Node, #{inet_stats := Stats}} <- Data
    ];
metric(port, Key, Data) ->
    [
        {[{peer, Node}], element(2, erlang:port_info(DistPort, Key))}
     || {Node, #{dist_port := DistPort}} <- Data
    ];
metric(proc, Key, Data) ->
    metric_proc(dist_pid, Key, Data) ++
        metric_proc(tls_connection_pid, Key, Data) ++
        metric_proc(tls_sender_pid, Key, Data).

metric_proc(PidKey, Key, Data) ->
    [
        {[{peer, Node}, {type, type(PidKey)}], element(2, process_info(Pid, Key))}
     || {Node, #{PidKey := Pid}} <- Data
    ].

type(dist_pid) -> dist;
type(tls_connection_pid) -> tls_connection;
type(tls_sender_pid) -> tls_sender.

metric_proc_status(Data) ->
    metric_proc_status(Data, dist_pid) ++
        metric_proc_status(Data, tls_connection_pid) ++
        metric_proc_status(Data, tls_sender_pid).

metric_proc_status(Data, PidKey) ->
    [
        {
            [{peer, Node}, {type, type(PidKey)}],
            proc_status(element(2, process_info(DistPid, status)))
        }
     || {Node, #{PidKey := DistPid}} <- Data
    ].

proc_status(exiting) -> 1;
proc_status(suspended) -> 2;
proc_status(runnable) -> 3;
proc_status(garbage_collecting) -> 4;
proc_status(running) -> 5;
proc_status(waiting) -> 6.

metric_node_state(Data) ->
    [
        {[{peer, Node}], node_state(Value)}
     || {Node, #{node_state := Value}} <- Data
    ].

node_state(pending) -> 1;
node_state(up_pending) -> 2;
node_state(up) -> 3.

metric_node_queue_size(Data) ->
    [
        {[{peer, Node}], node_queue_size(Node)}
     || {Node, _} <- Data
    ].

node_queue_size(Node) ->
    ConnId = ets:lookup_element(sys_dist, Node, 3),
    {ok, _, _, QueueSize} = erlang:dist_get_stat(ConnId),
    case QueueSize of
        false -> 0;
        true -> -1;
        _ -> QueueSize
    end.
