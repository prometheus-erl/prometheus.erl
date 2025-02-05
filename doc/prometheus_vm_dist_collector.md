

# Module prometheus_vm_dist_collector #
* [Description](#description)

Collects information about the sockets and processes involved
in the Erlang distribution mechanism.

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##

All metrics include a label 'peer' that indicates which
distributed connection the metric is about.


### <a name="Exported_metrics">Exported metrics</a> ###

Metrics pertaining to processes may apply to three different types
of processes depending on the distribution transport:
`type="dist"`, `type="tls_connection"` or `type="tls_sender"`.

* `erlang_vm_dist_recv_bytes`<br />
Type: gauge.<br />
Number of bytes received by the socket.

* `erlang_vm_dist_recv_cnt`<br />
Type: gauge.<br />
Number of packets received by the socket.

* `erlang_vm_dist_recv_max_bytes`<br />
Type: gauge.<br />
Size of the largest packet, in bytes, received by the socket.

* `erlang_vm_dist_recv_avg_bytes`<br />
Type: gauge.<br />
Average size of packets, in bytes, received by the socket.

* `erlang_vm_dist_recv_dvi_bytes`<br />
Type: gauge.<br />
Average packet size deviation, in bytes, received by the socket.

* `erlang_vm_dist_send_bytes`<br />
Type: gauge.<br />
Number of bytes sent from the socket.

* `erlang_vm_dist_send_cnt`<br />
Type: gauge.<br />
Number of packets sent from the socket.

* `erlang_vm_dist_send_max_bytes`<br />
Type: gauge.<br />
Size of the largest packet, in bytes, sent from the socket.

* `erlang_vm_dist_send_avg_bytes`<br />
Type: gauge.<br />
Average size of packets, in bytes, sent from the socket.

* `erlang_vm_dist_send_pend_bytes`<br />
Type: gauge.<br />
Number of bytes waiting to be sent by the socket.

* `erlang_vm_dist_port_input_bytes`<br />
Type: gauge.<br />
The total number of bytes read from the port.

* `erlang_vm_dist_port_output_bytes`<br />
Type: gauge.<br />
The total number of bytes written to the port.

* `erlang_vm_dist_port_memory_bytes`<br />
Type: gauge.<br />
The total number of bytes allocated for this port by the runtime system.
The port itself can have allocated memory that is not included.

* `erlang_vm_dist_port_queue_size_bytes`<br />
Type: gauge.<br />
The total number of bytes queued by the port using the ERTS driver queue implementation.

* `erlang_vm_dist_proc_memory_bytes`<br />
Type: gauge.<br />
The size in bytes of the process. This includes call stack, heap, and internal structures.

* `erlang_vm_dist_proc_heap_size_words`<br />
Type: gauge.<br />
The size in words of the youngest heap generation of the process.
This generation includes the process stack. This information is
highly implementation-dependent, and can change if the implementation changes.

* `erlang_vm_dist_proc_min_heap_size_words`<br />
Type: gauge.<br />
The minimum heap size for the process.

* `erlang_vm_dist_proc_min_bin_vheap_size_words`<br />
Type: gauge.<br />
The minimum binary virtual heap size for the process.

* `erlang_vm_dist_proc_stack_size_words`<br />
Type: gauge.<br />
The stack size, in words, of the process.

* `erlang_vm_dist_proc_total_heap_size_words`<br />
Type: gauge.<br />
The total size, in words, of all heap fragments of the process.
This includes the process stack and any unreceived messages that
are considered to be part of the heap.

* `erlang_vm_dist_proc_message_queue_len`<br />
Type: gauge.<br />
The number of messages currently in the message queue of the process.

* `erlang_vm_dist_proc_reductions`<br />
Type: gauge.<br />
The number of reductions executed by the process.

* `erlang_vm_dist_proc_status`<br />
Type: gauge.<br />
The current status of the distribution process.<br />
The status is represented as a numerical value where `exiting=1`,
`suspended=2`, `runnable=3`, `garbage_collecting=4`, `running=5`
and `waiting=6`.

* `erlang_vm_dist_node_state`<br />
Type: gauge.<br />
The current state of the distribution link.<br />
The state is represented as a numerical value where `pending=1`,
`up_pending=2` and `up=3`.

* `erlang_vm_dist_node_queue_size_bytes`<br />
Type: gauge.<br />
The number of bytes in the output distribution queue.<br />
This queue sits between the Erlang code and the port driver.



### <a name="Configuration">Configuration</a> ###

Metrics exported by this collector can be configured via
`vm_dist_collector_metrics` key of `prometheus` app environment.

Available options:

* `recv_bytes` for `erlang_vm_dist_recv_bytes`.

* `recv_cnt` for `erlang_vm_dist_recv_cnt`.

* `recv_max_bytes` for `erlang_vm_dist_recv_max_bytes`.

* `recv_avg_bytes` for `erlang_vm_dist_recv_avg_bytes`.

* `recv_dvi_bytes` for `erlang_vm_dist_recv_dvi_bytes`.

* `send_bytes` for `erlang_vm_dist_send_bytes`.

* `send_cnt` for `erlang_vm_dist_send_cnt`.

* `send_max_bytes` for `erlang_vm_dist_send_max_bytes`.

* `send_avg_bytes` for `erlang_vm_dist_send_avg_bytes`.

* `send_pend_bytes` for `erlang_vm_dist_send_pend_bytes`.

* `port_input_bytes` for `erlang_vm_dist_port_input_bytes`.

* `port_output_bytes` for `erlang_vm_dist_port_output_bytes`.

* `port_memory_bytes` for `erlang_vm_dist_port_memory_bytes`.

* `port_queue_size_bytes` for `erlang_vm_dist_port_queue_size_bytes`.

* `proc_memory_bytes` for `erlang_vm_dist_proc_memory_bytes`.

* `proc_heap_size_words` for `erlang_vm_dist_proc_heap_size_words`.

* `proc_min_heap_size_words` for `erlang_vm_dist_proc_min_heap_size_words`.

* `proc_min_bin_vheap_size_words` for `erlang_vm_dist_proc_min_bin_vheap_size_words`.

* `proc_stack_size_words` for `erlang_vm_dist_proc_stack_size_words`.

* `proc_total_heap_size_words` for `erlang_vm_dist_proc_total_heap_size_words`.

* `proc_message_queue_len` for `erlang_vm_dist_proc_message_queue_len`.

* `proc_reductions` for `erlang_vm_dist_proc_reductions`.

* `proc_status` for `erlang_vm_dist_proc_status`.

* `node_state` for `erlang_vm_dist_node_state`.

* `node_queue_size_bytes` for `erlang_vm_dist_node_queue_size_bytes`.


By default all metrics are enabled.