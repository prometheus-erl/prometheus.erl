-module(prometheus_format).
-compile({parse_transform, prometheus_pt}).

-moduledoc """
Module that implements this behaviour can be used as `foramt` parameter for exporters.

Built-in formats:
- `m:prometheus_text_format`
- `m:prometheus_protobuf_format`
""".

%%====================================================================
%% Callbacks
%%====================================================================

-doc "Should return content type of the format.".
-callback content_type() -> binary().

-doc "Should format `default` registry.".
-callback format() -> binary().

-doc "Should format `Registry`.".
-callback format(Registry :: prometheus_registry:registry()) -> binary().
