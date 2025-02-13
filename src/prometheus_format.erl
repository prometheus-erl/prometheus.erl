-module(prometheus_format).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Module that implements this behaviour can be used as `foramt` parameter for exporters.

Built-in formats:
- `m:prometheus_text_format`
- `m:prometheus_protobuf_format`
""").

%%====================================================================
%% Callbacks
%%====================================================================

?DOC("Should return content type of the format.").
-callback content_type() -> binary().

?DOC("Should format `default` registry.").
-callback format() -> binary().

?DOC("Should format `Registry`.").
-callback format(Registry :: prometheus_registry:registry()) -> binary().
