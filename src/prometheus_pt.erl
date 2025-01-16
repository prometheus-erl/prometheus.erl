-module(prometheus_pt).
-moduledoc false.

-export([parse_transform/2]).

-spec parse_transform(Ast, Opts :: list()) -> Ast.
parse_transform(Ast, Opts) ->
    case lists:member({with_ex_doc, true}, Opts) of
        true ->
            Res = lists:map(fun map_doc_attributes/1, Ast),
            Res;
        false ->
            Res = lists:filter(fun filter_doc_attributes/1, Ast),
            Res
    end.

map_doc_attributes({attribute, Metadata, moduledoc, Text} = _ModuleDoc) when is_list(Text) ->
    NewText = string:replace(Text, "\\\"", "\"", all),
    {attribute, Metadata, moduledoc, NewText};
map_doc_attributes({attribute, Metadata, doc, Text} = _Doc) when is_list(Text) ->
    NewText = string:replace(Text, "\\\"", "\"", all),
    {attribute, Metadata, doc, NewText};
map_doc_attributes(El) ->
    El.

filter_doc_attributes({error, {_, erl_parse, "bad attribute"}} = _Warning) ->
    false;
filter_doc_attributes({warning, {_, epp, string_concat}} = _Warning) ->
    false;
filter_doc_attributes({attribute, _, moduledoc, _} = _ModuleDoc) ->
    false;
filter_doc_attributes({attribute, _, doc, _} = _Attribute) ->
    false;
filter_doc_attributes(_) ->
    true.
