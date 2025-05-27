%% behaviour_modules original Copyright message
%% all other code is under MIT
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
-module(prometheus_misc).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-export([behaviour_modules/1]).

%% Retrieves a list of modules that implement a specified behaviour.
-spec behaviour_modules(Behaviour :: atom()) -> [module()].
behaviour_modules(Behaviour) ->
    Applications = application:loaded_applications(),
    Modules = lists:flatmap(fun get_modules_for_app/1, Applications),
    Targets = lists:usort(Modules),
    extract_behaviour_from_modules(Targets, Behaviour).

-spec get_modules_for_app({atom(), string(), string()}) -> [module()].
get_modules_for_app({App, _, _}) ->
    case application:get_key(App, modules) of
        {ok, Modules} -> Modules;
        _ -> []
    end.

-spec extract_behaviour_from_modules([module()], Behaviour :: atom()) -> [module()].
extract_behaviour_from_modules(Modules, Behaviour) ->
    Filter = fun(Module) -> does_module_implement_behaviour(Module, Behaviour) end,
    lists:filter(Filter, Modules).

-spec does_module_implement_behaviour(Module :: module(), Behaviour :: atom()) -> boolean().
does_module_implement_behaviour(Module, Behaviour) ->
    Attributes = module_attributes(Module),
    Behaviours = [Atts || {N, Atts} <- Attributes, (N =:= behaviour orelse N =:= behavior)],
    lists:member(Behaviour, lists:flatten(Behaviours)).

-spec module_attributes(atom()) -> [{atom(), term()}] | [].
module_attributes(Module) ->
    try
        Module:module_info(attributes)
    catch
        error:undef -> []
    end.
