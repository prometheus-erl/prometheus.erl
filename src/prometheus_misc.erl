%% @hidden
%% behaviour_modules original Copyright message
%% all other code is under MIT
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(prometheus_misc).

-export([behaviour_modules/1]).

%%====================================================================
%% Public API
%%====================================================================

-spec behaviour_modules(Behaviour :: atom()) -> [atom()] | [].
behaviour_modules(Behaviour) ->
    [
        Module
     || {Module, Behaviours} <-
            all_module_attributes(behaviour),
        lists:member(Behaviour, Behaviours)
    ].

%%====================================================================
%% Private Parts
%%====================================================================

all_module_attributes(Name) ->
    Targets =
        lists:usort(
            lists:append(
                [
                    [{App, Module} || Module <- Modules]
                 || {App, _, _} <- application:loaded_applications(),
                    {ok, Modules} <- [application:get_key(App, modules)]
                ]
            )
        ),
    lists:foldl(
        fun({_App, Module}, Acc) ->
            case
                lists:append([
                    Atts
                 || {N, Atts} <- module_attributes(Module),
                    N =:= Name
                ])
            of
                [] -> Acc;
                Atts -> [{Module, Atts} | Acc]
            end
        end,
        [],
        Targets
    ).

module_attributes(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
            [];
        {'EXIT', _Reason} ->
            %% return empty list too
            [];
        V ->
            V
    end.
