%% -------------------------------------------------------------------
%%
%% Copyright (c) 2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(re_wm_jsonapi).

-export([links/1,links/2,
         doc/1,doc/2,doc/3,doc/4,
         res/2,res/3,res/4,res/5]).

%%%===================================================================
%%% API
%%%===================================================================

links(RD) ->
    Self = list_to_binary(wrq:path(RD)),
    [{self, Self}].
links(RD,Related) ->
    Self = list_to_binary(wrq:path(RD)),
    [{self, Self}, {related, list_to_binary(Related)}].

doc(Resource) ->
    doc(Resource, [], [], []).
doc(Resource, Links) ->
    doc(Resource, Links, [], []).
doc(Resource, Links, Meta) ->
    doc(Resource, Links, Meta, []).
doc(Resource, Links, Meta, Included) ->
    D0 = build_object([{data, Resource},
                       {links, Links},
                       {meta, Meta},
                       {included, Included}], []),
    D0.

res(Type, Id) ->
    res(Type, Id, [], [], []).
res(Type, Id, Attributes) ->
    res(Type, Id, Attributes, [], []).
res(Type, Id, Attributes, Links) ->
    res(Type, Id, Attributes, Links, []).
res(Type, Id, Attributes, Links, Meta) ->
    R0 = build_object([{type, Type},
                       {id, Id}], []),
    R1 = build_object([{links, Links},
                       {meta, Meta}], []),
    R0 ++ Attributes ++ R1.

%% ====================================================================
%% Private
%% ====================================================================

build_object([], Accum) ->
    lists:reverse(Accum);
build_object([{_, []}|Rest], Accum) ->
    build_object(Rest, Accum);
build_object([{Name, Value}|Rest], Accum) ->
    build_object(Rest, [{Name, Value} | Accum]).