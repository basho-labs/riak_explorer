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

-export([convert_attributes/1, convert_attributes/2,
         self_link/2,
         links/1,
         links/2,
         doc/6,
         res/5]).

%%%===================================================================
%%% API
%%%===================================================================

doc(RD, DataName, Resource, Links0, Meta, Included) ->
    Links1 = case proplists:is_defined(self, Links0) of
        true -> Links0;
        false -> links(RD) ++ Links0
    end,

    D0 = build_object([{DataName, Resource},
                       {links, Links1},
                       {meta, Meta},
                       {included, Included}], []),
    D0.

convert_attributes(Attributes) ->
    convert_attributes(Attributes, []).

convert_attributes([], Accum) ->
    lists:reverse(Accum);
convert_attributes([{name, Value}|Rest], Accum) ->
    convert_attributes(Rest, [{id, Value} | Accum]);
convert_attributes([A|Rest], Accum) ->
    convert_attributes(Rest, [A | Accum]).

self_link(RD, Name) when is_atom(Name) ->
    self_link(RD, atom_to_list(Name));
self_link(RD, Name) ->
    Self = list_to_binary(re_file_util:add_slash(wrq:path(RD)) ++ Name),
    [{self, Self}].

links(RD) ->
    Self = list_to_binary(wrq:path(RD)),
    [{self, Self}].
links(RD, Related) ->
    links(RD) ++ [{rleated, list_to_binary(Related)}].

res(RD, Type, [[{_,_}|_]|_]=List, Links0, Meta) ->
    lists:map(fun(R0) -> 
        R1 = case proplists:is_defined(id, R0) of
            true -> R0;
            false -> convert_attributes(R0)
        end,
        Id = proplists:get_value(id, R1),
        Links1 = maybe_add_self_link(RD, Id, R1, Links0),
        res(RD, Type, R1, Links1, Meta) end, List);

res(_RD, Type, [{_,_}|_]=A0, Links, Meta) ->
    A1 = case proplists:is_defined(id, A0) of
        true -> A0;
        false -> convert_attributes(A0)
    end,

    R0 = build_object([
        {type, Type}], []),
    R1 = build_object([{links, Links}, {meta, Meta}], []),

    R0 ++ A1 ++ R1.

%% ====================================================================
%% Private
%% ====================================================================

maybe_add_self_link(RD, Id, R, L) ->
    case proplists:is_defined(links, R) of
        undefined -> 
            case Id of
                undefined -> L;
                _ -> self_link(RD, Id) ++ L
            end;
        _ -> []
    end.

build_object([], Accum) ->
    lists:reverse(Accum);
build_object([{_, []}|Rest], Accum) ->
    build_object(Rest, Accum);
build_object([{Name, Value}|Rest], Accum) ->
    build_object(Rest, [{Name, Value} | Accum]).