%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.  All Rights Reserved.
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

-module(re_cluster).

-compile({no_auto_import,[nodes/1]}).

-export([exists/1,
         props/1,
         from_props/2,
         riak_node/1,
         development_mode/1,
         nodes/1]).

-type(re_cluster() :: atom()).
-export_type([re_cluster/0]).

-type(re_cluster_prop() ::
        {id, re_cluster()} |
        {development_mode, boolean()} |
        {riak_type, re_node:re_node_type() | unavailable} |
        {riak_version, binary() | unavailable} |
        {available, boolean()}).

-type(re_cluster_props() :: [re_cluster_prop()]).
-export_type([re_cluster_props/0]).
        
%%%===================================================================
%%% API
%%%===================================================================

-spec exists(re_cluster()) -> boolean().
exists(C) ->
    riak_explorer:cluster_config(C) =/= {error, not_found}.

-spec props(re_cluster()) -> {error, not_found} | re_cluster_props().
props(C) ->
    case riak_explorer:cluster_config(C) of
        {error, not_found} -> {error, not_found};
        Props -> from_props(C, Props)
    end.
    
-spec from_props(re_cluster(), re_cluster_props()) -> re_cluster_props().
from_props(C, Props) ->
    N = proplists:get_value(riak_node, Props, default_riak_node()),
    [{id,C},
     {riak_node, N},
     {development_mode, proplists:get_value(development_mode, Props, true)},
     {riak_type, proplists:get_value(riak_type, Props, re_node:type(N))},
     {riak_version, proplists:get_value(riak_version, Props, re_node:version(N))},
     {available, proplists:get_value(available, Props, re_node:available(N))}].

-spec riak_node(re_cluster()) -> {error, not_found} | re_node:re_node().
riak_node(C) ->
    case props(C) of
        {error, not_found} ->
            {error, not_found};
        Props ->
            proplists:get_value(riak_node, Props, {error, not_found})
    end.

-spec development_mode(re_cluster()) -> boolean().
development_mode(C) ->
    case riak_explorer:cluster_config(C) of
        {error, not_found} -> 
            true;
        Props ->
            proplists:get_value(development_mode, Props, true)
    end.

-spec nodes(re_cluster()) -> {error, term()} | [re_node:re_node_props()].
nodes(C) ->
    case riak_node(C) of
        {error, not_found} ->
            {error, not_found};
        N ->
            case re_node:ring_members(N) of
                {error, Reason} ->
                    {error, Reason};
                Nodes ->
                    lists:map(fun(N1) -> re_node:props(N1) end, Nodes)
            end
    end.

%%%===================================================================
%%% Private
%%%===================================================================

-spec default_riak_node() -> re_node:re_node().
default_riak_node() ->
    case riak_explorer:is_riak() of
        false -> 'riak@127.0.0.1';
        true -> node()
    end.
