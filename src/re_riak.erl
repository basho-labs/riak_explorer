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

-module(re_riak).

-include("riak_explorer.hrl").
-export([load_patch/1,
         http_listener/1,
         bucket_types/1,
         nodes/1]).

%%%===================================================================
%%% API
%%%===================================================================

load_patch(Node) ->
    IsLoaded = remote(Node, code, is_loaded, [re_riak_patch]),
    maybe_load_patch(Node, IsLoaded).

http_listener(Node) ->
    {ok,[{Ip,Port}]} = remote(Node, application, get_env, [riak_api, http]),
    [{http_listener, list_to_binary(Ip ++ ":" ++ integer_to_list(Port))}].

bucket_types(Node) ->
    load_patch(Node),
    remote(Node, re_riak_patch, bucket_types, []).

nodes(Cluster) ->
    case Cluster of
        "default" ->
            {ok, MyRing} = remote(riak_core_ring_manager, get_my_ring, []), 
            Nodes = remote(riak_core_ring, all_members, [MyRing]),
            WithIds = lists:map(fun(N) -> {id, N} end, Nodes),
            [{nodes, WithIds}];
        _ ->
            %%TODO: Connect to target_node, find route to MDC cluster(s), get nodes
            [{nodes, []}]
    end.

%%%===================================================================
%%% Private
%%%===================================================================

maybe_load_patch(Node, false) ->
    lager:info("Loading re_riak_patch module into node[~p].", [Node]),
    {Mod, Bin, _} = code:get_object_code(re_riak_patch),
    rpc:call(Node, code, load_binary, [Mod, "/tmp/re_riak_patch.beam", Bin]);
maybe_load_patch(Node, _) ->
    LocalVersion = re_riak_patch:version(),
    RemoteVersion = remote(Node, re_riak_patch, version, []),
    lager:info("Found version ~p of re_riak_patch on node[~p], current version is ~p.", [RemoteVersion, Node, LocalVersion]),
    case LocalVersion == RemoteVersion of
        false -> maybe_load_patch(Node, false);
        _ -> ok
    end.

remote(N,M,F,A) ->
    rpc:call(N, M, F, A, 60000).

remote(M,F,A) ->
    remote(re_config:target_node(), M, F, A).