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

-module(riak_explorer).
-export([ping/0,
         home/0,
         routes/0,
         bucket_types/0,
         cluster_nodes/1,
         cluster_http_listeners/1,
         node_http_listener/1]).

-include("riak_explorer.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

home() ->
    [{message, <<"riak_explorer api">>}].

ping() ->
    [{message, <<"pong">>}].

routes() ->
    re_config:formatted_routes().



bucket_types() ->
    remote(re_riak_patch, bucket_types, []).

cluster_nodes(Cluster) ->
    case Cluster of
        "default" ->
            {ok, MyRing} = remote(riak_core_ring_manager, get_my_ring, []), 
            [{cluster_nodes, remote(riak_core_ring, all_members, [MyRing])}];
        _ ->
            [{cluster_nodes, []}]
    end.

cluster_http_listeners(Cluster) ->
    case Cluster of
        "default" ->
            [{cluster_nodes, ClusterNodes}] = cluster_nodes(Cluster),
            [{cluster_http_listeners, cluster_http_listeners(ClusterNodes, [])}];
        _ ->
            [{cluster_http_listeners, []}]
    end.

cluster_http_listeners([], Acc) ->
    lists:reverse(Acc);
cluster_http_listeners([Node|Rest], Acc) ->
    cluster_http_listeners(Rest, [node_http_listener(Node) | Acc]).

node_http_listener(Node) ->
    {ok,[{Ip,Port}]} = remote(Node, application, get_env, [riak_api, http]),
    list_to_binary(Ip ++ ":" ++ integer_to_list(Port)).

%%%===================================================================
%%% Private
%%%===================================================================

remote(N,M,F,A) ->
    rpc:call(N, M, F, A, 5000).

remote(M,F,A) ->
    remote(re_config:target_node(), M, F, A).


-ifdef(TEST).

home_test() ->
    Expected = [{message, <<"riak_explorer api">>}],
    ?assertEqual(Expected, home()).

ping_test() ->
    Expected = [{message, <<"pong">>}],
    ?assertEqual(Expected, ping()).

-endif.