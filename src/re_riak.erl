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
-compile({no_auto_import,[nodes/1]}).

-export([join/2,
         plan/1,
         commit/1,
         status/1,
         ringready/1,
         leave/1,
         leave/2,
         force_remove/2,
         replace/3,
         force_replace/3,
         clear/1]).

-export([client/1,
         get_json/3,
         put_json/4]).

-export([list_buckets/4,
         clean_buckets/2,
         put_buckets/3,
         list_keys/5,
         clean_keys/3,
         put_keys/4]).

-export([http_listener/1,
         pb_listener/1,
         bucket_types/1]).

-export([clusters/0,
         cluster/1,
         first_node/1,
         nodes/1,
         node_exists/2]).

-export([load_patch/1]).

%%%===================================================================
%%% Control API
%%%===================================================================

replace(Node, Node1, Node2) ->
    try
        Response = remote(Node, riak_core_claimant, replace, [Node1, Node2]),
        case Response of
            ok ->
                [{control, [{success, ok}]}];
            {error, R} ->
                [{control, [{error, R}]}]
        end
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason])),
            [{control, [{error, Error}]}]
    end.

force_replace(Node, Node1, Node2) ->
    try
        Response = remote(Node, riak_core_claimant, force_replace, [Node1, Node2]),
        case Response of
            ok ->
                [{control, [{success, ok}]}];
            {error, R} ->
                [{control, [{error, R}]}]
        end
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason])),
            [{control, [{error, Error}]}]
    end.

join(Node, Node1) ->
    Response = remote(Node, riak_core, staged_join, [Node1]),
    case Response of
        {error, Reason} -> [{control, [{error, Reason}]}];
        ok -> [{control, [{success, ok}]}]
    end.

leave(Node, Node1) ->
    try
        Response = remote(Node, riak_core_claimant, leave_member, [Node1]),
        case Response of
            ok ->
                [{control, [{success, ok}]}];
            {error, R} ->
                [{control, [{error, R}]}]
        end
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason])),
            [{control, [{error, Error}]}]
    end.

force_remove(Node, Node1) ->
    try
        Response = remote(Node, riak_core_claimant, remove_member, [Node1]),
        case Response of
            ok ->
                [{control, [{success, ok}]}];
            {error, R} ->
                [{control, [{error, R}]}]
        end
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason])),
            [{control, [{error, Error}]}]
    end.

leave(Node) ->
    leave(Node, Node).

plan(Node) ->
    Response = remote(Node, riak_core_claimant, plan, []),

    case Response of
        {error, Reason} ->
            [{control, [{error, Reason}]}];
        {ok, Changes, _} ->
            [{control, [{success, lists:map(fun({N, {Action, Target}}) ->
                    {N, [{Action, Target}]};
                 ({N, Action}) -> {N, Action}
            end, Changes)}]}]
    end.
commit(Node) ->
    Response = remote(Node, riak_core_claimant, commit, []),

    case Response of
        ok -> [{control, [{success, ok}]}];
        {error, Reason} -> [{control, [{error, Reason}]}];
        _ -> [{control, [{error, retry_plan}]}]
    end.

clear(Node) ->
    try
        Response = remote(Node, riak_core_claimant, clear, []),
        case Response of
            ok -> [{control, [{success, ok}]}];
            _ -> [{control, [{error, unkown}]}]
        end
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason])),
            [{control, [{error, Error}]}]
    end.

status(Node) ->
    {ok, Ring} = remote(Node, riak_core_ring_manager, get_my_ring, []),
    AllStatus = lists:keysort(2, remote(Node, riak_core_ring, all_member_status, [Ring])),
    IsPending = ([] /= remote(Node, riak_core_ring, pending_changes, [Ring])),
    {Nodes, Joining, Valid, Down, Leaving, Exiting} =
        lists:foldl(fun({N, Status},
                        {Nodes0, Joining0, Valid0, Down0, Leaving0, Exiting0}) ->
            {RingPercent, NextPercent} =
                remote(Node, riak_core_console, pending_claim_percentage, [Ring, N]),
            NodeObj = case IsPending of
                true ->
                    {N, [{status, Status},
                          {ring_percentage, RingPercent},
                          {pending_percentage, NextPercent}]};
                false ->
                    {N, [{status, Status},
                          {ring_percentage, RingPercent},
                          {pending_percentage, null}]}
            end,
            case Status of
                joining ->
                    {[NodeObj|Nodes0], Joining0 + 1, Valid0, Down0, Leaving0, Exiting0};
                valid ->
                    {[NodeObj|Nodes0], Joining0, Valid0 + 1, Down0, Leaving0, Exiting0};
                down ->
                    {[NodeObj|Nodes0], Joining0, Valid0, Down0 + 1, Leaving0, Exiting0};
                leaving ->
                    {[NodeObj|Nodes0], Joining0, Valid0, Down0, Leaving0 + 1, Exiting0};
                exiting ->
                    {[NodeObj|Nodes0], Joining0, Valid0, Down0, Leaving0, Exiting0 + 1}
            end
        end, {[],0,0,0,0,0}, AllStatus),
    [{control, [
        {success, [
            {nodes, lists:reverse(Nodes)},
            {valid, Valid},
            {leaving, Leaving},
            {exiting, Exiting},
            {joining, Joining},
            {down, Down}]}]}].

ringready(Node) ->
    try
        Response = remote(Node, riak_core_status, ringready, []),
        case Response of
            {ok, Nodes} ->
                [{control, [{success, [{nodes, Nodes}]}]}];
            {error, {different_owners, N1, N2}} ->
                [{control, [{error, [{different_owners, [N1, N2]}]}]}];
            {error, {nodes_down, Down}} ->
                [{control, [{error, [{nodes_down, Down}]}]}]
        end
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason])),
            [{control, [{error, Error}]}]
    end.

%%%===================================================================
%%% Riak Client API
%%%===================================================================

%% Local Client
client(Node) ->
    {ok,[{Ip,Port}]} = remote(Node, application, get_env, [riak_api, pb]),
    {ok, Pid} = riakc_pb_socket:start_link(Ip, Port),
    Pid.

%% Local Client Get
get_json(Node, Bucket, Key) ->
    C = client(Node),
    O = riakc_pb_socket:get(C, Bucket, Key),
    RawData = riakc_obj:value(O),
    mochijson2:decode(RawData).

%% Local Client Put
put_json(Node, Bucket, Key, Data) ->
    C = client(Node),
    RawData = mochijson2:encode(Data),
    O = riakc_obj:new(Bucket, Key, RawData, "application/json"),
    riakc_pb_socket:get(C, O).

%%%===================================================================
%%% Keyjournal API
%%%===================================================================

list_buckets(Node, BucketType, Start, Rows) ->
    case re_config:development_mode() of
        true ->
            re_keyjournal:read({buckets, Node, [BucketType]}, Start, Rows);
        false ->
            re_keyjournal:read_cache({buckets, Node, [BucketType]}, Start, Rows)
    end.

clean_buckets(Node, BucketType) ->
    re_keyjournal:clean({buckets, Node, [BucketType]}).

put_buckets(Node, BucketType, Buckets) ->
    re_keyjournal:write_cache({buckets, Node, [BucketType]}, Buckets).

list_keys(Node, BucketType, Bucket, Start, Rows) ->
    case re_config:development_mode() of
        true ->
            re_keyjournal:read({keys, Node, [BucketType, Bucket]}, Start, Rows);
        false ->
            re_keyjournal:read_cache({keys, Node, [BucketType, Bucket]}, Start, Rows)
    end.

clean_keys(Node, BucketType, Bucket) ->
    re_keyjournal:clean({keys, Node, [BucketType, Bucket]}).

put_keys(Node, BucketType, Bucket, Keys) ->
    re_keyjournal:write_cache({keys, Node, [BucketType, Bucket]}, Keys).

%%%===================================================================
%%% Node Properties API
%%%===================================================================

http_listener(Node) ->
    {ok,[{Ip,Port}]} = remote(Node, application, get_env, [riak_api, http]),
    [{http_listener, list_to_binary(Ip ++ ":" ++ integer_to_list(Port))}].

pb_listener(Node) ->
    {ok,[{Ip,Port}]} = remote(Node, application, get_env, [riak_api, pb]),
    [{pb_listener, list_to_binary(Ip ++ ":" ++ integer_to_list(Port))}].

bucket_types(Node) ->
    load_patch(Node),
    List = remote(Node, re_riak_patch, bucket_types, []),
    [{bucket_types, List}].

%%%===================================================================
%%% Cluster API
%%%===================================================================

clusters() ->
    Clusters = re_config:clusters(),
    Mapped = lists:map(fun({C, _}) -> [{id,C}, {riak_node, re_config:riak_node(C)}, {development_mode, re_config:development_mode(C)}] end, Clusters),
    [{clusters, Mapped}].

cluster(Id) ->
    [{clusters, Clusters}] = clusters(),

    case find_cluster(list_to_atom(binary_to_list(Id)), Clusters) of
        {error, not_found} = R -> R;
        Props -> [{clusters, Props}]
    end.

first_node(Cluster) ->
    case nodes(Cluster) of
        [{nodes, [[{id, Node}]|_]}] -> Node;
        [{nodes, []}] -> [{error, no_nodes}];
        [{error, not_found}] -> [{error, no_nodes}]
    end.

nodes(Cluster) ->
    RiakNode = re_config:riak_node(list_to_atom(Cluster)),

    case remote(RiakNode, riak_core_ring_manager, get_my_ring, []) of
        {ok, MyRing} ->
            Nodes = remote(RiakNode, riak_core_ring, all_members, [MyRing]),
            WithIds = lists:map(fun(N) -> [{id, N}] end, Nodes),
            [{nodes, WithIds}];
        _ -> [{nodes, []}]
    end.

node_exists(Cluster, Node) ->
    Filter = lists:filter(fun(X) ->
        case X of
            {error, not_found} -> false;
            [{id, Node}] -> true;
            _ -> false
        end end, nodes(Cluster)),
    case length(Filter) of
        N when N > 0 -> true;
        _ -> false
    end.

%%%===================================================================
%%% Utility API
%%%===================================================================

load_patch(Node) ->
    IsLoaded = remote(Node, code, is_loaded, [re_riak_patch]),
    maybe_load_patch(Node, IsLoaded).

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

% remote(M,F,A) ->
%     remote(re_config:riak_node(), M, F, A).

find_cluster(_, []) ->
    {error, not_found};
find_cluster(Id, [[{id, Id}|_]=Props|_]) ->
    Props;
find_cluster(Id, [[{id, _}|_]|Rest]) ->
    find_cluster(Id, Rest).
