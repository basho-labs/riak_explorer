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

-export([repair/1,
         join/2,
         staged_join/2,
         force_remove/2,
         leave/2,
         staged_leave/1,
         staged_leave/2,
         force_replace/3,
         replace/3,
         staged_replace/3,
         plan/1,
         clear/1,
         commit/1,
         status/1,
         ringready/1,
         transfers/1,
         aae_status/1]).

-export([repl_clustername/1,
         repl_clustername/2,
         repl_connections/1,
         repl_connect/3,
         repl_disconnect/2,
         repl_realtime_enable/2,
         repl_realtime_disable/2,
         repl_realtime_start/1,
         repl_realtime_start/2,
         repl_realtime_stop/1,
         repl_realtime_stop/2,
         repl_fullsync_enable/2,
         repl_fullsync_disable/2,
         repl_fullsync_start/1,
         repl_fullsync_start/2,
         repl_fullsync_stop/1,
         repl_fullsync_stop/2,
         repl_clusterstats/1,
         repl_clusterstats/3,
         repl_clusterstats_cluster_mgr/1,
         repl_clusterstats_fs_coordinate/1,
         repl_clusterstats_fullsync/1,
         repl_clusterstats_proxy_get/1,
         repl_clusterstats_realtime/1]).

-export([client/1,
         get_json/3,
         put_json/4,
         delete_bucket/3,
         delete_bucket/4,
         delete_bucket_job/1]).

-export([list_buckets_cache/4,
         list_buckets/2,
         clean_buckets/2,
         put_buckets/3,
         list_keys_cache/5,
         list_keys/3,
         clean_keys/3,
         put_keys/4]).

-export([http_listener/1,
         pb_listener/1,
         bucket_types/1,
         create_bucket_type/3]).

-export([cluster_id_for_node/1,
         clusters/0,
         cluster/1,
         first_node/1,
         nodes/1,
         node_exists/2]).

-export([load_patch/1]).

-export([remote/4,
         node_is_alive/1]).

%%%===================================================================
%%% Control API
%%%===================================================================

repair(Node) ->
    {ok, Ring} = remote(Node, riak_core_ring_manager, get_my_ring, []),
    AllOwners = remote(Node, riak_core_ring, all_owners, [Ring]),
    F1 = fun({P, N}, Acc) ->
        case N of
            Node -> [P|Acc];
            _ -> Acc
        end
    end,
    Partitions = lists:foldl(F1, [], AllOwners),
    Results = [remote(Node, riak_kv_vnode, repair, [P]) || P <- Partitions],
    F2 = fun(R, [{success, S},{failure, F}]) ->
        case R of
            {ok,_} -> [{success, S+1},{failure, F}];
            _ -> [{success, S},{failure, F+1}]
        end
    end,
    Result = lists:foldl(F2, [{success, 0},{failure, 0}], Results),
    [{control, Result}].

force_replace(Node, Node1, Node2) ->
    handle_error(fun () ->
                         Response = remote(Node, riak_core_claimant, force_replace, [Node1, Node2]),
                         case Response of
                             ok ->
                                 [{control, [{success, ok}]}];
                             {error, R} ->
                                 [{control, [{error, R}]}]
                         end
                 end).

replace(Node, Node1, Node2) ->
    force_replace(Node, Node1, Node2),
    plan(Node),
    commit(Node).

staged_replace(Node, Node1, Node2) ->
    handle_error(fun () ->
                         Response = remote(Node, riak_core_claimant, replace, [Node1, Node2]),
                         case Response of
                             ok ->
                                 [{control, [{success, ok}]}];
                             {error, R} ->
                                 [{control, [{error, R}]}]
                         end
                 end).

join(Node, Node1) ->
    Response = remote(Node, riak_core, join, [Node1]),
    case Response of
        {error, Reason} -> [{control, [{error, Reason}]}];
        ok -> [{control, [{success, ok}]}]
    end.

staged_join(Node, Node1) ->
    Response = remote(Node, riak_core, staged_join, [Node1]),
    case Response of
        {error, Reason} -> [{control, [{error, Reason}]}];
        ok -> [{control, [{success, ok}]}]
    end.

    force_remove(Node, Node1) ->
        leave(Node, Node1).

leave(Node, Node1) ->
    handle_error(fun () ->
                         Response = remote(Node, riak_core, remove, [Node1]),
                         case Response of
                             ok ->
                                 [{control, [{success, ok}]}];
                             {error, R} ->
                                 [{control, [{error, R}]}]
                         end
                 end).

staged_leave(Node) ->
    staged_leave(Node, Node).

staged_leave(Node, Node1) ->
    handle_error(fun () ->
                         Response = remote(Node, riak_core_claimant, leave_member, [Node1]),
                         case Response of
                             ok ->
                                 [{control, [{success, ok}]}];
                             {error, R} ->
                                 [{control, [{error, R}]}]
                         end
                 end).

plan(Node) ->
    Response = remote(Node, riak_core_claimant, plan, []),

    case Response of
        {error, Reason} ->
            [{control, [{error, Reason}]}];
        {ok, Changes, _} ->
            [{control, [{changes, lists:map(fun({N, {Action, Target}}) ->
                    [{node, N},{action, Action},{target, Target}];
                 ({N, Action}) ->
                    [{node, N},{action, Action},{target, null}]
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
    handle_error(fun () ->
                         Response = remote(Node, riak_core_claimant, clear, []),
                         case Response of
                             ok -> [{control, [{success, ok}]}];
                             _ -> [{control, [{error, unkown}]}]
                         end
                 end).

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
                    [{id, N},
                     {status, Status},
                     {ring_percentage, RingPercent},
                     {pending_percentage, NextPercent}];
                false ->
                    [{id, N},
                     {status, Status},
                     {ring_percentage, RingPercent},
                     {pending_percentage, null}]
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
        {nodes, lists:reverse(Nodes)},
        {valid, Valid},
        {leaving, Leaving},
        {exiting, Exiting},
        {joining, Joining},
        {down, Down}]}].

ringready(Node) ->
    handle_error(fun () ->
                         Response = remote(Node, riak_core_status, ringready, []),
                         case Response of
                             {ok, Nodes} ->
                                 [{control, [{ready, true},{nodes, Nodes}]}];
                             {error, {different_owners, N1, N2}} ->
                                 [{control, [{ready, false},{error, [{different_owners, [N1, N2]}]}]}];
                             {error, {nodes_down, Down}} ->
                                 [{control, [{ready, false},{error, [{nodes_down, Down}]}]}]
                         end
                 end).

datetime_str({_Mega, _Secs, _Micro}=Now) ->
    datetime_str(calendar:now_to_datetime(Now));
datetime_str({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    list_to_binary(lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                 [Year,Month,Day,Hour,Min,Sec]))).

get_active_transfers({{Mod, Partion}, Node, outbound, active, _Status}) ->
    {Mod, Partion, Node};
get_active_transfers({status_v2, Status}) ->
    Type = proplists:get_value(type, Status),
    Mod = proplists:get_value(mod, Status),
    SrcPartition = proplists:get_value(src_partition, Status),
    TargetPartition = proplists:get_value(target_partition, Status),
    StartTS = datetime_str(proplists:get_value(start_ts, Status)),
    SrcNode = proplists:get_value(src_node, Status),
    TargetNode = proplists:get_value(target_node, Status),
    Stats = case proplists:get_value(stats, Status) of
                no_stats ->
                    [no_stats];
                StatsPropList ->
                    ObjsS = proplists:get_value(objs_per_s, StatsPropList),
                    BytesS = proplists:get_value(bytes_per_s, StatsPropList),
                    LastUpdate = proplists:get_value(last_update, StatsPropList),
                    Objs = proplists:get_value(objs_total, StatsPropList),
                    Size = proplists:get_value(size, StatsPropList),
                    DonePctDecimal = proplists:get_value(pct_done_decimal, StatsPropList),
                    [{objs_per_s, ObjsS},
                     {bytes_per_s, BytesS},
                     {last_update, LastUpdate},
                     {objs_total, Objs},
                     {size, Size},
                     {pct_done_decimal, DonePctDecimal}]
            end,
    case Type of
        repair ->
            [{src_node, SrcNode},
             {target_node, TargetNode},
             {type, Type},
             {mod, Mod},
             {src_partition, SrcPartition},
             {target_partition, TargetPartition},
             {start_ts, StartTS},
             {stats, Stats}];
        _ ->
            [{src_node, SrcNode},
             {target_node, TargetNode},
             {type, Type},
             {mod, Mod},
             {target_partition, TargetPartition},
             {start_ts, StartTS},
             {stats, Stats}]
    end.

transfers(Node) ->
    handle_error(fun () ->
                         {Down, Pending} = remote(Node, riak_core_status, transfers, []),
                         F = fun({waiting_to_handoff, WaitingNode, Count}, {Waiting, Stopped}) ->
                                     {[{WaitingNode, Count} | Waiting], Stopped};
                                ({stopped, StoppedNode, Count}, {Waiting, Stopped}) ->
                                     {Waiting, [{StoppedNode, Count} | Stopped]}
                             end,
                         {Waiting, Stopped} = lists:foldl(F, {[], []}, Pending),
                         {Xfers, Down} = remote(Node, riak_core_status, all_active_transfers, []),
                         ActiveTransfers = [get_active_transfers(Xfer) || Xfer <- lists:flatten(Xfers)],
                         [{control, [{down, Down},
                                     {waiting_to_handoff, Waiting},
                                     {stopped, Stopped},
                                     {active, ActiveTransfers}]}]
                 end).

aae_status(Node) ->
    handle_error(fun () ->
                         ExchangeInfo = remote(Node, riak_kv_entropy_info, compute_exchange_info, []),
                         Exchanges = [[{index, Index},
                                       {last_ts, datetime_str(LastTS)},
                                       {all_ts, datetime_str(AllTS)}] || {Index, LastTS, AllTS, _Repairs} <- ExchangeInfo],
                         TreeInfo = remote(Node, riak_kv_entropy_info, compute_tree_info, []),
                         Trees = [[{index, Index},
                                   {built_ts, datetime_str(BuiltTS)}] || {Index, BuiltTS} <- TreeInfo],
                         KeysRepaired = [[{index, Index},
                                          {last, Last},
                                          {max, Max},
                                          {mean, Mean}] || {Index, _, _, {Last,_Min,Max,Mean}} <- ExchangeInfo],
                         [{control, [{exchanges, Exchanges},
                                     {entropy_trees, Trees},
                                     {keys_repaired, KeysRepaired}]}]
                 end).

%%%===================================================================
%%% Control -- Riak Repl API
%%%===================================================================
ensure_repl_available(Node) ->
    case remote(Node, code, is_loaded, [riak_repl_console]) of
        false -> throw(not_implemented);
        _ -> ok
    end.

repl_clustername(Node) ->
    handle_error(fun () ->
                         %% Exceptions vs return codes, oh my!
                         ensure_repl_available(Node),
                         ClusterName = list_to_binary(remote(Node, riak_core_connection, symbolic_clustername, [])),
                         [{control, [{clustername, ClusterName}]}]
                 end).

%% Note that this has the effect of outputting "Setting clustername to $clustername"
%% to stdout on the Riak process
repl_clustername(Node, ClusterName) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         %% The function expects the argument to be in a list
                         _ = remote(Node, riak_repl_console, clustername, [[atom_to_list(ClusterName)]]),
                         [{control, [{success, ok}]}]
                 end).

string_of_ipaddr({IP, Port}) ->
    list_to_binary(lists:flatten(io_lib:format("~s:~p", [IP, Port]))).

choose_best_addr({cluster_by_addr, {IP,Port}}, _ClientAddr) ->
    string_of_ipaddr({IP,Port});
choose_best_addr({cluster_by_name, _}, ClientAddr) ->
    string_of_ipaddr(ClientAddr).

string_of_remote({cluster_by_addr, {IP,Port}}) ->
    string_of_ipaddr({IP,Port});
string_of_remote({cluster_by_name, ClusterName}) when is_list(ClusterName) ->
    list_to_binary(ClusterName);
string_of_remote({cluster_by_name, ClusterName}) ->
    ClusterName;
%% Temporary, until bug is fixed in Riak Repl that returns just the cluster name instead of
%% the remote tuple
string_of_remote(ClusterName) when is_list(ClusterName) ->
    list_to_binary(ClusterName);
string_of_remote(ClusterName) ->
    ClusterName.

%% Get info about this sink
%% Remote :: {ip,port} | ClusterName
get_cluster_conn(Node, {Remote,Pid}) ->
    ConnName = string_of_remote(Remote),
    PidStr = list_to_binary(io_lib:format("~p", [Pid])),
    %% try to get status from Pid of cluster control channel.
    %% if we haven't connected successfully yet, it will time out, which we will fail
    %% fast for since it's a local process, not a remote one.
    try remote(Node, riak_core_cluster_conn, status, [Pid, 2]) of
        {Pid, status, {ClientAddr, _Transport, Name, Members}} ->
            IPs = [string_of_ipaddr(Addr) || Addr <- Members],
            CAddr = choose_best_addr(Remote, ClientAddr),
            NameStr = if is_list(Name) -> list_to_binary(Name); true -> Name end,
            [{connection_name, ConnName},
             {name, NameStr},
             {pid, PidStr},
             {ips, IPs},
             {caddr, CAddr}];
        {_StateName, SRemote} ->
            [{connection_name, ConnName},
             {name, ""},
             {pid, PidStr},
             {remote, string_of_remote(SRemote)}]
    catch
        'EXIT':{timeout, _} ->
            {badrpc, timeout}
    end.

repl_connections(Node) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         {ok, Conns} = remote(Node, riak_core_cluster_mgr, get_connections, []),
                         Connections = [get_cluster_conn(Node, Conn) || Conn <- Conns],
                         [{control, [{connections, Connections}]}]
                 end).

%% Note this has the effect of outputting a log message in the normal case
%% and error messages to the stdout of the Riak process if there are issues.
repl_connect(Node, Host, Port) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         _ = remote(Node, riak_repl_console, connect, [[atom_to_list(Host), atom_to_list(Port)]]),
                         [{control, [{success, ok}]}]
                 end).

repl_disconnect(Node, ClusterName) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         _ = remote(Node, riak_repl_console, disconnect, [[atom_to_list(ClusterName)]]),
                         [{control, [{success, ok}]}]
                 end).

repl_command(Node, Protocol, Command, ClusterName) ->
     handle_error(fun () ->
                          ensure_repl_available(Node),
                          Args = case ClusterName of
                                     [] -> [[Command]];
                                     ClusterName -> [[atom_to_list(ClusterName)]]
                                 end,
                         _ = remote(Node, riak_repl_console, Protocol, Args),
                         [{control, [{success, ok}]}]
                 end).

repl_realtime_enable(Node, ClusterName) ->
    repl_command(Node, realtime, "enable", ClusterName).

repl_realtime_disable(Node, ClusterName) ->
    repl_command(Node, realtime, "disable", ClusterName).

repl_realtime_start(Node, ClusterName) ->
    repl_command(Node, realtime, "start", ClusterName).

repl_realtime_start(Node) ->
    repl_command(Node, realtime, "start", []).

repl_realtime_stop(Node, ClusterName) ->
    repl_command(Node, realtime, "stop", ClusterName).

repl_realtime_stop(Node) ->
    repl_command(Node, realtime, "stop", []).

repl_fullsync_enable(Node, ClusterName) ->
    repl_command(Node, fullsync, "enable", ClusterName).

repl_fullsync_disable(Node, ClusterName) ->
    repl_command(Node, fullsync, "disable", ClusterName).

repl_fullsync_start(Node, ClusterName) ->
    repl_command(Node, fullsync, "start", ClusterName).

repl_fullsync_start(Node) ->
    repl_command(Node, fullsync, "start", []).

repl_fullsync_stop(Node, ClusterName) ->
    repl_command(Node, fullsync, "stop", ClusterName).

repl_fullsync_stop(Node) ->
    repl_command(Node, fullsync, "stop", []).

repl_clusterstats(Node) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         CMStats = remote(Node, riak_repl_console, cluster_mgr_stats, []),
                         CConnStats = remote(Node, riak_core_connection_mgr_stats, get_consolidated_stats, []),
                         [{control, [{clusterstats, CMStats ++ CConnStats}]}]
                 end).

repl_clusterstats(Node, Host, PortAtom) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         IP = atom_to_list(Host),
                         {Port, _Rest} = string:to_integer(atom_to_list(PortAtom)),
                         CConnStats = remote(Node, riak_core_connection_mgr_stats, get_stats_by_ip, [{IP,Port}]),
                         CMStats = remote(Node, riak_repl_console, cluster_mgr_stats, []),
                         [{control, [{clusterstats, CMStats ++ CConnStats}]}]
                 end).

clusterstats_protocol(Node, Protocol) ->
     handle_error(fun () ->
                          ensure_repl_available(Node),
                          CConnStats = remote(Node, riak_core_connection_mgr_stats, get_stats_by_protocol, [Protocol]),
                          CMStats = remote(Node, riak_repl_console, cluster_mgr_stats, []),
                          [{control, [{clusterstats, CMStats ++ CConnStats}]}]
                  end).

repl_clusterstats_cluster_mgr(Node) ->
    clusterstats_protocol(Node, cluster_mgr).

repl_clusterstats_fs_coordinate(Node) ->
    clusterstats_protocol(Node, fs_coordinate).

repl_clusterstats_fullsync(Node) ->
    clusterstats_protocol(Node, fullsync).

repl_clusterstats_proxy_get(Node) ->
    clusterstats_protocol(Node, proxy_get).

repl_clusterstats_realtime(Node) ->
    clusterstats_protocol(Node, realtime).

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

delete_bucket(Node, BucketType, Bucket) ->
    delete_bucket(Node, BucketType, Bucket, true).

delete_bucket(Node, BucketType, Bucket, RefreshCache) ->
    case re_config:development_mode() of
        true ->
            JobType = delete_bucket,
            Meta = {JobType, Node, [BucketType, Bucket, RefreshCache]},
            re_job_manager:create(JobType, {?MODULE, delete_bucket_job, [Meta]});
        false ->
            lager:warning("Failed request to delete types/~p/buckets/~p because developer mode is off", [BucketType, Bucket]),
            {error, developer_mode_off}
    end.

delete_bucket_job({delete_bucket, Node, [BucketType, Bucket, RefreshCache]}) ->
    C = client(Node),
    case re_keyjournal:cache_for_each({keys, Node, [BucketType, Bucket]},
            fun(Entry0, {Oks0, Errors0}) ->
                RT = list_to_binary(BucketType),
                RB = list_to_binary(Bucket),
                RK = list_to_binary(re:replace(Entry0, "(^\\s+)|(\\s+$)", "", [global,{return,list}])),
                {Oks1,Errors1} = case riakc_pb_socket:delete(C, {RT,RB}, RK) of
                    ok ->
                        riakc_pb_socket:get(C, {RT,RB}, RK),
                        {Oks0+1, Errors0};
                    {error, Reason} ->
                        lager:warning("Failed to delete types/~p/buckets/~p/keys/~p with reason ~p", [RT, RB, RK, Reason]),
                        {Oks0, Errors0+1}
                end,
                re_job_manager:set_meta(delete_bucket, [{oks, Oks1},{errors,Errors1}]),
                {Oks1,Errors1}
            end, [read], {0, 0}) of
        {error, not_found} ->
            lager:warning("Deletetion of types/~p/buckets/~p could not be completed because no cache was found", [BucketType, Bucket]),
            re_job_manager:error(delete_bucket, [{error, cache_not_found}]);
        {Os,Es} ->
            lager:info("Completed deletion of types/~p/buckets/~p with ~p successful deletes and ~p errors", [BucketType, Bucket, Os, Es]),
            re_job_manager:finish(delete_bucket),
            case RefreshCache of
                true ->
                    clean_buckets(Node, BucketType),
                    clean_keys(Node, BucketType, Bucket);
                    %% TODO: Want to list keys here eventually, but need to figure out
                    %% tombstone reaping
                false ->
                    ok
            end
    end.

%%%===================================================================
%%% Keyjournal API
%%%===================================================================

list_buckets(Node, BucketType) ->
    case re_config:development_mode() of
        true ->
            re_keyjournal:write({buckets, Node, [BucketType]});
        false ->
            {error, developer_mode_off}
    end.

list_buckets_cache(Node, BucketType, Start, Rows) ->
    re_keyjournal:read_cache({buckets, Node, [BucketType]}, Start, Rows).

clean_buckets(Node, BucketType) ->
    re_keyjournal:clean({buckets, Node, [BucketType]}).

put_buckets(Node, BucketType, Buckets) ->
    re_keyjournal:write_cache({buckets, Node, [BucketType]}, Buckets).

list_keys(Node, BucketType, Bucket) ->
    case re_config:development_mode() of
        true ->
            re_keyjournal:write({keys, Node, [BucketType, Bucket]});
        false ->
            {error, developer_mode_off}
    end.

list_keys_cache(Node, BucketType, Bucket, Start, Rows) ->
    re_keyjournal:read_cache({keys, Node, [BucketType, Bucket]}, Start, Rows).

clean_keys(Node, BucketType, Bucket) ->
    re_keyjournal:clean({keys, Node, [BucketType, Bucket]}).

put_keys(Node, BucketType, Bucket, Keys) ->
    re_keyjournal:write_cache({keys, Node, [BucketType, Bucket]}, Keys).

%%%===================================================================
%%% Node Properties API
%%%===================================================================

http_listener(Node) ->
    NodeStr = atom_to_list(Node),
    [_,Addr] = string:tokens(NodeStr, "@"),
    {ok,[{_,Port}]} = remote(Node, application, get_env, [riak_api, http]),
    [{http_listener, list_to_binary(Addr ++ ":" ++ integer_to_list(Port))}].

pb_listener(Node) ->
    NodeStr = atom_to_list(Node),
    [_,Addr] = string:tokens(NodeStr, "@"),
    {ok,[{_,Port}]} = remote(Node, application, get_env, [riak_api, pb]),
    [{pb_listener, list_to_binary(Addr ++ ":" ++ integer_to_list(Port))}].

bucket_types(Node) ->
    load_patch(Node),
    List = remote(Node, re_riak_patch, bucket_types, []),
    [{bucket_types, List}].

create_bucket_type(Node, BucketType, RawValue) ->
    % Props = case mochijson2:decode(RawValue) of
    %     {struct, [{<<"props", _/binary>>, {struct, Props1}}]} ->
    % Result = riak_core_bucket_type:create(Type, Props),
    case remote(Node, riak_kv_console, bucket_type_create, [[BucketType, RawValue]]) of
        ok ->
            case riak_core_bucket_type:activate(list_to_binary(BucketType)) of
                ok -> ok;
                {error, _} -> error
            end;
        error -> error
    end.

%%%===================================================================
%%% Cluster API
%%%===================================================================

cluster_id_for_node(Node) ->
    [{clusters, Clusters}] = clusters(),

    case find_cluster_by_node(Node, Clusters) of
        {error, not_found} -> undefined;
        [{id, Id}|_] -> Id
    end.

clusters() ->
    Clusters = re_config:clusters(),
    Mapped = lists:map(fun({C, _}) -> [{id,C}, {riak_node, re_config:riak_node(C)}, {development_mode, re_config:development_mode(C)}] end, Clusters),
    [{clusters, Mapped}].

cluster(Id) ->
    [{clusters, Clusters}] = clusters(),

    case find_cluster_by_id(Id, Clusters) of
        {error, not_found} -> {error, not_found};
        Props -> [{clusters, Props}]
    end.

first_node(Cluster) ->
    case nodes(Cluster) of
        [{nodes, [[{id, Node}]|_]}] -> Node;
        [{nodes, []}] -> [{error, no_nodes}];
        [{error, not_found}] -> [{error, no_nodes}]
    end.

nodes(Cluster) ->
    RiakNode = re_config:riak_node(Cluster),

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

node_is_alive(Node) ->
    Test = remote(Node, erlang, node, []),
    is_atom(Test).
%%%===================================================================
%%% Utility API
%%%===================================================================

load_patch(Node) ->
    IsLoaded = remote(Node, code, is_loaded, [re_riak_patch]),
    maybe_load_patch(Node, IsLoaded).

remote(N, M, F, A) ->
    safe_rpc(N, M, F, A, 60000).

%%%===================================================================
%%% Private
%%%===================================================================

maybe_load_patch(Node, false) ->
    lager:info("Loading re_riak_patch module into node[~p].", [Node]),
    {Mod, Bin, _} = code:get_object_code(re_riak_patch),
    remote(Node, code, load_binary, [Mod, "/tmp/re_riak_patch.beam", Bin]);
maybe_load_patch(Node, _) ->
    LocalVersion = re_riak_patch:version(),
    RemoteVersion = remote(Node, re_riak_patch, version, []),
    lager:info("Found version ~p of re_riak_patch on node[~p], current version is ~p.", [RemoteVersion, Node, LocalVersion]),
    case LocalVersion == RemoteVersion of
        false -> maybe_load_patch(Node, false);
        _ -> ok
    end.

safe_rpc(Node, Module, Function, Args, Timeout) ->
    try rpc:call(Node, Module, Function, Args, Timeout) of
        Result ->
            Result
    catch
        'EXIT':{noproc, _NoProcDetails} ->
            {badrpc, rpc_process_down}
    end.

handle_error(Action) ->
    try
        Action()
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason])),
            lager:info("~p:~p", [Exception, Reason]),
            lager:info("Backtrace: ~p", [erlang:get_stacktrace()]),
            [{control, [{error, Error}]}]
    end.

find_cluster_by_id(_, []) ->
    {error, not_found};
find_cluster_by_id(Id, [[{id, Id}|_]=Props|_]) ->
    Props;
find_cluster_by_id(Id, [_|Rest]) ->
    find_cluster_by_id(Id, Rest).

find_cluster_by_node(_, []) ->
    {error, not_found};
find_cluster_by_node(Node, [[_,{riak_node,Node}|_]=Props|_]) ->
    Props;
find_cluster_by_node(Node, [[{id, Cluster}|_]=Props|Rest]) ->
    case node_exists(Cluster, Node) of
        true -> Props;
        false -> find_cluster_by_node(Node, Rest)
    end.
