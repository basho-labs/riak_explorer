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

-module(re_node_control).

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

%%%===================================================================
%%% API
%%%===================================================================

-spec repair(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repair(Node) ->
    case re_node:command(Node, riak_core_ring_manager, get_my_ring, []) of
        {error, Reason} ->
            {error, Reason};
        {ok, Ring} ->
             case re_node:command(Node, riak_core_ring, all_owners, [Ring]) of
                 {error, Reason1} ->
                     {error, Reason1};
                 AllOwners ->
                     F1 = fun({P, N}, Acc) ->
                                  case N of
                                      Node -> [P|Acc];
                                      _ -> Acc
                                  end
                          end,
                     Partitions = lists:foldl(F1, [], AllOwners),
                     Results = [re_node:command(Node, riak_kv_vnode, repair, [P]) || P <- Partitions],
                     F2 = fun(R, [{success, S},{failure, F}]) ->
                                  case R of
                                      {ok,_} -> [{success, S+1},{failure, F}];
                                      _ -> [{success, S},{failure, F+1}]
                                  end
                          end,
                     lists:foldl(F2, [{success, 0},{failure, 0}], Results)
             end
    end.
    
-spec force_replace(re_node:re_node(), re_node:re_node(), re_node:re_node()) -> 
                           {error, term()} | [{atom(), term()}].
force_replace(Node, Node1, Node2) ->
    handle_error(fun () ->
                         Response = re_node:command(Node, riak_core_claimant, force_replace, [Node1, Node2]),
                         case Response of
                             ok ->
                                 [{success, true}];
                             {error, R} ->
                                 {error, R}
                         end
                 end).

-spec replace(re_node:re_node(), re_node:re_node(), re_node:re_node()) -> 
                     {error, term()} | [{atom(), term()}].
replace(Node, Node1, Node2) ->
    case force_replace(Node, Node1, Node2) of
        {error, Reason} ->
            {error, Reason};
        _ ->
            case plan(Node) of
                {error, Reason1} ->
                    {error, Reason1};
                _ ->
                    commit(Node)
            end
    end.
                            
-spec staged_replace(re_node:re_node(), re_node:re_node(), re_node:re_node()) -> 
                            {error, term()} | [{atom(), term()}].
staged_replace(Node, Node1, Node2) ->
    handle_error(fun () ->
                         Response = re_node:command(Node, riak_core_claimant, replace, [Node1, Node2]),
                         case Response of
                             ok ->
                                 [{success, true}];
                             {error, R} ->
                                 {error, R}
                         end
                 end).

-spec join(re_node:re_node(), re_node:re_node()) -> 
                  {error, term()} | [{atom(), term()}].
join(Node, Node1) ->
    Response = re_node:command(Node, riak_core, join, [Node1]),
    case Response of
        {error, Reason} -> {error, Reason};
        ok -> [{success, true}]
    end.

-spec staged_join(re_node:re_node(), re_node:re_node()) -> 
                  {error, term()} | [{atom(), term()}].
staged_join(Node, Node1) ->
    Response = re_node:command(Node, riak_core, staged_join, [Node1]),
    case Response of
        {error, Reason} -> {error, Reason};
        ok -> [{success, true}]
    end.

-spec force_remove(re_node:re_node(), re_node:re_node()) -> 
                          {error, term()} | [{atom(), term()}].
force_remove(Node, Node1) ->
    leave(Node, Node1).

-spec leave(re_node:re_node(), re_node:re_node()) -> 
                   {error, term()} | [{atom(), term()}].
leave(Node, Node1) ->
    handle_error(fun () ->
                         Response = re_node:command(Node, riak_core, remove, [Node1]),
                         case Response of
                             ok ->
                                 [{success, true}];
                             {error, R} ->
                                 {error, R}
                         end
                 end).

-spec staged_leave(re_node:re_node()) -> 
                          {error, term()} | [{atom(), term()}].
staged_leave(Node) ->
    staged_leave(Node, Node).

-spec staged_leave(re_node:re_node(), re_node:re_node()) -> 
                          {error, term()} | [{atom(), term()}].
staged_leave(Node, Node1) ->
    handle_error(fun () ->
                         Response = re_node:command(Node, riak_core_claimant, leave_member, [Node1]),
                         case Response of
                             ok ->
                                 [{success, true}];
                             {error, R} ->
                                 {error, R}
                         end
                 end).

-spec plan(re_node:re_node()) -> 
                  {error, term()} | [{atom(), term()}].
plan(Node) ->
    Response = re_node:command(Node, riak_core_claimant, plan, []),
    case Response of
        {error, Reason} ->
            {error, Reason};
        {ok, Changes, _} ->
            [{changes, 
              lists:map(
                fun({N, {Action, Target}}) ->
                        [{node, N},{action, Action},{target, Target}];
                   ({N, Action}) ->
                        [{node, N},{action, Action},{target, null}]
                end, Changes)}]
    end.

-spec commit(re_node:re_node()) -> 
                    {error, term()} | [{atom(), term()}].
commit(Node) ->
    Response = re_node:command(Node, riak_core_claimant, commit, []),
    case Response of
        ok -> [{success, true}];
        {error, Reason} -> {error, Reason};
        _ -> {error, retry_plan}
    end.

-spec clear(re_node:re_node()) -> 
                   {error, term()} | [{atom(), term()}].
clear(Node) ->
    handle_error(fun () ->
                         Response = re_node:command(Node, riak_core_claimant, clear, []),
                         case Response of
                             ok -> [{success, true}];
                             {error, Reason} -> {error, Reason};
                             _ -> {error, retry_clear}
                         end
                 end).

-spec status(re_node:re_node()) -> 
                    {error, term()} | [{atom(), term()}].
status(Node) ->
    case re_node:command(Node, riak_core_ring_manager, get_my_ring, []) of
        {error, Reason} ->
            {error, Reason};
        {ok, Ring} ->
            case re_node:command(Node, riak_core_ring, all_member_status, [Ring]) of
                {error, Reason1} ->
                    {error, Reason1};
                AS ->
                    AllStatus = lists:keysort(2, AS),
                    IsPending = ([] =/= re_node:command(Node, riak_core_ring, pending_changes, [Ring])),
                    {Nodes, Joining, Valid, Down, Leaving, Exiting} =
                        lists:foldl(
                          fun({N, Status}, {Nodes0, Joining0, Valid0, Down0, Leaving0, Exiting0}) ->
                                  {RingPercent, NextPercent} = 
                                      case re_node:command(Node, riak_core_console, pending_claim_percentage, [Ring, N]) of
                                          {error, _} -> {null, null};
                                          {RP, NP} -> {RP, NP};
                                          _ -> {null, null}
                                      end,
                                  NodeObj = 
                                      case IsPending of
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
                    [
                     {nodes, lists:reverse(Nodes)},
                     {valid, Valid},
                     {leaving, Leaving},
                     {exiting, Exiting},
                     {joining, Joining},
                     {down, Down}]
            end
    end.

-spec ringready(re_node:re_node()) -> 
                       {error, term()} | [{atom(), term()}].
ringready(Node) ->
    handle_error(fun () ->
                         Response = re_node:command(Node, riak_core_status, ringready, []),
                         case Response of
                             {ok, Nodes} ->
                                 [{ready, true},{nodes, Nodes}];
                             {error, {different_owners, N1, N2}} ->
                                 [{ready, false},{error, [{different_owners, [N1, N2]}]}];
                             {error, {nodes_down, Down}} ->
                                 [{ready, false},{error, [{nodes_down, Down}]}];
                             {error, Reason} ->
                                 {error, Reason}
                         end
                 end).

-spec transfers(re_node:re_node()) -> 
                       {error, term()} | [{atom(), term()}].
transfers(Node) ->
    handle_error(fun () ->
                         case re_node:command(Node, riak_core_status, transfers, []) of
                             {error, Reason} ->
                                 {error, Reason};
                             {Down, Pending} ->
                                 F = fun({waiting_to_handoff, WaitingNode, Count}, {Waiting, Stopped}) ->
                                             {[{WaitingNode, Count} | Waiting], Stopped};
                                        ({stopped, StoppedNode, Count}, {Waiting, Stopped}) ->
                                             {Waiting, [{StoppedNode, Count} | Stopped]}
                                     end,
                                 {Waiting, Stopped} = lists:foldl(F, {[], []}, Pending),
                                 case re_node:command(Node, riak_core_status, all_active_transfers, []) of
                                     {error, Reason} ->
                                         {error, Reason};
                                     {Xfers, Down} ->
                                         ActiveTransfers = [get_active_transfers(Xfer) || Xfer <- lists:flatten(Xfers)],
                                         [{down, Down},
                                          {waiting_to_handoff, Waiting},
                                          {stopped, Stopped},
                                          {active, ActiveTransfers}]
                                 end
                         end
                 end).

-spec aae_status(re_node:re_node()) -> 
                        {error, term()} | [{atom(), term()}].
aae_status(Node) ->
    handle_error(fun () ->
                         case re_node:command(Node, riak_kv_entropy_info, compute_exchange_info, []) of
                             {error, Reason} ->
                                 {error, Reason};
                             ExchangeInfo ->
                                 Exchanges = 
                                     [[{index, Index},
                                       {last_ts, datetime_str(LastTS)},
                                       {all_ts, datetime_str(AllTS)}] 
                                      || {Index, LastTS, AllTS, _Repairs} <- ExchangeInfo],
                                 case re_node:command(Node, riak_kv_entropy_info, compute_tree_info, []) of
                                     {error, Reason} ->
                                         {error, Reason};
                                     TreeInfo ->
                                         Trees = [[{index, Index},
                                                   {built_ts, datetime_str(BuiltTS)}] 
                                                  || {Index, BuiltTS} <- TreeInfo],
                                         KeysRepaired = [[{index, Index},
                                                          {last, Last},
                                                          {max, Max},
                                                          {mean, Mean}] 
                                                         || {Index, _, _, {Last,_Min,Max,Mean}} <- ExchangeInfo],
                                         [{exchanges, Exchanges},
                                          {entropy_trees, Trees},
                                          {keys_repaired, KeysRepaired}]
                                 end
                         end
                 end).


%%% Riak Repl API

-spec repl_clustername(re_node:re_node()) ->
                              {error, term()} | [{atom(), term()}].
repl_clustername(Node) ->
    handle_error(fun () ->
                         %% Exceptions vs return codes, oh my!
                         ensure_repl_available(Node),
                         case re_node:command(Node, riak_core_connection, symbolic_clustername, []) of
                             {error, Reason} ->
                                 {error, Reason};
                             ClusterName ->
                                 [{clustername, list_to_binary(ClusterName)}]
                         end
                 end).

%% Note that this has the effect of outputting "Setting clustername to $clustername"
%% to stdout on the Riak process
-spec repl_clustername(re_node:re_node(), atom()) ->
                              {error, term()} | [{atom(), term()}].
repl_clustername(Node, ClusterName) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         %% The function expects the argument to be in a list
                         case re_node:command(Node, riak_repl_console, clustername, [[atom_to_list(ClusterName)]]) of
                             {error, Reason} ->
                                 {error, Reason};
                             _ ->
                                 [{success, true}]
                         end
                 end).

-spec repl_connections(re_node:re_node()) ->
                              {error, term()} | [{atom(), term()}].
repl_connections(Node) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         case re_node:command(Node, riak_core_cluster_mgr, get_connections, []) of
                             {error, Reason} ->
                                 {error, Reason};
                             {ok, Conns} ->
                                 Connections = [get_cluster_conn(Node, Conn) || Conn <- Conns],
                                 [{connections, Connections}]
                         end
                 end).

%% Note this has the effect of outputting a log message in the normal case
%% and error messages to the stdout of the Riak process if there are issues.
-spec repl_connect(re_node:re_node(), atom(), atom()) ->
                          {error, term()} | [{atom(), term()}].
repl_connect(Node, Host, Port) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         case re_node:command(Node, riak_repl_console, connect, [[atom_to_list(Host), atom_to_list(Port)]]) of
                             {error, Reason} ->
                                 {error, Reason};
                             _ ->
                                 [{success, true}]
                         end
                 end).

-spec repl_disconnect(re_node:re_node(), atom()) ->
                             {error, term()} | [{atom(), term()}].
repl_disconnect(Node, ClusterName) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         case re_node:command(Node, riak_repl_console, disconnect, [[atom_to_list(ClusterName)]]) of
                             {error, Reason} ->
                                 {error, Reason};
                             _ ->
                                 [{success, true}]
                         end
                 end).

-spec repl_realtime_enable(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_realtime_enable(Node, ClusterName) ->
    repl_command(Node, realtime, "enable", ClusterName).

-spec repl_realtime_disable(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_realtime_disable(Node, ClusterName) ->
    repl_command(Node, realtime, "disable", ClusterName).

-spec repl_realtime_start(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_realtime_start(Node, ClusterName) ->
    repl_command(Node, realtime, "start", ClusterName).

-spec repl_realtime_start(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_realtime_start(Node) ->
    repl_command(Node, realtime, "start", []).

-spec repl_realtime_stop(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_realtime_stop(Node, ClusterName) ->
    repl_command(Node, realtime, "stop", ClusterName).

-spec repl_realtime_stop(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_realtime_stop(Node) ->
    repl_command(Node, realtime, "stop", []).

-spec repl_fullsync_enable(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_fullsync_enable(Node, ClusterName) ->
    repl_command(Node, fullsync, "enable", ClusterName).

-spec repl_fullsync_disable(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_fullsync_disable(Node, ClusterName) ->
    repl_command(Node, fullsync, "disable", ClusterName).

-spec repl_fullsync_start(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_fullsync_start(Node, ClusterName) ->
    repl_command(Node, fullsync, "start", ClusterName).

-spec repl_fullsync_start(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_fullsync_start(Node) ->
    repl_command(Node, fullsync, "start", []).

-spec repl_fullsync_stop(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
repl_fullsync_stop(Node, ClusterName) ->
    repl_command(Node, fullsync, "stop", ClusterName).

-spec repl_fullsync_stop(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_fullsync_stop(Node) ->
    repl_command(Node, fullsync, "stop", []).

-spec repl_clusterstats(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_clusterstats(Node) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         combine_clusterstats(
                            Node,
                            re_node:command(Node, riak_core_connection_mgr_stats, get_consolidated_stats, []))
                 end).

-spec repl_clusterstats(re_node:re_node(), atom(), atom()) -> {error, term()} | [{atom(), term()}].
repl_clusterstats(Node, Host, PortAtom) ->
    handle_error(fun () ->
                         ensure_repl_available(Node),
                         IP = atom_to_list(Host),
                         {Port, _Rest} = string:to_integer(atom_to_list(PortAtom)),
                         combine_clusterstats(
                            Node,
                            re_node:command(Node, riak_core_connection_mgr_stats, get_stats_by_ip, [{IP,Port}]))
                 end).

-spec repl_clusterstats_cluster_mgr(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_clusterstats_cluster_mgr(Node) ->
    clusterstats_protocol(Node, cluster_mgr).

-spec repl_clusterstats_fs_coordinate(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_clusterstats_fs_coordinate(Node) ->
    clusterstats_protocol(Node, fs_coordinate).

-spec repl_clusterstats_fullsync(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_clusterstats_fullsync(Node) ->
    clusterstats_protocol(Node, fullsync).

-spec repl_clusterstats_proxy_get(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_clusterstats_proxy_get(Node) ->
    clusterstats_protocol(Node, proxy_get).

-spec repl_clusterstats_realtime(re_node:re_node()) -> {error, term()} | [{atom(), term()}].
repl_clusterstats_realtime(Node) ->
    clusterstats_protocol(Node, realtime).

%%%===================================================================
%%% Private
%%%===================================================================

-spec handle_error(fun()) -> {error, term()} | term().
handle_error(Action) ->
    try
        Action()
    catch
        Exception:Reason ->
            Error = list_to_binary(io_lib:format("~p:~p, Check console.log.", [Exception,Reason])),
            lager:info("~p:~p", [Exception, Reason]),
            lager:info("Backtrace: ~p", [erlang:get_stacktrace()]),
            {error, Error}
    end.

-spec datetime_str(undefined | 
                   {integer(), integer(), integer()} | 
                   {{integer(), integer(), integer()},{integer(), integer(), integer()}}) -> 
                          null | binary().
datetime_str(undefined) ->
    null;
datetime_str({_Mega, _Secs, _Micro}=Now) ->
    datetime_str(calendar:now_to_datetime(Now));
datetime_str({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    list_to_binary(lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                 [Year,Month,Day,Hour,Min,Sec]))).

-spec get_active_transfers(term()) -> {term(), term(), term()} |
                                      [{atom(), term()}].
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
                    LastUpdate = datetime_str(proplists:get_value(last_update, StatsPropList)),
                    Objs = proplists:get_value(objs_total, StatsPropList),
                    Size = case proplists:get_value(size, StatsPropList) of
                               undefined -> undefined;
                               {S, _} -> S
                           end,
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

-spec ensure_repl_available(re_node:re_node()) ->
                                   ok.
ensure_repl_available(Node) ->
    case re_node:command(Node, re_riak_patch, is_enterprise, []) of
        {error, Reason} ->
            throw(Reason);
        false ->
            throw(not_implemented);
        _ -> ok
    end.

-spec string_of_ipaddr({term(), term()}) -> binary().
string_of_ipaddr({IP, Port}) ->
    list_to_binary(lists:flatten(io_lib:format("~s:~p", [IP, Port]))).

-spec choose_best_addr({atom(), term()}, term()) -> binary() | atom().
choose_best_addr({cluster_by_addr, {IP,Port}}, _ClientAddr) ->
    string_of_ipaddr({IP,Port});
choose_best_addr({cluster_by_name, _}, ClientAddr) ->
    string_of_ipaddr(ClientAddr).

-spec string_of_remote({atom(), term()}) -> binary() | atom().
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
-spec get_cluster_conn(re_node:re_node(), {term(), pid()}) ->
                              {error, term()} | [{atom(), term()}].
get_cluster_conn(Node, {Remote,Pid}) ->
    ConnName = string_of_remote(Remote),
    PidStr = list_to_binary(io_lib:format("~p", [Pid])),
    %% try to get status from Pid of cluster control channel.
    %% if we haven't connected successfully yet, it will time out, which we will fail
    %% fast for since it's a local process, not a remote one.
    case re_node:command(Node, riak_core_cluster_conn, status, [Pid, 2]) of
        {error, Reason} ->
            {error, Reason};
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
    end.

-spec repl_command(re_node:re_node(), atom(), string(), atom()) ->
                          {error, term()} | [{atom(), term()}].
repl_command(Node, Protocol, Command, ClusterName) ->
     handle_error(fun () ->
                          ensure_repl_available(Node),
                          Args = case ClusterName of
                                     [] -> [[Command]];
                                     ClusterName -> [[Command, atom_to_list(ClusterName)]]
                                 end,
                         case re_node:command(Node, riak_repl_console, Protocol, Args) of
                             {error, Reason} ->
                                 {error, Reason};
                             _ ->
                                 [{success, true}]
                         end
                 end).

-spec clusterstats_protocol(re_node:re_node(), atom()) -> {error, term()} | [{atom(), term()}].
clusterstats_protocol(Node, Protocol) ->
     handle_error(fun () ->
                          ensure_repl_available(Node),
                          combine_clusterstats(
                            Node,
                            re_node:command(Node, riak_core_connection_mgr_stats, get_stats_by_protocol, [Protocol]))
                  end).

-spec combine_clusterstats(re_node:re_node(), {error, term()} | [term()]) ->
                                  {error, term()} | [{atom(), [term()]}].
combine_clusterstats(_Node, {error, Reason}) ->
    {error, Reason};
combine_clusterstats(Node, Stats1) ->
    case re_node:command(Node, riak_repl_console, cluster_mgr_stats, []) of
        {error, Reason} ->
            {error, Reason};
        [_|_]=CMStats ->
            [{clusterstats, CMStats ++ Stats1}]
    end.
