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

-module(re_wm_control).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_jsonapi_content/2,
         provide_content/2]).

-record(ctx, {cluster, node, command, arg1, arg2, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(command(Command), #ctx{command=Command,arg1=undefined,arg2=undefined}).
-define(command(Command, Arg1), #ctx{command=Command,arg1=Arg1,arg2=undefined}).
-define(command(Command, Arg1, Arg2), #ctx{command=Command,arg1=Arg1,arg2=Arg2}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [].

routes() ->
    Base = re_config:base_route(?RE_CONTROL_ROUTE),

    Clusters         = Base ++ ["clusters"],
    Cluster          = Clusters ++ [cluster],
    CRepair          = Cluster ++ ["repair"],
    CJoin            = Cluster ++ ["join"] ++ [arg1],
    CLeave2          = Cluster ++ ["leave"] ++ [arg1],
    CSJoin           = Cluster ++ ["staged-join"] ++ [arg1],
    CSLeave          = Cluster ++ ["staged-leave"],
    CSLeave2         = Cluster ++ ["staged-leave"] ++ [arg1],
    CForceRemove     = Cluster ++ ["force-remove"] ++ [arg1],
    CReplace         = Cluster ++ ["replace"] ++ [arg1] ++ [arg2],
    CSReplace        = Cluster ++ ["staged-replace"] ++ [arg1] ++ [arg2],
    CForceReplace    = Cluster ++ ["force-replace"] ++ [arg1] ++ [arg2],
    CPlan            = Cluster ++ ["plan"],
    CCommit          = Cluster ++ ["commit"],
    CClear           = Cluster ++ ["clear"],
    CStatus          = Cluster ++ ["status"],
    CRingReady       = Cluster ++ ["ringready"],
    CTransfers       = Cluster ++ ["transfers"],
    CAAEStatus       = Cluster ++ ["aae-status"],
    CClusterName     = Cluster ++ ["repl-clustername"],
    CClusterName2    = Cluster ++ ["repl-clustername"] ++ [arg1],
    CConnect         = Cluster ++ ["repl-connect"] ++ [arg1] ++ [arg2],
    CDisconnect      = Cluster ++ ["repl-disconnect"] ++ [arg1],
    CConnections     = Cluster ++ ["repl-connections"],
    CClusterStats    = Cluster ++ ["repl-clusterstats"],
    CClusterStats2   = Cluster ++ ["repl-clusterstats"] ++ [arg1] ++ [arg2],
    CClusterStatsCM  = Cluster ++ ["repl-clusterstats-cluster_mgr"],
    CClusterStatsFSC = Cluster ++ ["repl-clusterstats-fs_coordinate"],
    CClusterStatsFS  = Cluster ++ ["repl-clusterstats-fullsync"],
    CClusterStatsPG  = Cluster ++ ["repl-clusterstats-proxy_get"],
    CClusterStatsRT  = Cluster ++ ["repl-clusterstats-realtime"],
    CRealtimeEnable  = Cluster ++ ["repl-realtime-enable"] ++ [arg1],
    CRealtimeDisable = Cluster ++ ["repl-realtime-disable"] ++ [arg1],
    CRealtimeStart   = Cluster ++ ["repl-realtime-start"],
    CRealtimeStart2  = Cluster ++ ["repl-realtime-start"] ++ [arg1],
    CRealtimeStop    = Cluster ++ ["repl-realtime-stop"],
    CRealtimeStop2   = Cluster ++ ["repl-realtime-stop"] ++ [arg1],
    CFullsyncEnable  = Cluster ++ ["repl-fullsync-enable"] ++ [arg1],
    CFullsyncDisable = Cluster ++ ["repl-fullsync-disable"] ++ [arg1],
    CFullsyncStart   = Cluster ++ ["repl-fullsync-start"],
    CFullsyncStart2  = Cluster ++ ["repl-fullsync-start"] ++ [arg1],
    CFullsyncStop    = Cluster ++ ["repl-fullsync-stop"],
    CFullsyncStop2   = Cluster ++ ["repl-fullsync-stop"] ++ [arg1],

    Nodes           = Base ++ ["nodes"],
    Node            = Nodes ++ [node],
    Repair          = Node ++ ["repair"],
    Join            = Node ++ ["join"] ++ [arg1],
    Leave2          = Node ++ ["leave"] ++ [arg1],
    SJoin           = Node ++ ["staged-join"] ++ [arg1],
    SLeave          = Node ++ ["staged-leave"],
    SLeave2         = Node ++ ["staged-leave"] ++ [arg1],
    ForceRemove     = Node ++ ["force-remove"] ++ [arg1],
    Replace         = Node ++ ["replace"] ++ [arg1] ++ [arg2],
    SReplace        = Node ++ ["staged-replace"] ++ [arg1] ++ [arg2],
    ForceReplace    = Node ++ ["force-replace"] ++ [arg1] ++ [arg2],
    Plan            = Node ++ ["plan"],
    Commit          = Node ++ ["commit"],
    Clear           = Node ++ ["clear"],
    Status          = Node ++ ["status"],
    RingReady       = Node ++ ["ringready"],
    Transfers       = Node ++ ["transfers"],
    AAEStatus       = Node ++ ["aae-status"],
    ClusterName     = Node ++ ["repl-clustername"],
    ClusterName2    = Node ++ ["repl-clustername"] ++ [arg1],
    Connect         = Node ++ ["repl-connect"] ++ [arg1] ++ [arg2],
    Disconnect      = Node ++ ["repl-disconnect"] ++ [arg1],
    Connections     = Node ++ ["repl-connections"],
    ClusterStats    = Node ++ ["repl-clusterstats"],
    ClusterStats2   = Node ++ ["repl-clusterstats"] ++ [arg1] ++ [arg2],
    ClusterStatsCM  = Node ++ ["repl-clusterstats-cluster_mgr"],
    ClusterStatsFSC = Node ++ ["repl-clusterstats-fs_coordinate"],
    ClusterStatsFS  = Node ++ ["repl-clusterstats-fullsync"],
    ClusterStatsPG  = Node ++ ["repl-clusterstats-proxy_get"],
    ClusterStatsRT  = Node ++ ["repl-clusterstats-realtime"],
    RealtimeEnable  = Node ++ ["repl-realtime-enable"] ++ [arg1],
    RealtimeDisable = Node ++ ["repl-realtime-disable"] ++ [arg1],
    RealtimeStart   = Node ++ ["repl-realtime-start"],
    RealtimeStart2  = Node ++ ["repl-realtime-start"] ++ [arg1],
    RealtimeStop    = Node ++ ["repl-realtime-stop"],
    RealtimeStop2   = Node ++ ["repl-realtime-stop"] ++ [arg1],
    FullsyncEnable  = Node ++ ["repl-fullsync-enable"] ++ [arg1],
    FullsyncDisable = Node ++ ["repl-fullsync-disable"] ++ [arg1],
    FullsyncStart   = Node ++ ["repl-fullsync-start"],
    FullsyncStart2  = Node ++ ["repl-fullsync-start"] ++ [arg1],
    FullsyncStop    = Node ++ ["repl-fullsync-stop"],
    FullsyncStop2   = Node ++ ["repl-fullsync-stop"] ++ [arg1],

    [CRepair, CJoin,CLeave2,CSJoin,CSLeave,CSLeave2,CForceRemove,CReplace,CSReplace,
     CForceReplace,CPlan,CCommit,CClear,CStatus,CRingReady,CTransfers,CAAEStatus,
     CClusterName,CClusterName2,CConnect,CDisconnect,CConnections,
     CClusterStats,CClusterStats2,CClusterStatsCM,CClusterStatsFSC,CClusterStatsFS,CClusterStatsPG,CClusterStatsRT,
     CRealtimeEnable,CRealtimeDisable,CRealtimeStart,CRealtimeStart2,CRealtimeStop,CRealtimeStop2,
     CFullsyncEnable,CFullsyncDisable,CFullsyncStart,CFullsyncStart2,CFullsyncStop,CFullsyncStop2] ++
    [Repair, Join,Leave2,SJoin,SLeave,SLeave2,ForceRemove,Replace,SReplace,
     ForceReplace,Plan,Commit,Clear,Status,RingReady,Transfers,AAEStatus,
     ClusterName,ClusterName2,Connect,Disconnect,Connections,
     ClusterStats,ClusterStats2,ClusterStatsCM,ClusterStatsFSC,ClusterStatsFS,ClusterStatsPG,ClusterStatsRT,
     RealtimeEnable,RealtimeDisable,RealtimeStart,RealtimeStart2,RealtimeStop,RealtimeStop2,
     FullsyncEnable,FullsyncDisable,FullsyncStart,FullsyncStart2,FullsyncStop,FullsyncStop2].

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx0) ->
    Ctx1 = Ctx0#ctx{
        node = wrq:path_info(node, RD),
        cluster = wrq:path_info(cluster, RD),
        command = lists:nth(length(re_config:base_route("")) + 3, string:tokens(wrq:path(RD), "/")),
        arg1 = maybe_atomize(wrq:path_info(arg1, RD)),
        arg2 = maybe_atomize(wrq:path_info(arg2, RD))
    },
    {true, RD, Ctx1#ctx{node = node_from_context(Ctx1)}}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content},
             {"application/vnd.api+json", provide_jsonapi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?command(Command)) ->
    Node = Ctx#ctx.node,
    {Exists, Response} = case Command of
        "repair" ->
            {true, re_riak:repair(Node)};
        "staged-leave" ->
            {true, re_riak:staged_leave(Node)};
        "plan" ->
            {true, re_riak:plan(Node)};
        "commit" ->
            {true, re_riak:commit(Node)};
        "clear" ->
            {true, re_riak:clear(Node)};
        "status" ->
            {true, re_riak:status(Node)};
        "ringready" ->
            {true, re_riak:ringready(Node)};
        "transfers" ->
            {true, re_riak:transfers(Node)};
        "aae-status" ->
            {true, re_riak:aae_status(Node)};
        "repl-clustername" ->
            {true, re_riak:repl_clustername(Node)};
        "repl-connections" ->
            {true, re_riak:repl_connections(Node)};
        "repl-realtime-start" ->
            {true, re_riak:repl_realtime_start(Node)};
        "repl-realtime-stop" ->
            {true, re_riak:repl_realtime_stop(Node)};
        "repl-fullsync-start" ->
            {true, re_riak:repl_fullsync_start(Node)};
        "repl-fullsync-stop" ->
            {true, re_riak:repl_fullsync_stop(Node)};
        "repl-clusterstats" ->
            {true, re_riak:repl_clusterstats(Node)};
        "repl-clusterstats-cluster_mgr" ->
            {true, re_riak:repl_clusterstats_cluster_mgr(Node)};
        "repl-clusterstats-fs_coordinate" ->
            {true, re_riak:repl_clusterstats_fs_coordinate(Node)};
        "repl-clusterstats-fullsync" ->
            {true, re_riak:repl_clusterstats_fullsync(Node)};
        "repl-clusterstats-proxy_get" ->
            {true, re_riak:repl_clusterstats_proxy_get(Node)};
        "repl-clusterstats-realtime" ->
            {true, re_riak:repl_clusterstats_realtime(Node)};
        _ -> {false, undefined}
    end,
    {Exists, RD, Ctx#ctx{id=list_to_atom(Command), response=Response}};
resource_exists(RD, Ctx=?command(Command, Arg1)) ->
    Node = Ctx#ctx.node,
    {Exists, Response} = case Command of
        "join" ->
            {true, re_riak:join(Node, Arg1)};
        "staged-join" ->
            {true, re_riak:staged_join(Node, Arg1)};
        "leave" ->
            {true, re_riak:leave(Node, Arg1)};
        "staged-leave" ->
            {true, re_riak:staged_leave(Node, Arg1)};
        "force-remove" ->
            {true, re_riak:force_remove(Node, Arg1)};
        "repl-clustername" ->
            {true, re_riak:repl_clustername(Node, Arg1)};
        "repl-disconnect" ->
            {true, re_riak:repl_disconnect(Node, Arg1)};
        "repl-realtime-enable" ->
            {true, re_riak:repl_realtime_enable(Node, Arg1)};
        "repl-realtime-disable" ->
            {true, re_riak:repl_realtime_disable(Node, Arg1)};
        "repl-realtime-start" ->
            {true, re_riak:repl_realtime_start(Node, Arg1)};
        "repl-realtime-stop" ->
            {true, re_riak:repl_realtime_stop(Node, Arg1)};
        "repl-fullsync-enable" ->
            {true, re_riak:repl_fullsync_enable(Node, Arg1)};
        "repl-fullsync-disable" ->
            {true, re_riak:repl_fullsync_disable(Node, Arg1)};
        "repl-fullsync-start" ->
            {true, re_riak:repl_fullsync_start(Node, Arg1)};
        "repl-fullsync-stop" ->
            {true, re_riak:repl_fullsync_stop(Node, Arg1)};
        _ -> {false, undefined}
    end,
    {Exists, RD, Ctx#ctx{id=list_to_atom(Command), response=Response}};
resource_exists(RD, Ctx=?command(Command, Arg1, Arg2)) ->
    Node = Ctx#ctx.node,
    {Exists, Response} = case Command of
        "staged-replace" ->
            {true, re_riak:staged_replace(Node, Arg1, Arg2)};
        "replace" ->
            {true, re_riak:replace(Node, Arg1, Arg2)};
        "force-replace" ->
            {true, re_riak:force_replace(Node, Arg1, Arg2)};
        "repl-connect" ->
            {true, re_riak:repl_connect(Node, Arg1, Arg2)};
        "repl-clusterstats" ->
            {true, re_riak:repl_clusterstats(Node, Arg1, Arg2)};
        _ -> {false, undefined}
    end,
    {Exists, RD, Ctx#ctx{id=list_to_atom(Command), response=Response}};
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

provide_content(RD, Ctx=#ctx{response=undefined}) ->
    JDoc = re_wm_jsonapi:doc(RD, data, null, re_wm_jsonapi:links(RD, "/explore/routes"), [], []),
    render_json(JDoc, RD, Ctx);
provide_content(RD, Ctx=#ctx{id=Id, response=[{_, Objects}]}) ->
    JRes = re_wm_jsonapi:res(RD, [], Objects, [], []),
    JDoc = re_wm_jsonapi:doc(RD, Id, JRes, [], [], []),
    render_json(JDoc, RD, Ctx).

provide_jsonapi_content(RD, Ctx=#ctx{response=undefined}) ->
    JDoc = re_wm_jsonapi:doc(RD, data, null, re_wm_jsonapi:links(RD, "/explore/routes"), [], []),
    render_json(JDoc, RD, Ctx);
provide_jsonapi_content(RD, Ctx=#ctx{id=Id, response=[{Type, Objects}]}) ->
    JRes = re_wm_jsonapi:res(RD, Type, Objects, [], []),
    JDoc = re_wm_jsonapi:doc(RD, Id, JRes, [], [], []),
    render_json(JDoc, RD, Ctx).

%% ====================================================================
%% Private
%% ====================================================================

maybe_atomize(Data) when is_list(Data) -> list_to_atom(Data);
maybe_atomize(Data) when is_atom(Data) -> Data.

node_from_context(Ctx) ->
    case Ctx of
        #ctx{cluster=undefined, node=N} ->
            Node = list_to_atom(N),
            re_config:set_adhoc_cluster(Node),
            Node;
        #ctx{cluster=C} -> re_riak:first_node(list_to_atom(C))
    end.

render_json(Data, RD, Ctx) ->
    Body = mochijson2:encode(Data),
    {Body, RD, Ctx}.
