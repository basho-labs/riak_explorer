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

-record(ctx, {cluster, node, command, node1, node2, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(command(Command), #ctx{command=Command,node1=undefined,node2=undefined}).
-define(command(Command, Node1), #ctx{command=Command,node1=Node1,node2=undefined}).
-define(command(Command, Node1, Node2), #ctx{command=Command,node1=Node1,node2=Node2}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [].

routes() ->
    Base = [?RE_CONTROL_ROUTE],

    Clusters       = Base ++ ["clusters"],
    Cluster        = Clusters ++ [cluster],
    CRepair        = Cluster ++ ["repair"],
    CJoin          = Cluster ++ ["join"] ++ [node1],
    CLeave2        = Cluster ++ ["leave"] ++ [node1],
    CSJoin         = Cluster ++ ["staged-join"] ++ [node1],
    CSLeave        = Cluster ++ ["staged-leave"],
    CSLeave2       = Cluster ++ ["staged-leave"] ++ [node1],
    CForceRemove   = Cluster ++ ["force-remove"] ++ [node1],
    CReplace       = Cluster ++ ["replace"] ++ [node1] ++ [node2],
    CSReplace      = Cluster ++ ["staged-replace"] ++ [node1] ++ [node2],
    CForceReplace  = Cluster ++ ["force-replace"] ++ [node1] ++ [node2],
    CPlan          = Cluster ++ ["plan"],
    CCommit        = Cluster ++ ["commit"],
    CClear         = Cluster ++ ["clear"],
    CStatus        = Cluster ++ ["status"],
    CRingReady     = Cluster ++ ["ringready"],

    Nodes         = Base ++ ["nodes"],
    Node          = Nodes ++ [node],
    Repair        = Node ++ ["repair"],
    Join          = Node ++ ["join"] ++ [node1],
    Leave2        = Node ++ ["leave"] ++ [node1],
    SJoin         = Node ++ ["staged-join"] ++ [node1],
    SLeave        = Node ++ ["staged-leave"],
    SLeave2       = Node ++ ["staged-leave"] ++ [node1],
    ForceRemove   = Node ++ ["force-remove"] ++ [node1],
    Replace       = Node ++ ["replace"] ++ [node1] ++ [node2],
    SReplace      = Node ++ ["staged-replace"] ++ [node1] ++ [node2],
    ForceReplace  = Node ++ ["force-replace"] ++ [node1] ++ [node2],
    Plan          = Node ++ ["plan"],
    Commit        = Node ++ ["commit"],
    Clear         = Node ++ ["clear"],
    Status        = Node ++ ["status"],
    RingReady     = Node ++ ["ringready"],

    [CRepair, CJoin,CLeave2,CSJoin,CSLeave,CSLeave2,CForceRemove,CReplace,CSReplace,
     CForceReplace,CPlan,CCommit,CClear,CStatus,CRingReady] ++
    [Repair, Join,Leave2,SJoin,SLeave,SLeave2,ForceRemove,Replace,SReplace,
     ForceReplace,Plan,Commit,Clear,Status,RingReady].

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
        command = lists:nth(4, string:tokens(wrq:path(RD), "/")),
        node1 = maybe_atomize(wrq:path_info(node1, RD)),
        node2 = maybe_atomize(wrq:path_info(node2, RD))
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
        _ -> {false, undefined}
    end,
    {Exists, RD, Ctx#ctx{id=list_to_atom(Command), response=Response}};
resource_exists(RD, Ctx=?command(Command, Node1)) ->
    Node = Ctx#ctx.node,
    {Exists, Response} = case Command of
        "join" ->
            {true, re_riak:join(Node, Node1)};
        "staged-join" ->
            {true, re_riak:staged_join(Node, Node1)};
        "leave" ->
            {true, re_riak:leave(Node, Node1)};
        "staged-leave" ->
            {true, re_riak:staged_leave(Node, Node1)};
        "force-remove" ->
            {true, re_riak:force_remove(Node, Node1)};
        _ -> {false, undefined}
    end,
    {Exists, RD, Ctx#ctx{id=list_to_atom(Command), response=Response}};
resource_exists(RD, Ctx=?command(Command, Node1, Node2)) ->
    Node = Ctx#ctx.node,
    {Exists, Response} = case Command of
        "staged-replace" ->
            {true, re_riak:staged_replace(Node, Node1, Node2)};
        "replace" ->
            {true, re_riak:replace(Node, Node1, Node2)};
        "force-replace" ->
            {true, re_riak:force_replace(Node, Node1, Node2)};
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
        #ctx{cluster=undefined, node=N} -> list_to_atom(N);
        #ctx{cluster=C} -> re_riak:first_node(list_to_atom(C))
    end.

render_json(Data, RD, Ctx) ->
    Body = mochijson2:encode(Data),
    {Body, RD, Ctx}.
