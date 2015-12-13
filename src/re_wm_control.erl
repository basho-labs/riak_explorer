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

-module(re_wm_control).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_japi_content/2,
         provide_json_content/2]).

-record(ctx, {cluster, node, command, arg1, arg2, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(command(Node, Command),
    #ctx{node=Node,command=Command,arg1=undefined,arg2=undefined}).
-define(command(Node, Command, Arg1),
    #ctx{node=Node,command=Command,arg1=Arg1,arg2=undefined}).
-define(command(Node, Command, Arg1, Arg2),
    #ctx{node=Node,command=Command,arg1=Arg1,arg2=Arg2}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [].

routes() ->
    re_config:build_routes(?RE_CONTROL_ROUTE, [
        ["clusters", cluster],
        ["nodes", node]
    ], [
        ["repair"],
        ["join", arg1],
        ["leave", arg1],
        ["staged-join", arg1],
        ["staged-leave"],
        ["staged-leave", arg1],
        ["force-remove", arg1],
        ["replace", arg1, arg2],
        ["staged-replace", arg1, arg2],
        ["force-replace", arg1, arg2],
        ["plan"],
        ["commit"],
        ["clear"],
        ["status"],
        ["ringready"],
        ["transfers"],
        ["aae-status"],
        ["repl-clustername"],
        ["repl-clustername", arg1],
        ["repl-connect", arg1, arg2],
        ["repl-disconnect", arg1],
        ["repl-connections"],
        ["repl-clusterstats"],
        ["repl-clusterstats", arg1, arg2],
        ["repl-clusterstats-cluster_mgr"],
        ["repl-clusterstats-fs_coordinate"],
        ["repl-clusterstats-fullsync"],
        ["repl-clusterstats-proxy_get"],
        ["repl-clusterstats-realtime"],
        ["repl-realtime-enable", arg1],
        ["repl-realtime-disable", arg1],
        ["repl-realtime-start"],
        ["repl-realtime-start", arg1],
        ["repl-realtime-stop"],
        ["repl-realtime-stop", arg1],
        ["repl-fullsync-enable", arg1],
        ["repl-fullsync-disable", arg1],
        ["repl-fullsync-start"],
        ["repl-fullsync-start", arg1],
        ["repl-fullsync-stop"],
        ["repl-fullsync-stop", arg1]
    ]).

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx) ->
    Cluster = re_wm_util:maybe_atomize(wrq:path_info(cluster, RD)),
    Node = re_wm_util:maybe_atomize(wrq:path_info(node, RD)),

    {true, RD, Ctx#ctx{
        cluster = Cluster,
        command = lists:nth(length(re_config:base_route("")) + 3, string:tokens(wrq:path(RD), "/")),
        arg1 = re_wm_util:maybe_atomize(wrq:path_info(arg1, RD)),
        arg2 = re_wm_util:maybe_atomize(wrq:path_info(arg2, RD)),
        node = re_wm_util:node_from_context(Cluster, Node)
    }}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_json_content},
 {"application/vnd.api+json", provide_japi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?command(Node, Command)) ->
    Response = case Command of
        "repair" -> re_riak:repair(Node);
        "staged-leave" -> re_riak:staged_leave(Node);
        "plan" -> re_riak:plan(Node);
        "commit" -> re_riak:commit(Node);
        "clear" -> re_riak:clear(Node);
        "status" -> re_riak:status(Node);
        "ringready" -> re_riak:ringready(Node);
        "transfers" -> re_riak:transfers(Node);
        "aae-status" -> re_riak:aae_status(Node);
        "repl-clustername" -> re_riak:repl_clustername(Node);
        "repl-connections" -> re_riak:repl_connections(Node);
        "repl-realtime-start" -> re_riak:repl_realtime_start(Node);
        "repl-realtime-stop" -> re_riak:repl_realtime_stop(Node);
        "repl-fullsync-start" -> re_riak:repl_fullsync_start(Node);
        "repl-fullsync-stop" -> re_riak:repl_fullsync_stop(Node);
        "repl-clusterstats" -> re_riak:repl_clusterstats(Node);
        "repl-clusterstats-cluster_mgr" -> re_riak:repl_clusterstats_cluster_mgr(Node);
        "repl-clusterstats-fs_coordinate" -> re_riak:repl_clusterstats_fs_coordinate(Node);
        "repl-clusterstats-fullsync" -> re_riak:repl_clusterstats_fullsync(Node);
        "repl-clusterstats-proxy_get" -> re_riak:repl_clusterstats_proxy_get(Node);
        "repl-clusterstats-realtime" -> re_riak:repl_clusterstats_realtime(Node);
        _ -> [{error, not_found}]
    end,
    set_response(RD, Ctx, list_to_atom(Command), Response);
resource_exists(RD, Ctx=?command(Node, Command, Arg1)) ->
    Node = Ctx#ctx.node,
    Response = case Command of
        "join" -> re_riak:join(Node, Arg1);
        "staged-join" -> re_riak:staged_join(Node, Arg1);
        "leave" -> re_riak:leave(Node, Arg1);
        "staged-leave" -> re_riak:staged_leave(Node, Arg1);
        "force-remove" -> re_riak:force_remove(Node, Arg1);
        "repl-clustername" -> re_riak:repl_clustername(Node, Arg1);
        "repl-disconnect" -> re_riak:repl_disconnect(Node, Arg1);
        "repl-realtime-enable" -> re_riak:repl_realtime_enable(Node, Arg1);
        "repl-realtime-disable" -> re_riak:repl_realtime_disable(Node, Arg1);
        "repl-realtime-start" -> re_riak:repl_realtime_start(Node, Arg1);
        "repl-realtime-stop" -> re_riak:repl_realtime_stop(Node, Arg1);
        "repl-fullsync-enable" -> re_riak:repl_fullsync_enable(Node, Arg1);
        "repl-fullsync-disable" -> re_riak:repl_fullsync_disable(Node, Arg1);
        "repl-fullsync-start" -> re_riak:repl_fullsync_start(Node, Arg1);
        "repl-fullsync-stop" -> re_riak:repl_fullsync_stop(Node, Arg1);
        _ -> [{error, not_found}]
    end,
    set_response(RD, Ctx, list_to_atom(Command), Response);
resource_exists(RD, Ctx=?command(Node, Command, Arg1, Arg2)) ->
    Response = case Command of
        "staged-replace" -> re_riak:staged_replace(Node, Arg1, Arg2);
        "replace" -> re_riak:replace(Node, Arg1, Arg2);
        "force-replace" -> re_riak:force_replace(Node, Arg1, Arg2);
        "repl-connect" -> re_riak:repl_connect(Node, Arg1, Arg2);
        "repl-clusterstats" -> re_riak:repl_clusterstats(Node, Arg1, Arg2);
        _ -> [{error, not_found}]
    end,
    set_response(RD, Ctx, list_to_atom(Command), Response);
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

provide_json_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(json, RD, Id, Response), RD, Ctx}.

provide_japi_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(jsonapi, RD, Id, Response), RD, Ctx}.

%% ====================================================================
%% Private
%% ====================================================================

set_response(RD, Ctx, Id, Response) ->
    re_wm_util:resource_exists(RD, Ctx#ctx{id=Id, response=Response}, Response).
