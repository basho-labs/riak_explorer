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

-export([routes/0]).

-export([command0_exists/1,
         command0/1,
         command1_exists/1,
         command1/1,
         command2_exists/1,
         command2/1]).

-define(BASE, "control").
-define(
   CONTROL_BASE, 
   [
    [?BASE, "clusters", cluster],
    [?BASE, "nodes", node]
   ]).
                   
-define(
   COMMAND0_ROUTES, 
   [
    ["repair"],
    ["staged-leave"],
    ["plan"],
    ["commit"],
    ["clear"],
    ["status"],
    ["ringready"],
    ["transfers"],
    ["aae-status"],
    ["repl-clustername"],
    ["repl-connections"],
    ["repl-clusterstats"],
    ["repl-clusterstats-cluster_mgr"],
    ["repl-clusterstats-fs_coordinate"],
    ["repl-clusterstats-fullsync"],
    ["repl-clusterstats-proxy_get"],
    ["repl-clusterstats-realtime"],
    ["repl-realtime-start"],
    ["repl-realtime-stop"],
    ["repl-fullsync-start"],
    ["repl-fullsync-stop"]]).

-define(
   COMMAND1_ROUTES, 
   [
    ["join", arg1],
    ["leave", arg1],
    ["staged-join", arg1],
    ["staged-leave", arg1],
    ["force-remove", arg1],
    ["repl-clustername", arg1],
    ["repl-disconnect", arg1],
    ["repl-realtime-enable", arg1],
    ["repl-realtime-disable", arg1],
    ["repl-realtime-start", arg1],
    ["repl-realtime-stop", arg1],
    ["repl-fullsync-enable", arg1],
    ["repl-fullsync-disable", arg1],
    ["repl-fullsync-start", arg1],
    ["repl-fullsync-stop", arg1]]).

-define(
   COMMAND2_ROUTES, 
   [
    ["replace", arg1, arg2],
    ["staged-replace", arg1, arg2],
    ["force-replace", arg1, arg2],
    ["repl-connect", arg1, arg2],
    ["repl-clusterstats", arg1, arg2]]).

-include_lib("webmachine/include/webmachine.hrl").
-include("re_wm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

routes() ->
    [%% Base
     #route{base=?CONTROL_BASE, 
            path=?COMMAND0_ROUTES,
            exists={?MODULE, command0_exists},
            content={?MODULE,command0}},
     #route{base=?CONTROL_BASE, 
            path=?COMMAND1_ROUTES, 
            exists={?MODULE, command1_exists},
            content={?MODULE,command1}},
     #route{base=?CONTROL_BASE, 
            path=?COMMAND0_ROUTES, 
            exists={?MODULE, command2_exists},
            content={?MODULE,command2}}
    ].

%%%===================================================================
%%% Callbacks
%%%===================================================================

command0_exists(ReqData) ->
    Command = rd_command(ReqData),
    case re_wm:rd_node_exists(ReqData) of
        {true, _} ->
            {lists:member([Command], ?COMMAND0_ROUTES), ReqData};
        _ ->
            {false, ReqData}
    end.
    

command0(ReqData) ->
    Node = re_wm:rd_node(ReqData),
    Command = rd_command(ReqData),
    Response = 
        case Command of
            "repair" -> re_node_control:repair(Node);
            "staged-leave" -> re_node_control:staged_leave(Node);
            "plan" -> re_node_control:plan(Node);
            "commit" -> re_node_control:commit(Node);
            "clear" -> re_node_control:clear(Node);
            "status" -> re_node_control:status(Node);
            "ringready" -> re_node_control:ringready(Node);
            "transfers" -> re_node_control:transfers(Node);
            "aae-status" -> re_node_control:aae_status(Node);
            "repl-clustername" -> re_node_control:repl_clustername(Node);
            "repl-connections" -> re_node_control:repl_connections(Node);
            "repl-realtime-start" -> re_node_control:repl_realtime_start(Node);
            "repl-realtime-stop" -> re_node_control:repl_realtime_stop(Node);
            "repl-fullsync-start" -> re_node_control:repl_fullsync_start(Node);
            "repl-fullsync-stop" -> re_node_control:repl_fullsync_stop(Node);
            "repl-clusterstats" -> re_node_control:repl_clusterstats(Node);
            "repl-clusterstats-cluster_mgr" -> re_node_control:repl_clusterstats_cluster_mgr(Node);
            "repl-clusterstats-fs_coordinate" -> re_node_control:repl_clusterstats_fs_coordinate(Node);
            "repl-clusterstats-fullsync" -> re_node_control:repl_clusterstats_fullsync(Node);
            "repl-clusterstats-proxy_get" -> re_node_control:repl_clusterstats_proxy_get(Node);
            "repl-clusterstats-realtime" -> re_node_control:repl_clusterstats_realtime(Node);
            _ -> {error, not_found}
        end,
    re_wm:rd_content(Response, ReqData).

command1_exists(ReqData) ->
    Command = rd_command(ReqData),
    case re_wm:rd_node_exists(ReqData) of
        {true, _} ->
            {lists:member([Command, arg1], ?COMMAND1_ROUTES), ReqData};
        _ ->
            {false, ReqData}
    end.

command1(ReqData) ->
    Node = re_wm:rd_node(ReqData),
    Arg1 = rd_arg1(ReqData),
    Command = rd_command(ReqData),
    Response = 
        case Command of
            "join" -> re_node_control:join(Node, Arg1);
            "staged-join" -> re_node_control:staged_join(Node, Arg1);
            "leave" -> re_node_control:leave(Node, Arg1);
            "staged-leave" -> re_node_control:staged_leave(Node, Arg1);
            "force-remove" -> re_node_control:force_remove(Node, Arg1);
            "repl-clustername" -> re_node_control:repl_clustername(Node, Arg1);
            "repl-disconnect" -> re_node_control:repl_disconnect(Node, Arg1);
            "repl-realtime-enable" -> re_node_control:repl_realtime_enable(Node, Arg1);
            "repl-realtime-disable" -> re_node_control:repl_realtime_disable(Node, Arg1);
            "repl-realtime-start" -> re_node_control:repl_realtime_start(Node, Arg1);
            "repl-realtime-stop" -> re_node_control:repl_realtime_stop(Node, Arg1);
            "repl-fullsync-enable" -> re_node_control:repl_fullsync_enable(Node, Arg1);
            "repl-fullsync-disable" -> re_node_control:repl_fullsync_disable(Node, Arg1);
            "repl-fullsync-start" -> re_node_control:repl_fullsync_start(Node, Arg1);
            "repl-fullsync-stop" -> re_node_control:repl_fullsync_stop(Node, Arg1);
            _ -> {error, not_found}
        end,
    re_wm:rd_content(Response, ReqData).

command2_exists(ReqData) ->
    Command = rd_command(ReqData),
    case re_wm:rd_node_exists(ReqData) of
        {true, _} ->
            {lists:member([Command, arg1, arg2], ?COMMAND2_ROUTES), ReqData};
        _ ->
            {false, ReqData}
    end.

command2(ReqData) ->
    Node = re_wm:rd_node(ReqData),
    Arg1 = rd_arg1(ReqData),
    Arg2 = rd_arg2(ReqData),
    Command = rd_command(ReqData),
    Response = 
        case Command of
            "staged-replace" -> re_node_control:staged_replace(Node, Arg1, Arg2);
            "replace" -> re_node_control:replace(Node, Arg1, Arg2);
            "force-replace" -> re_node_control:force_replace(Node, Arg1, Arg2);
            "repl-connect" -> re_node_control:repl_connect(Node, Arg1, Arg2);
            "repl-clusterstats" -> re_node_control:repl_clusterstats(Node, Arg1, Arg2);
            _ -> {error, not_found}
        end,
    re_wm:rd_content(Response, ReqData).

%% ====================================================================
%% Private
%% ====================================================================

rd_command(ReqData) ->
    case riak_explorer:is_riak() of
        true ->
            lists:nth(5, string:tokens(wrq:path(ReqData), "/"));
        false ->
            lists:nth(4, string:tokens(wrq:path(ReqData), "/"))
    end.

rd_arg1(ReqData) ->
    re_wm:maybe_atomize(re_wm:url_decode(wrq:path_info(arg1, ReqData))).

rd_arg2(ReqData) ->
    re_wm:maybe_atomize(re_wm:url_decode(wrq:path_info(arg2, ReqData))).
