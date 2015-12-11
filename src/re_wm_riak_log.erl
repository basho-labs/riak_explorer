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

-module(re_wm_riak_log).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_text_content/2,
         provide_json_content/2,
         provide_japi_content/2]).

-record(ctx, {node, rows, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(noNode(),
    #ctx{node=undefined}).
-define(listFiles(Node),
    #ctx{node=Node, id=undefined}).
-define(getFile(Node, File, Rows),
    #ctx{node=Node, id=File, rows=Rows}).

%%%===================================================================
%%% API
%%%===================================================================

resources() -> [].

routes() ->
    re_config:build_routes(?RE_BASE_ROUTE, [
        ["clusters", cluster, "nodes", node],
        ["nodes", node]
    ],[
        ["log", "files"],
        ["log", "files", file]
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
        rows = list_to_integer(wrq:get_qs_value("rows","1000",RD)),
        id = wrq:path_info(file, RD),
        node = re_wm_util:node_from_context(Cluster, Node)
    }}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_json_content},
             {"plain/text", provide_text_content},
             {"application/vnd.api+json", provide_japi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?noNode()) ->
    {false, RD, Ctx};
resource_exists(RD, Ctx=?listFiles(Node)) ->
    set_response(RD, Ctx, files, re_riak:log_files(Node));
resource_exists(RD, Ctx=?getFile(Node, File, Rows)) ->
    set_response(RD, Ctx, files, re_riak:log_file(Node, File, Rows));
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

provide_text_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(text, RD, Id, Response), RD, Ctx}.

provide_json_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(json, RD, Id, Response), RD, Ctx}.

provide_japi_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(jsonapi, RD, Id, Response), RD, Ctx}.

%% ====================================================================
%% Private
%% ====================================================================

set_response(RD, Ctx, Id, Response) ->
    re_wm_util:resource_exists(RD, Ctx#ctx{id=Id, response=Response}, Response).
