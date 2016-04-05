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

-module(re_wm_table).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         delete_resource/2,
         accept_content/2,
         provide_json_content/2]).

-record(ctx, {method, cluster, node, table, data, is_query, id, resource, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(noNode(Error),
    #ctx{node=[_]=Error}).
-define(listTables(Node),
    #ctx{node=Node, method='GET', table=undefined, is_query=false}).
-define(tableInfo(Node, Table),
    #ctx{node=Node, method='GET', table=Table, is_query=false}).
-define(putRows(Node, Table, Rows),
    #ctx{node=Node, method='PUT', table=Table, data=Rows, is_query=false}).
-define(queryAll(Node, Query),
    #ctx{node=Node, method='POST', table=undefined, data=Query, is_query=true}).
-define(queryTable(Node, Table, Query),
    #ctx{node=Node, method='POST', table=Table, data=Query, is_query=true}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [].

routes() ->
    re_config:build_routes(?RE_BASE_ROUTE, [
        ["clusters", cluster],
        ["nodes", node]
    ], [
        ["tables"],
        ["tables", "query"],
        ["tables", table],
        ["tables", table, "query"]
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
    IsQuery = string:str(wrq:path(RD), "query") > 0,
    
    {true, RD, Ctx#ctx{
        resource = wrq:path_info(resource, RD),
        table = wrq:path_info(table, RD),
        data = wrq:req_body(RD),
        node = re_wm_util:node_from_context(Cluster, Node),
        cluster = Cluster,
        method = wrq:method(RD),
        is_query = IsQuery
    }}.

allowed_methods(RD, Ctx) ->
    Methods = ['POST', 'GET', 'PUT'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_json_content}],
    {Types, RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    Types = [{"application/json", accept_content},
             {"plain/text", accept_content},
             {"application/octet-stream", accept_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?noNode(Error)) ->
    set_response(RD, Ctx, error, Error);
resource_exists(RD, Ctx=?listTables(Node)) ->
    set_response(RD, Ctx, tables, re_riak:tables(Node));
resource_exists(RD, Ctx=?tableInfo(Node, Table)) ->
    Id = list_to_binary(Table),
    set_response(RD, Ctx, Table, re_riak:table(Node, Id));
resource_exists(RD, Ctx=?putRows(_, _, _)) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=?queryAll(Node, Query)) ->
    Body = re_wm_util:provide_content(json, RD, query, 
                                      re_riak:query_ts(Node, Query)),
    re_wm_util:halt(
      200, [{<<"Content-Type">>, <<"application/json">>}],
      Body, RD, Ctx);
resource_exists(RD, Ctx=?queryTable(Node, Table, Query)) ->
    Body = re_wm_util:provide_content(json, RD, query, 
                                 re_riak:get_ts(Node, Table, 
                                                mochijson2:decode(Query))),
    re_wm_util:halt(
      200, [{<<"Content-Type">>, <<"application/json">>}],
      Body, RD, Ctx);
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

delete_resource(RD, Ctx) ->
    {true, RD, Ctx}.

accept_content(RD, Ctx=?putRows(Node, Table, RawRows)) ->
    Rows = mochijson2:decode(RawRows),
    case re_riak:put_ts(Node, Table, Rows) of
        [{ts, [{success, true}]}] ->
            {true, RD, Ctx};
        _ ->
            {true, RD, Ctx}
    end;
accept_content(RD, Ctx) ->
    {false, RD, Ctx}.

provide_json_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(json, RD, Id, Response), RD, Ctx}.

%% ====================================================================
%% Private
%% ====================================================================

set_response(RD, Ctx, Id, Response) ->
    re_wm_util:resource_exists(RD, Ctx#ctx{id=Id, response=Response}, Response).
