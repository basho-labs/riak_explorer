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

-module(re_wm_bucket_type).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         accept_content/2,
         provide_japi_content/2,
         provide_json_content/2]).

-record(ctx, {cluster, node, bucket_type, resource, id, method, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(noNode(Error),
    #ctx{node=[_]=Error}).
-define(listBucketTypes(Node),
    #ctx{node=Node, method='GET', bucket_type=undefined}).
-define(bucketTypeInfo(Node, BucketType),
    #ctx{node=Node, method='GET', bucket_type=BucketType, resource=undefined}).
-define(createBucketType(Node, BucketType),
    #ctx{node=Node, method='PUT', bucket_type=BucketType, resource=undefined}).
-define(bucketTypeResource(Node, BucketType, Resource),
    #ctx{node=Node, method='GET', bucket_type=BucketType, resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [{jobs, [riak_explorer, jobs_for_resource]}].

routes() ->
    re_config:build_routes(?RE_BASE_ROUTE, [
        ["clusters", cluster],
        ["nodes", node]
    ], [
        ["bucket_types"],
        ["bucket_types", bucket_type],
        ["bucket_types", bucket_type, resource]
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
        resource = wrq:path_info(resource, RD),
        bucket_type = wrq:path_info(bucket_type, RD),
        cluster = Cluster,
        method = wrq:method(RD),
        node = re_wm_util:node_from_context(Cluster, Node)
    }}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET', 'PUT'],
    {Methods, RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    Types = [{"application/json", accept_content},
             {"application/vnd.api+json", accept_content},
             {"application/octet-stream", accept_content}],
    {Types, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_json_content},
             {"application/vnd.api+json", provide_japi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?noNode(Error)) ->
    set_response(RD, Ctx, error, Error);
resource_exists(RD, Ctx=?listBucketTypes(Node)) ->
    set_response(RD, Ctx, bucket_types, re_riak:bucket_types(Node));
resource_exists(RD, Ctx=?createBucketType(_,_)) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=?bucketTypeInfo(Node, BucketType)) ->
    Id = list_to_binary(BucketType),
    set_response(RD, Ctx, BucketType, re_riak:bucket_type(Node, Id));
resource_exists(RD, Ctx=?bucketTypeResource(Node, BucketType, Resource)) ->
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] ->
            set_response(RD, Ctx, Id, M:F(Node, BucketType));
        _ ->
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

accept_content(RD, Ctx=?createBucketType(Node, BucketType)) ->
    RawValue = wrq:req_body(RD),
    case re_riak:create_bucket_type(Node, BucketType, RawValue) of
        [{error, _, Message}] ->
            re_wm_util:halt_json(500, Message, RD, Ctx);
        Response ->
            Json = re_wm_util:provide_content(json, RD, list_to_binary(BucketType), Response),
            re_wm_util:halt(200, [{<<"Content-Type">>, <<"application/json">>}], Json, RD, Ctx)
    end;
accept_content(RD, Ctx) ->
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
