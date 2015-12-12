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

-module(re_wm_bucket).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         delete_resource/2,
         accept_content/2,
         provide_japi_content/2,
         provide_json_content/2]).

-record(ctx, {method, start, rows, cluster, node, bucket_type, bucket, resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(noNode(),
    #ctx{node=[{error, no_nodes}]}).
-define(putBuckets(Node, BucketType),
    #ctx{node=Node, method='PUT', bucket_type=BucketType, bucket=undefined}).
-define(cleanBuckets(Node, BucketType),
    #ctx{node=Node, method='DELETE', bucket_type=BucketType, bucket=undefined}).
-define(listBuckets(Node, BucketType),
    #ctx{node=Node, method='POST', bucket_type=BucketType, bucket=undefined}).
-define(listBucketsCache(Node, BucketType),
    #ctx{node=Node, method='GET', bucket_type=BucketType, bucket=undefined}).
-define(bucketInfo(Node, BucketType, Bucket),
    #ctx{node=Node, method='GET', bucket_type=BucketType, bucket=Bucket, resource=undefined}).
-define(deleteBucket(Node, BucketType, Bucket),
    #ctx{node=Node, method='DELETE', bucket_type=BucketType, bucket=Bucket, resource=undefined}).
-define(bucketResource(Node, BucketType, Bucket, Resource),
    #ctx{node=Node, method='GET', bucket_type=BucketType, bucket=Bucket, resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [{jobs, [riak_explorer, jobs_for_resource]}].

routes() ->
    re_config:build_routes(?RE_BASE_ROUTE, [
        ["clusters", cluster, "bucket_types", bucket_type],
        ["nodes", node, "bucket_types", bucket_type]
    ], [
        ["refresh_buckets", "source", "riak_kv"],
        ["buckets"],
        ["buckets", bucket],
        ["buckets", bucket, resource]
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
        bucket = wrq:path_info(bucket, RD),
        node = re_wm_util:node_from_context(Cluster, Node),
        cluster = Cluster,
        method = wrq:method(RD),
        start = list_to_integer(wrq:get_qs_value("start","0",RD)),
        rows = list_to_integer(wrq:get_qs_value("rows","1000",RD))
    }}.

allowed_methods(RD, Ctx) ->
    Methods = ['POST', 'GET', 'PUT', 'DELETE'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_json_content},
             {"application/vnd.api+json", provide_japi_content}],
    {Types, RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    Types = [{"application/json", accept_content},
             {"application/vnd.api+json", accept_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?noNode()) ->
    {false, RD, Ctx};
resource_exists(RD, Ctx=?putBuckets(_Node, _BucketType)) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=?cleanBuckets(Node, BucketType)) ->
    re_riak:clean_buckets(Node, BucketType),
    {true, RD, Ctx};
resource_exists(RD, Ctx=?listBucketsCache(Node, BucketType)) ->
    set_response(RD, Ctx, buckets,
        re_riak:list_buckets_cache(Node, BucketType, Ctx#ctx.start, Ctx#ctx.rows));
resource_exists(RD, Ctx=?listBuckets(Node, BucketType)) ->
    JobsPath = string:substr(wrq:path(RD),1, string:str(wrq:path(RD), "refresh_buckets") - 1) ++ "jobs",
    re_wm_util:set_jobs_response(RD, Ctx, JobsPath,
        re_riak:list_buckets(Node, BucketType));
resource_exists(RD, Ctx=?deleteBucket(Node, BucketType, Bucket)) ->
    JobsPath = wrq:path(RD) ++ "/jobs",
    re_wm_util:set_jobs_response(RD, Ctx, JobsPath,
        re_riak:delete_bucket(Node, BucketType, Bucket));
resource_exists(RD, Ctx=?bucketInfo(_Node, _BucketType, Bucket)) ->
    Id = list_to_binary(Bucket),
    Response = [{buckets, [{id,Id}, {props, []}]}],
    {true, RD, Ctx#ctx{id=Id, response=Response}};
resource_exists(RD, Ctx=?bucketResource(Node, BucketType, Bucket, Resource)) ->
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] ->
            set_response(RD, Ctx, Id, M:F(Node, BucketType, Bucket));
        _ ->
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

delete_resource(RD, Ctx) ->
    {true, RD, Ctx}.

accept_content(RD, Ctx=?putBuckets(Node, BucketType)) ->
    RawValue = wrq:req_body(RD),
    {struct, [{<<"buckets">>, Buckets}]} = mochijson2:decode(RawValue),
    re_riak:put_buckets(Node, BucketType, Buckets),
    {true, RD, Ctx}.

provide_json_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(json, RD, Id, Response), RD, Ctx}.

provide_japi_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(jsonapi, RD, Id, Response), RD, Ctx}.

%% ====================================================================
%% Private
%% ====================================================================

set_response(RD, Ctx, Id, Response) ->
    re_wm_util:resource_exists(RD, Ctx#ctx{id=Id, response=Response}, Response).
