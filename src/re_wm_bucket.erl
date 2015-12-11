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
         provide_jsonapi_content/2,
         provide_content/2]).

-record(ctx, {method, start, rows, cluster, node, bucket_type, bucket, resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(noNode(),
    #ctx{node=[{error, no_nodes}]}).
-define(putBuckets(BucketType),
    #ctx{method='PUT', bucket_type=BucketType, bucket=undefined}).
-define(cleanBuckets(BucketType),
    #ctx{method='DELETE', bucket_type=BucketType, bucket=undefined}).
-define(listBuckets(BucketType),
    #ctx{method='POST', bucket_type=BucketType, bucket=undefined}).
-define(listBucketsCache(BucketType),
    #ctx{method='GET', bucket_type=BucketType, bucket=undefined}).
-define(bucketInfo(BucketType, Bucket),
    #ctx{method='GET', bucket_type=BucketType, bucket=Bucket, resource=undefined}).
-define(deleteBucket(BucketType, Bucket),
    #ctx{method='DELETE', bucket_type=BucketType, bucket=Bucket, resource=undefined}).
-define(bucketResource(BucketType, Bucket, Resource),
    #ctx{method='GET', bucket_type=BucketType, bucket=Bucket, resource=Resource}).

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

service_available(RD, Ctx0) ->
    Ctx1 = Ctx0#ctx{
        resource = wrq:path_info(resource, RD),
        bucket_type = wrq:path_info(bucket_type, RD),
        bucket = wrq:path_info(bucket, RD),
        node = wrq:path_info(node, RD),
        cluster = wrq:path_info(cluster, RD),
        method = wrq:method(RD),
        start = list_to_integer(wrq:get_qs_value("start","0",RD)),
        rows = list_to_integer(wrq:get_qs_value("rows","1000",RD))},

    {true, RD, Ctx1#ctx{node = node_from_context(Ctx1)}}.

allowed_methods(RD, Ctx) ->
    Methods = ['POST', 'GET', 'PUT', 'DELETE'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content},
             {"application/vnd.api+json", provide_jsonapi_content}],
    {Types, RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    Types = [{"application/json", accept_content},
             {"application/vnd.api+json", accept_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?noNode()) ->
    {false, RD, Ctx};
resource_exists(RD, Ctx=?putBuckets(_BucketType)) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=?cleanBuckets(BucketType)) ->
    Node = Ctx#ctx.node,
    re_riak:clean_buckets(Node, BucketType),
    {true, RD, Ctx};
resource_exists(RD, Ctx=?listBucketsCache(BucketType)) ->
    Node = Ctx#ctx.node,
    case re_riak:list_buckets_cache(Node, BucketType, Ctx#ctx.start, Ctx#ctx.rows) of
        {error, not_found} ->
            {false, RD, Ctx};
        Response ->
            {true, RD, Ctx#ctx{id=buckets, response=Response}}
    end;
resource_exists(RD, Ctx=?listBuckets(BucketType)) ->
    Node = Ctx#ctx.node,
    JobsPath = string:substr(wrq:path(RD),1, string:str(wrq:path(RD), "refresh_buckets") - 1) ++ "jobs",
    case re_riak:list_buckets(Node, BucketType) of
        ok ->
            {{halt, 202}, wrq:set_resp_header("Location",JobsPath,RD), Ctx};
        [{error, already_started}] ->
            {{halt, 102}, wrq:set_resp_header("Location",JobsPath,RD), Ctx};
        {error, developer_mode_off} ->
            {{halt, 403}, RD, Ctx}
    end;
resource_exists(RD, Ctx=?deleteBucket(BucketType, Bucket)) ->
    Node = Ctx#ctx.node,
    JobsPath = wrq:path(RD) ++ "/jobs",
    case re_riak:delete_bucket(Node, BucketType, Bucket) of
        ok ->
            {{halt, 202}, wrq:set_resp_header("Location",JobsPath,RD), Ctx};
        [{error, already_started}] ->
            {{halt, 102}, wrq:set_resp_header("Location",JobsPath,RD), Ctx};
        {error, developer_mode_off} ->
            {{halt, 403}, RD, Ctx}
    end;
resource_exists(RD, Ctx=?bucketInfo(_BucketType, Bucket)) ->
    Id = list_to_binary(Bucket),
    Response = [{buckets, [{id,Id}, {props, []}]}],
    {true, RD, Ctx#ctx{id=bucket, response=Response}};
resource_exists(RD, Ctx=?bucketResource(BucketType, Bucket, Resource)) ->
    Node = Ctx#ctx.node,
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] ->
            Response = M:F(Node, BucketType, Bucket),
            {true, RD, Ctx#ctx{id=Id, response=Response}};
        _ ->
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

delete_resource(RD, Ctx) ->
    {true, RD, Ctx}.

accept_content(RD, Ctx=?putBuckets(BucketType)) ->
    Node = Ctx#ctx.node,
    RawValue = wrq:req_body(RD),
    {struct, [{<<"buckets">>, Buckets}]} = mochijson2:decode(RawValue),
    re_riak:put_buckets(Node, BucketType, Buckets),
    {true, RD, Ctx}.

provide_content(RD, Ctx=#ctx{response=undefined}) ->
    JDoc = re_wm_jsonapi:doc(RD, data, null, re_wm_jsonapi:links(RD, "/explore/routes"), [], []),
    {mochijson2:encode(JDoc), RD, Ctx};
provide_content(RD, Ctx=#ctx{id=Id, response=[{_, Objects}]}) ->
    JRes = re_wm_jsonapi:res(RD, [], Objects, [], []),
    JDoc = re_wm_jsonapi:doc(RD, Id, JRes, [], [], []),
    {mochijson2:encode(JDoc), RD, Ctx}.

provide_jsonapi_content(RD, Ctx=#ctx{response=undefined}) ->
    JDoc = re_wm_jsonapi:doc(RD, data, null, re_wm_jsonapi:links(RD, "/explore/routes"), [], []),
    {mochijson2:encode(JDoc), RD, Ctx};
provide_jsonapi_content(RD, Ctx=#ctx{id=Id, response=[{Type, Objects}]}) ->
    JRes = re_wm_jsonapi:res(RD, Type, Objects, [], []),
    JDoc = re_wm_jsonapi:doc(RD, Id, JRes, [], [], []),
    {mochijson2:encode(JDoc), RD, Ctx}.

%% ====================================================================
%% Private
%% ====================================================================

node_from_context(Ctx) ->
    case Ctx of
        #ctx{cluster=undefined, node=N} ->
            Node = list_to_atom(N),
            re_config:set_adhoc_cluster(Node),
            Node;
        #ctx{cluster=C} -> re_riak:first_node(list_to_atom(C))
    end.
