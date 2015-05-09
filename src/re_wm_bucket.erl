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

-module(re_wm_bucket).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2, 
         content_types_provided/2,
         resource_exists/2,
         delete_resource/2,
         provide_jsonapi_content/2,
         provide_content/2]).

-record(ctx, {method, start, rows, cluster, node, bucket_type, bucket, resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(cleanBuckets(BucketType),
    #ctx{method='DELETE', bucket_type=BucketType, bucket=undefined}).
-define(listBuckets(BucketType),
    #ctx{method='GET', bucket_type=BucketType, bucket=undefined}).
-define(bucketInfo(BucketType, Bucket),
    #ctx{method='GET', bucket_type=BucketType, bucket=Bucket, resource=undefined}).
-define(bucketResource(BucketType, Bucket, Resource),
    #ctx{method='GET', bucket_type=BucketType, bucket=Bucket, resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() -> 
    [].

routes() ->
    Base = lists:last(re_wm_base:routes()),

    Clusters = Base ++ ["clusters"],
    Cluster = Clusters ++ [cluster],
    CBucketTypes = Cluster ++ ["bucket_types"],
    CBucketType = CBucketTypes ++ [bucket_type],
    CBuckets = CBucketType ++ ["buckets"],
    CBucket = CBuckets ++ [bucket],
    CBucketResource = CBucket ++ [resource],

    Nodes = Base ++ ["nodes"],
    Node = Nodes ++ [node],
    BucketTypes = Node ++ ["bucket_types"],
    BucketType = BucketTypes ++ [bucket_type],
    Buckets = BucketType ++ ["buckets"],
    Bucket = Buckets ++ [bucket],
    BucketResource = Bucket ++ [resource],
    [CBuckets, CBucketResource, CBucket, Buckets, BucketResource, Bucket].

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
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET', 'DELETE'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content},
             {"application/vnd.api+json", provide_jsonapi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?cleanBuckets(BucketType)) ->
    Node = node_from_context(Ctx),
    Succeeded = re_riak:clean_buckets(Node, BucketType),
    {Succeeded, RD, Ctx};
resource_exists(RD, Ctx=?listBuckets(BucketType)) ->
    Node = node_from_context(Ctx),
    case re_riak:list_buckets(Node, BucketType, Ctx#ctx.start, Ctx#ctx.rows) of
        true -> {{halt, 202}, RD, Ctx};
        false -> {{halt, 202}, RD, Ctx};
        Response -> {true, RD, Ctx#ctx{id=buckets, response=Response}}
    end;
resource_exists(RD, Ctx=?bucketInfo(_BucketType, Bucket)) ->
    Id = list_to_binary(Bucket),
    Response = [{buckets, [{id,Id}, {props, []}]}],
    {true, RD, Ctx#ctx{id=bucket_type, response=Response}};
resource_exists(RD, Ctx=?bucketResource(BucketType, Bucket, Resource)) ->
    Node = node_from_context(Ctx),
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] -> 
            Response = M:F(list_to_atom(Node), BucketType, Bucket),
            {true, RD, Ctx#ctx{id=Id, response=Response}};
        _ -> 
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

delete_resource(RD, Ctx) -> 
    {true, RD, Ctx}.

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

node_from_context(Ctx) ->
    case Ctx of
        #ctx{cluster=undefined, node=N} -> list_to_atom(N);
        #ctx{cluster=C} -> re_riak:first_node(C)
    end.

render_json(Data, RD, Ctx) ->
    Body = mochijson2:encode(Data),
    {Body, RD, Ctx}.