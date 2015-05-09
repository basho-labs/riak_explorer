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

-module(re_wm_bucket_type).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2, 
         content_types_provided/2,
         resource_exists/2,
         provide_jsonapi_content/2,
         provide_content/2]).

-record(ctx, {cluster, node, bucket_type, resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(listBucketTypes(),
    #ctx{bucket_type=undefined}).
-define(bucketTypeInfo(BucketType),
    #ctx{bucket_type=BucketType, resource=undefined}).
-define(bucketTypeResource(BucketType, Resource),
    #ctx{bucket_type=BucketType, resource=Resource}).

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
    CBucketTypeResource = CBucketType ++ [resource],

    Nodes = Base ++ ["nodes"],
    Node = Nodes ++ [node],
    BucketTypes = Node ++ ["bucket_types"],
    BucketType = BucketTypes ++ [bucket_type],
    BucketTypeResource = BucketType ++ [resource],
    [CBucketTypes, CBucketTypeResource, CBucketType, BucketTypes, BucketTypeResource, BucketType].

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
        node = wrq:path_info(node, RD),
        cluster = wrq:path_info(cluster, RD)},
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content},
             {"application/vnd.api+json", provide_jsonapi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?listBucketTypes()) ->
    Node = node_from_context(Ctx),
    Response = re_riak:bucket_types(Node),
    {true, RD, Ctx#ctx{id=bucket_types, response=Response}};
resource_exists(RD, Ctx=?bucketTypeInfo(BucketType)) ->
    Id = list_to_binary(BucketType),
    Response = [{bucket_types, [{id,Id}, {props, []}]}],
    {true, RD, Ctx#ctx{id=bucket_type, response=Response}};
resource_exists(RD, Ctx=?bucketTypeResource(BucketType, Resource)) ->
    Node = node_from_context(Ctx),
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] -> 
            Response = M:F(Node, BucketType),
            {true, RD, Ctx#ctx{id=Id, response=Response}};
        _ -> 
            {false, RD, Ctx}
    end;
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

node_from_context(Ctx) ->
    case Ctx of
        #ctx{cluster=undefined, node=N} -> list_to_atom(N);
        #ctx{cluster=C} -> re_riak:first_node(C)
    end.

render_json(Data, RD, Ctx) ->
    Body = mochijson2:encode(Data),
    {Body, RD, Ctx}.