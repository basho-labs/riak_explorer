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

-module(re_wm_key).
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

-record(ctx, {method, start, rows, cluster, node, bucket_type, bucket, key, resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(putKeys(BucketType, Bucket),
    #ctx{method='PUT', bucket_type=BucketType, bucket=Bucket, key=undefined}).
-define(cleanKeys(BucketType, Bucket),
    #ctx{method='DELETE', bucket_type=BucketType, bucket=Bucket, key=undefined}).
-define(listKeys(BucketType, Bucket),
    #ctx{method='GET', bucket_type=BucketType, bucket=Bucket, key=undefined}).
-define(keyInfo(BucketType, Bucket, Key),
    #ctx{method='GET', bucket_type=BucketType, bucket=Bucket, key=Key, resource=undefined}).
-define(keyResource(BucketType, Bucket, Key, Resource),
    #ctx{method='GET', bucket_type=BucketType, bucket=Bucket, key=Key, resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() -> 
    [].

routes() ->
    [_, _, CBucket, _, _, Bucket] = re_wm_bucket:routes(),

    CKeys = CBucket ++ ["keys"],
    CKey = CKeys ++ [key],
    CKeyResource = CKey ++ [resource],

    Keys = Bucket ++ ["keys"],
    Key = Keys ++ [key],
    KeyResource = Key ++ [resource],
    [CKeys, CKeyResource, CKey, Keys, KeyResource, Key].

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
        key = wrq:path_info(key, RD),
        node = wrq:path_info(node, RD),
        cluster = wrq:path_info(cluster, RD),
        method = wrq:method(RD),
        start = list_to_integer(wrq:get_qs_value("start","0",RD)),
        rows = list_to_integer(wrq:get_qs_value("rows","1000",RD))},
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET', 'PUT', 'DELETE'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content},
             {"application/vnd.api+json", provide_jsonapi_content}],
    {Types, RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    Types = [{"application/json", accept_content},
             {"application/vnd.api+json", accept_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?putKeys(_BucketType, _Bucket)) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=?cleanKeys(BucketType, Bucket)) ->
    Node = node_from_context(Ctx),
    Succeeded = re_riak:clean_keys(Node, BucketType, Bucket),
    {Succeeded, RD, Ctx};
resource_exists(RD, Ctx=?listKeys(BucketType, Bucket)) ->
    Node = node_from_context(Ctx),
    case re_riak:list_keys(Node, BucketType, Bucket, Ctx#ctx.start, Ctx#ctx.rows) of
        true -> {{halt, 202}, RD, Ctx};
        false -> {{halt, 202}, RD, Ctx};
        Response -> {true, RD, Ctx#ctx{id=keys, response=Response}}
    end;
resource_exists(RD, Ctx=?keyInfo(_BucketType, _Bucket, Key)) ->
    Id = list_to_binary(Key),
    Response = [{keys, [{id,Id}, {props, []}]}],
    {true, RD, Ctx#ctx{id=key, response=Response}};
resource_exists(RD, Ctx=?keyResource(BucketType, Bucket, Key, Resource)) ->
    Node = node_from_context(Ctx),
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] -> 
            Response = M:F(list_to_atom(Node), BucketType, Bucket, Key),
            {true, RD, Ctx#ctx{id=Id, response=Response}};
        _ -> 
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

delete_resource(RD, Ctx) -> 
    {true, RD, Ctx}.

accept_content(RD, Ctx=?putKeys(BucketType, Bucket)) ->
    Node = node_from_context(Ctx),
    RawValue = wrq:req_body(RD),
    {struct, [{<<"keys">>, Keys}]} = mochijson2:decode(RawValue),
    re_riak:put_keys(Node, BucketType, Bucket, Keys),
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