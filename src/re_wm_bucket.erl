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
         provide_content/2]).

-record(ctx, {bucket_type, bucket, resource, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(listBuckets(BucketType),
    #ctx{bucket_type=BucketType, bucket=undefined}).
-define(bucketInfo(BucketType, Bucket),
    #ctx{bucket_type=BucketType, bucket=Bucket, resource=undefined}).
-define(bucketResource(BucketType, Bucket, Resource),
    #ctx{bucket_type=BucketType, bucket=Bucket, resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() -> 
    [].

routes() ->
    BucketType = lists:last(re_wm_bucket_type:routes()),
    Buckets = BucketType ++ ["buckets"],
    Bucket = Buckets ++ [bucket],
    BucketResource = Bucket ++ [resource],
    [Buckets, BucketResource, Bucket].

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
        bucket = wrq:path_info(bucket, RD)},
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?listBuckets(_BucketType)) ->
    Response = [{buckets, []}],
    {true, RD, Ctx#ctx{response=Response}};
resource_exists(RD, Ctx=?bucketInfo(_BucketType, Bucket)) ->
    Response = [{bucket, list_to_binary(Bucket)}],
    {true, RD, Ctx#ctx{response=Response}};
resource_exists(RD, Ctx=?bucketResource(BucketType, Bucket, Resource)) ->
    RKey = list_to_atom(Resource),
    case proplists:get_value(RKey, resources()) of
        [M,F] -> 
            Response = M:F(BucketType, Bucket),
            {true, RD, Ctx#ctx{response=Response}};
        _ -> 
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

provide_content(RD, Ctx=#ctx{response=Response}) ->
    render_json(Response, RD, Ctx).

%% ====================================================================
%% Private
%% ====================================================================

render_json(Data, RD, CTX) ->
    Body = mochijson2:encode(Data),
    {Body, RD, CTX}.