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

-module(re_wm_cluster).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_jsonapi_content/2,
         provide_content/2]).

-record(ctx, {cluster, resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(listClusters(),
    #ctx{cluster=undefined}).
-define(clusterInfo(Cluster),
    #ctx{cluster=Cluster, resource=undefined}).
-define(clusterResource(Cluster, Resource),
    #ctx{cluster=Cluster, resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [].

routes() ->
    Base = lists:last(re_wm_base:routes()),
    Clusters = Base ++ ["clusters"],
    Cluster = Clusters ++ [cluster],
    ClusterResource = Cluster ++ [resource],
    [Clusters, ClusterResource, Cluster].

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx0) ->
    Ctx1 = Ctx0#ctx{
        resource = wrq:path_info(resource, RD),
        cluster = re_wm_util:maybe_atomize(wrq:path_info(cluster, RD))},
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content},
             {"application/vnd.api+json", provide_jsonapi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?listClusters()) ->
    Response = re_riak:clusters(),
    {true, RD, Ctx#ctx{id=clusters, response=Response}};
resource_exists(RD, Ctx=?clusterInfo(Cluster)) ->
    case re_riak:cluster(Cluster) of
        {error, not_found} ->
            {false, RD, Ctx};
        Response ->
            {true, RD, Ctx#ctx{id=cluster, response=Response}}
    end;
resource_exists(RD, Ctx=?clusterResource(Cluster, Resource)) ->
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] ->
            Response = M:F(Cluster),
            {true, RD, Ctx#ctx{id=Id, response=Response}};
        _ ->
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

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
