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

-module(re_wm_node).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_jsonapi_content/2,
         provide_content/2]).

-record(ctx, {cluster, node, resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(listNodes(Cluster),
    #ctx{cluster=Cluster, node=undefined}).
-define(nodeInfo(Cluster, Node),
    #ctx{cluster=Cluster, node=Node, resource=undefined}).
-define(nodeResource(Cluster, Node, Resource),
    #ctx{cluster=Cluster, node=Node, resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [{config, [re_riak, node_config]}].

routes() ->
    Base = lists:last(re_wm_base:routes()),
    BaseNodes = Base ++ ["nodes"],
    BaseNode = BaseNodes ++ [node],
    NodeResource = BaseNode ++ [resource],

    Cluster = lists:last(re_wm_cluster:routes()),
    Nodes = Cluster ++ ["nodes"],
    Node = Nodes ++ [node],
    ClusterNodeResource = Node ++ [resource],
    [Nodes, ClusterNodeResource, Node, NodeResource, BaseNode].

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx0) ->
    Ctx1 = Ctx0#ctx{
        resource = wrq:path_info(resource, RD),
        cluster = maybe_atomize(wrq:path_info(cluster, RD)),
        node = maybe_atomize(wrq:path_info(node, RD))},
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content},
             {"application/vnd.api+json", provide_jsonapi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?listNodes(Cluster)) ->
    Response = re_riak:nodes(Cluster),
    case Response of
        [{error, not_found}] ->
            {false, RD, Ctx};
        _ ->
            {true, RD, Ctx#ctx{id=nodes, response=Response}}
    end;
resource_exists(RD, Ctx=?nodeInfo(Cluster0, Node)) ->
    Cluster = case Cluster0 of
        undefined ->
            re_config:set_adhoc_cluster(Node),
            adhoc;
        _ -> Cluster0
    end,
    case re_riak:node_exists(Cluster, Node) of
        true ->
            Id = case Node of
                     S when is_list(S) -> list_to_binary(S);
                     _ -> Node
                 end,
            Response = [{nodes, [{id,Id}, {props, re_riak:node_props(Node)}]}],
            {true, RD, Ctx#ctx{id=node, response=Response}};
        false ->
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx=?nodeResource(Cluster, Node, Resource)) ->
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] ->
            Response = M:F(Cluster, Node),
            case Response of
                [{error, not_found, Message}] ->
                    {{halt, 404},
                     wrq:set_resp_headers([], wrq:set_resp_body(mochijson2:encode(Message), RD)),
                     Ctx};
                _ ->
                    {true, RD, Ctx#ctx{id=Id, response=Response}}
            end;
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

maybe_atomize(Data) when is_list(Data) -> list_to_atom(Data);
maybe_atomize(Data) when is_atom(Data) -> Data.

render_json(Data, RD, Ctx) ->
    Body = mochijson2:encode(Data),
    {Body, RD, Ctx}.
