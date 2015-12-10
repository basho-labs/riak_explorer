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

-module(re_wm_riak_proxy).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2]).

-record(ctx, {cluster, node}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [].

routes() ->
    CProxy = re_config:base_route(?RE_RIAK_PROXY_ROUTE) ++ ["clusters", cluster, '*'],
    Proxy = re_config:base_route(?RE_RIAK_PROXY_ROUTE) ++ ["nodes", node, '*'],
    [CProxy, Proxy].

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx0) ->
    Ctx1 = Ctx0#ctx{
        node = wrq:path_info(node, RD),
        cluster = wrq:path_info(cluster, RD)},

    Node = node_from_context(Ctx1),
    Ctx2 = Ctx1#ctx{node=Node},

    case re_riak:node_is_alive(Node) of
        true ->
            send_proxy_request(RD, Ctx2);
        _ ->
            RespBody = mochijson2:encode([{error, <<"Node is not running.">>}]),
            {{halt, 404},
             wrq:set_resp_headers([{<<"Content-Type">>, <<"application/json">>}],
                                  wrq:set_resp_body(RespBody, RD)),
             Ctx2}
    end.

%% ====================================================================
%% Private
%% ====================================================================

send_proxy_request(RD, Ctx) ->
    Node = Ctx#ctx.node,
    Cluster = re_riak:cluster_id_for_node(Node),

    [{http_listener,Listener}] = re_riak:http_listener(Node),
    RiakPath = "http://" ++ binary_to_list(Listener) ++ "/",

    Path = lists:append(
             [RiakPath,
              wrq:disp_path(RD),
              case wrq:req_qs(RD) of
                  [] -> [];
                  Qs -> [$?|mochiweb_util:urlencode(Qs)]
              end]),

    %% translate webmachine details to ibrowse details
    Headers = clean_request_headers(
                mochiweb_headers:to_list(wrq:req_headers(RD))),
    Method = wm_to_ibrowse_method(wrq:method(RD)),
    ReqBody = case wrq:req_body(RD) of
                  undefined -> [];
                  B -> B
              end,

    case ibrowse:send_req(Path, Headers, Method, ReqBody) of
        {ok, Status, RiakHeaders, RespBody} ->
            RespHeaders = fix_location(RiakHeaders, Cluster),
            {{halt, list_to_integer(Status)},
             wrq:set_resp_headers(RespHeaders,
                                  wrq:set_resp_body(RespBody, RD)),
             Ctx};
        _ -> {false, RD, Ctx}
    end.

node_from_context(Ctx) ->
    case Ctx of
        #ctx{cluster=undefined, node=N} ->
            Node = list_to_atom(N),
            re_config:set_adhoc_cluster(Node),
            Node;
        #ctx{cluster=C} -> re_riak:first_node(list_to_atom(C))
    end.

clean_request_headers(Headers) ->
    [{K,V} || {K,V} <- Headers,
              K /= 'Host',
              K /= 'Content-Length',
              K /= 'X-Requested-With',
              K /= 'Referer'].

wm_to_ibrowse_method(Method) when is_list(Method) ->
    list_to_atom(string:to_lower(Method));
wm_to_ibrowse_method(Method) when is_atom(Method) ->
    wm_to_ibrowse_method(atom_to_list(Method)).

fix_location([], _) -> [];
fix_location([{"Location", RiakDataPath}|Rest], Cluster) ->
    [{"Location", re_config:url()++re_config:base_route()++"/"++?RE_RIAK_PROXY_ROUTE++"/clusters/"++atom_to_list(Cluster)++RiakDataPath}|Rest];
fix_location([H|T], Cluster) ->
    [H|fix_location(T, Cluster)].
