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

-record(ctx, {}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

resources() -> 
    [].

routes() ->
    Proxy = [?RE_RIAK_PROXY_ROUTE, node, '*'],
    [Proxy].

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx) ->
    Node = list_to_atom(wrq:path_info(node, RD)),
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
            RespHeaders = fix_location(RiakHeaders, RiakPath),
            {{halt, list_to_integer(Status)},
             wrq:set_resp_headers(RespHeaders,
                                  wrq:set_resp_body(RespBody, RD)),
             Ctx};
        _ -> {false, RD, Ctx}
    end.

%% ====================================================================
%% Private
%% ====================================================================

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
fix_location([{"Location", RiakDataPath}|Rest], RiakPath) ->
    DataPath = lists:nthtail(length(RiakPath), RiakDataPath),
    [{"Location", re_config:url()++DataPath}|Rest];
fix_location([H|T], RiakPath) ->
    [H|fix_location(T, RiakPath)].