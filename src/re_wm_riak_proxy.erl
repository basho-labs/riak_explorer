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
-export([init/1,
         service_available/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(Config) -> {ok, Config}.

service_available(RP, C) ->
    RiakPath = "http://" ++ wrq:path_info(node, RP) ++ "/",

    Path = lists:append(
             [RiakPath,
              wrq:disp_path(RP),
              case wrq:req_qs(RP) of
                  [] -> [];
                  Qs -> [$?|mochiweb_util:urlencode(Qs)]
              end]),

    %% translate webmachine details to ibrowse details
    Headers = clean_request_headers(
                mochiweb_headers:to_list(wrq:req_headers(RP))),
    Method = wm_to_ibrowse_method(wrq:method(RP)),
    ReqBody = case wrq:req_body(RP) of
                  undefined -> [];
                  B -> B
              end,

    case ibrowse:send_req(Path, Headers, Method, ReqBody) of
        {ok, Status, RiakHeaders, RespBody} ->
            io:format("Status: ~p~n", [Status]),
            io:format("RiakHeaders: ~p~n", [RiakHeaders]),
            io:format("RespBody: ~p~n", [RespBody]),
            RespHeaders = fix_location(RiakHeaders, RiakPath),
            {{halt, list_to_integer(Status)},
             wrq:set_resp_headers(RespHeaders,
                                  wrq:set_resp_body(RespBody, RP)),
             C};
        Reason ->
            io:format("Reason: ~p~n", [Reason]),
            io:format("RiakPath: ~p~n", [RiakPath]),
            io:format("Path: ~p~n", [Path]),
            {false, RP, C}
    end.

clean_request_headers(Headers) ->
    [{K,V} || {K,V} <- Headers,
              K /= 'Host', K /= 'Content-Length'].

wm_to_ibrowse_method(Method) when is_list(Method) ->
    list_to_atom(string:to_lower(Method));
wm_to_ibrowse_method(Method) when is_atom(Method) ->
    wm_to_ibrowse_method(atom_to_list(Method)).

fix_location([], _) -> [];
fix_location([{"Location", RiakDataPath}|Rest], RiakPath) ->
    DataPath = lists:nthtail(length(RiakPath), RiakDataPath),
    [{"Location", re_config:url()++DataPath}|Rest];
fix_location([H|T], C) ->
    [H|fix_location(T, C)].