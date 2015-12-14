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

-module(re_wm_util).
-export([
  maybe_atomize/1,
  node_from_context/2,
  provide_content/4,
  resource_exists/3,
  set_jobs_response/4,
  halt/3,halt/4,halt/5,
  halt_json/4]).

-include_lib("webmachine/include/webmachine.hrl").

%%%===================================================================
%%% API
%%%===================================================================

maybe_atomize(Data) when is_list(Data) -> list_to_atom(Data);
maybe_atomize(Data) when is_atom(Data) -> Data.

node_from_context(Cluster, undefined) ->
    case re_config:riak_node(maybe_atomize(Cluster)) of
        N when is_atom(N) -> N;
        Error -> Error
    end;
node_from_context(_, Node0) ->
    Node = maybe_atomize(Node0),
    case re_riak:node_is_alive(Node) of
        true -> Node;
        _ -> [{error, not_found, [{error, <<"Node is not running.">>}]}]
    end.

provide_content(text, _, _, undefined) ->
    "";
provide_content(_, RD, _, undefined) ->
    render_null_json(RD);
provide_content(text, RD, Id, [{_, Objects}]=Response) ->
    case proplists:get_value(lines, Objects) of
        undefined ->
            provide_content(json, RD, Id, Response);
        Lines ->
            UnbinaryLines = lists:map(fun(A) -> binary_to_list(A) end, Lines),
            LineSep = io_lib:nl(),
            [string:join(UnbinaryLines, LineSep), LineSep]
    end;
provide_content(json, RD, Id, [{_, Objects}]) ->
    render_json(RD, [], Id, Objects);
provide_content(jsonapi, RD, Id, [{Type, Objects}]) ->
    render_json(RD, Type, Id, Objects).

resource_exists(RD, Ctx, {error, not_found}) ->
    {false, RD, Ctx};
resource_exists(RD, Ctx, [{error, not_found, Message}]) ->
    halt_json(404, Message, RD, Ctx);
resource_exists(RD, Ctx, [{error, not_found}]) ->
    {false, RD, Ctx};
resource_exists(RD, Ctx, _) ->
    {true, RD, Ctx}.

set_jobs_response(RD, Ctx, Location, ok) ->
    halt(202, [{"Location",Location}], RD, Ctx);
set_jobs_response(RD, Ctx, Location, [{error, already_started}]) ->
    halt(102, [{"Location",Location}], RD, Ctx);
set_jobs_response(RD, Ctx, _, {error, developer_mode_off}) ->
    halt(403, RD, Ctx).

halt(Code, RD, Ctx) ->
    {{halt, Code}, RD, Ctx}.
halt(Code, Headers, RD, Ctx) ->
    {{halt, Code}, wrq:set_resp_headers(Headers, RD), Ctx}.
halt(Code, Headers, Data, RD, Ctx) ->
    {{halt, Code}, wrq:set_resp_headers(Headers, wrq:set_resp_body(Data, RD)), Ctx}.

halt_json(Code, Data, RD, Ctx) ->
    {{halt, Code},
     wrq:set_resp_headers([{<<"Content-Type">>, <<"application/json">>}],
     wrq:set_resp_body(mochijson2:encode(Data), RD)),
     Ctx}.
%% ====================================================================
%% Private
%% ====================================================================

render_null_json(RD) ->
    JDoc = re_wm_jsonapi:doc(RD, data, null, re_wm_jsonapi:links(RD, "/explore/routes"), [], []),
    mochijson2:encode(JDoc).
render_json(RD, Type, Id, Objects) ->
    JRes = re_wm_jsonapi:res(RD, Type, Objects, [], []),
    JDoc = re_wm_jsonapi:doc(RD, Id, JRes, [], [], []),
    mochijson2:encode(JDoc).
