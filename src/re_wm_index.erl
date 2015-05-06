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

-module(re_wm_index).
% -export([resources/0, routes/0, dispatch/0]).
% -export([init/1]).
% -export([service_available/2,
%          allowed_methods/2, 
%          content_types_provided/2,
%          resource_exists/2,
%          provide_jsonapi_content/2,
%          provide_content/2]).

% -record(ctx, {index, resource, id, response=undefined}).

% -include_lib("webmachine/include/webmachine.hrl").
% -include("riak_explorer.hrl").

% -define(listIndexes(),
%     #ctx{index=undefined}).
% -define(indexInfo(Index),
%     #ctx{index=Index, resource=undefined}).
% -define(indexResource(Index, Resource),
%     #ctx{index=Index, resource=Resource}).

% %%%===================================================================
% %%% API
% %%%===================================================================

% resources() -> 
%     [].

% routes() ->
%     Search = lists:last(re_wm_search:routes()),
%     Indexes = Search ++ ["indexes"],
%     Index = Indexes ++ [index],
%     IndexResource = Index ++ [resource],
%     [Indexes, IndexResource, Index].

% dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

% %%%===================================================================
% %%% Callbacks
% %%%===================================================================

% init(_) ->
%     {ok, #ctx{}}.

% service_available(RD, Ctx0) ->
%     Ctx1 = Ctx0#ctx{
%         resource = wrq:path_info(resource, RD),
%         index = wrq:path_info(index, RD)},
%     {true, RD, Ctx1}.

% allowed_methods(RD, Ctx) ->
%     Methods = ['GET'],
%     {Methods, RD, Ctx}.

% content_types_provided(RD, Ctx) ->
%     Types = [{"application/json", provide_content},
%             {"application/vnd.api+json", provide_jsonapi_content}],
%     {Types, RD, Ctx}.

% resource_exists(RD, Ctx=?listIndexes()) ->
%     Response = [{indexes, []}],
%     {true, RD, Ctx#ctx{id=list, response=Response}};
% resource_exists(RD, Ctx=?indexInfo(Index)) ->
%     Response = [{index, list_to_binary(Index)}],
%     {true, RD, Ctx#ctx{id=list_to_binary(Index), response=Response}};
% resource_exists(RD, Ctx=?indexResource(Index, Resource)) ->
%     Id = list_to_atom(Resource),
%     case proplists:get_value(Id, resources()) of
%         [M,F] -> 
%             Response = M:F(Index),
%             {true, RD, Ctx#ctx{id=Id, response=Response}};
%         _ -> 
%             {false, RD, Ctx}
%     end;
% resource_exists(RD, Ctx) ->
%     {false, RD, Ctx}.

% provide_jsonapi_content(RD, Ctx=#ctx{id=undefined}) ->
%     JDoc = re_wm_jsonapi:doc(null, re_wm_jsonapi:links(RD)),
%     render_json(JDoc, RD, Ctx);
% provide_jsonapi_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
%     JRes = re_wm_jsonapi:res(type(), Id, Response, re_wm_jsonapi:links(RD)),
%     JDoc = re_wm_jsonapi:doc(JRes),
%     render_json(JDoc, RD, Ctx).

% %% ====================================================================
% %% Private
% %% ====================================================================

% type() -> <<"indexes">>.

% render_json(Data, RD, Ctx) ->
%     Body = mochijson2:encode(Data),
%     {Body, RD, Ctx}.