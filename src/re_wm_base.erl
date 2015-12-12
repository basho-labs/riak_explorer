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

-module(re_wm_base).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_japi_content/2,
         provide_json_content/2]).

-record(ctx, {resource, id, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

-define(exploreInfo(),
    #ctx{resource=undefined}).
-define(exploreResource(Resource),
    #ctx{resource=Resource}).

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [{ping, [riak_explorer, ping]},
     {routes, [riak_explorer, routes]},
     {props, [riak_explorer, props]},
     {jobs, [riak_explorer, jobs]}].

routes() ->
    re_config:build_routes(?RE_BASE_ROUTE, [
        [resource],
        [] %% Self
    ]).

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx) ->
    {true, RD, Ctx#ctx{resource = wrq:path_info(resource, RD)}}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_json_content},
             {"application/vnd.api+json", provide_japi_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx=?exploreInfo()) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=?exploreResource(Resource)) ->
    Id = list_to_atom(Resource),
    case proplists:get_value(Id, resources()) of
        [M,F] ->
            set_response(RD, Ctx, Id, M:F());
        _ ->
            {false, RD, Ctx}
    end;
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

provide_json_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(json, RD, Id, Response), RD, Ctx}.

provide_japi_content(RD, Ctx=#ctx{id=Id, response=Response}) ->
    {re_wm_util:provide_content(jsonapi, RD, Id, Response), RD, Ctx}.

%% ====================================================================
%% Private
%% ====================================================================

set_response(RD, Ctx, Id, Response) ->
    re_wm_util:resource_exists(RD, Ctx#ctx{id=Id, response=Response}, Response).
