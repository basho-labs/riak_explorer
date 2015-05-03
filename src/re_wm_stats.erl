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

-module(re_wm_stats).
-export([routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2, 
         content_types_provided/2,
         resource_exists/2,
         provide_content/2]).

-record(ctx, {resource, response=undefined}).

-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

routes() ->
    [_, Cluster] = re_wm_cluster:routes(),

    Stats = Cluster ++ ["stats"],
    
    [Stats].

%% /explore/clusters/$/stats
dispatch() ->
    [Stats] = routes(),

    [{Stats, ?MODULE, []}].

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

service_available(RD, Ctx0) ->
    Ctx1 = Ctx0#ctx{resource = wrq:path_info(resource, RD)},
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", provide_content}],
    {Types, RD, Ctx}.

resource_exists(RD, Ctx0=#ctx{resource=undefined}) ->
    Ctx1 = Ctx0#ctx{response=riak_explorer:home()},
    {true, RD, Ctx1};
resource_exists(RD, Ctx0=#ctx{resource="ping"}) ->
    Ctx1 = Ctx0#ctx{response=riak_explorer:ping()},
    {true, RD, Ctx1};
resource_exists(RD, Ctx0=#ctx{resource="bucket_types"}) ->
    Ctx1 = Ctx0#ctx{response=riak_explorer:bucket_types()},
    {true, RD, Ctx1};
resource_exists(RD, Ctx0=#ctx{resource="cluster_nodes"}) ->
    Ctx1 = Ctx0#ctx{response=riak_explorer:cluster_nodes()},
    {true, RD, Ctx1};
resource_exists(RD, Ctx0=#ctx{resource="cluster_http_listeners"}) ->
    Ctx1 = Ctx0#ctx{response=riak_explorer:cluster_http_listeners()},
    {true, RD, Ctx1};
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