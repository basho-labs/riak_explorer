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

-module(re_wm_explore).
-export([
    init/1, 
    allowed_methods/2, 
    content_types_provided/2, 
    service_available/2, 
    is_authorized/2, 
    resource_forbidden/4, 
    forbidden/2, 
    to_json/2, 
    resource_exists/2]).
-include("riak_explorer.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {resource}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

allowed_methods(RD, Ctx) ->
    Methods = ['GET'],
    {Methods, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", to_json}],
    {Types, RD, Ctx}.

service_available(RD, Ctx) ->
    {
        true,
        RD,
        Ctx#ctx{
            resource = wrq:path_info(resource, RD)
        }
    }.

is_authorized(RD, Ctx) ->
    {true, RD, Ctx}.

resource_forbidden(RD, Ctx, _Permission, {_Resource, _Subresource}) ->
    {false, RD, Ctx}.

forbidden(RD, Ctx) ->
    {false, RD, Ctx}.

resource_exists(RD, Ctx=#ctx{resource=undefined}) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=#ctx{resource="ping"}) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx=#ctx{resource="bucket_types"}) ->
    {true, RD, Ctx};
resource_exists(RD, Ctx) ->
    {false, RD, Ctx}.

to_json(RD, Ctx=#ctx{resource=undefined}) ->
    render_json(riak_explorer:home(), RD, Ctx);
to_json(RD, Ctx=#ctx{resource="ping"}) ->
    render_json(riak_explorer:ping(), RD, Ctx);
to_json(RD, Ctx=#ctx{resource="bucket_types"}) ->
    render_json(riak_explorer:list_types(), RD, Ctx).

%% ====================================================================
%% Private
%% ====================================================================

render_json(Data, RD, CTX) ->
    Body = mochijson2:encode(Data),
    {Body, RD, CTX}.