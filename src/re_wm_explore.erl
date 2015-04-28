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

-record(ctx, {}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    {ok, #ctx{}}.

allowed_methods(Req, S) ->
    Methods = ['GET'],
    {Methods, Req, S}.

content_types_provided(Req, S) ->
    Types = [{"application/json", to_json}],
    {Types, Req, S}.

service_available(Req, S) ->
    {true, Req, S}.

is_authorized(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

resource_forbidden(RD, Ctx, _Permission, {_Resource, _Subresource}) ->
    {false, RD, Ctx}.

forbidden(RD, Ctx) ->
    {false, RD, Ctx}.

to_json(Req, S) ->
    Result = riak_explorer:ping(),
    Body = mochijson2:encode(Result),
    {Body, Req, S}.

resource_exists(RD, Context) ->
    {true, RD, Context}.

%% ====================================================================
%% Private
%% ====================================================================
