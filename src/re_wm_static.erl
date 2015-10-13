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

-module(re_wm_static).
-export([resources/0, routes/0, dispatch/0]).
-export([init/1]).
-export([service_available/2,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_content/2,
         last_modified/2,
         generate_etag/2]).

-record(ctx, {web_root, resource, response=undefined, metadata=[]}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [].

routes() ->
    Static = re_config:base_route('*'),
    [Static].

dispatch() -> lists:map(fun(Route) -> {Route, ?MODULE, []} end, routes()).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) ->
    WebRoot = re_config:web_root(),
    {ok, #ctx{web_root=WebRoot}}.

service_available(RD, Ctx0=#ctx{web_root=WebRoot}) ->
    Resource0 = get_resource(WebRoot, wrq:disp_path(RD)),
    {Resource1, Response} = get_response(filelib:is_regular(Resource0), WebRoot, Resource0),
    Ctx1 = Ctx0#ctx{resource=Resource1, response=Response},
    {true, RD, Ctx1}.

allowed_methods(RD, Ctx) ->
    {['HEAD', 'GET'], RD, Ctx}.

content_types_provided(RD, Ctx0=#ctx{resource=Resource}) ->
    CT = webmachine_util:guess_mime(Resource),
    CTHeader = {'content-type', CT},
    Ctx1 = Ctx0#ctx{metadata=[CTHeader|Ctx0#ctx.metadata]},
    {[{CT, provide_content}], RD, Ctx1}.

resource_exists(RD, Ctx) ->
    {true, RD, Ctx}.

provide_content(RD, Ctx=#ctx{response=Response}) ->
    {Response, RD, Ctx}.

last_modified(RD, Ctx0=#ctx{resource=Resource}) ->
    LM = filelib:last_modified(Resource),
    LMHeader = {'last-modified', httpd_util:rfc1123_date(LM)},
    Ctx1 = Ctx0#ctx{metadata=[LMHeader|Ctx0#ctx.metadata]},
    {LM, RD, Ctx1}.

generate_etag(RD, Ctx0=#ctx{response=Response}) ->
    ET = hash_body(Response),
    ETHeader = {etag,ET},
    Ctx1 = Ctx0#ctx{metadata=[ETHeader|Ctx0#ctx.metadata]},
    {ET, RD, Ctx1}.

%% ====================================================================
%% Private
%% ====================================================================

get_resource(WebRoot, Path) ->
    Name = resource_name(Path),
    RelName = rel_resource_name(Name),
    filename:join([WebRoot, RelName]).

resource_name([]) -> ?RE_DEFAULT_INDEX;
resource_name(P) -> P.

rel_resource_name(["/"|T]) -> T;
rel_resource_name(N) -> N.

get_response(false, WebRoot, _) ->
    Resource = get_resource(WebRoot, ?RE_DEFAULT_INDEX),
    get_response(true, WebRoot, Resource);
get_response(true, _, Resource) ->
    {ok, Response} = file:read_file(Resource),
    {Resource, Response}.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:hash(sha,Body))).
