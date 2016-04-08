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

-module(re_wm_static).

-export([routes/0]).

-export([static_types/1,
         static_file_exists/1,
         static_last_modified/1,
         static_file/1]).

-define(STATIC_ROOT, "priv/ember_riak_explorer/dist").
-define(DEFAULT_INDEX, "index.html").

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include("re_wm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

routes() ->
    [
     #route{base = [],
            path = [['*']],
            exists = {?MODULE, static_file_exists},
            provides = {?MODULE, static_types},
            content = {?MODULE, static_file},
            last_modified = {?MODULE, static_last_modified}}
    ].

%%%===================================================================
%%% Callbacks
%%%===================================================================

%% Static.

static_types(ReqData) ->
    Resource = static_filename(ReqData),
    CT = webmachine_util:guess_mime(Resource),
    {[{CT, provide_static_content}], ReqData}.

static_file_exists(ReqData) ->
    Filename = static_filename(ReqData),
    lager:info("Attempting to fetch static file: ~p", [Filename]),
    Result = filelib:is_regular(Filename),
    {Result, ReqData}.

static_last_modified(ReqData) ->
    LM = filelib:last_modified(static_filename(ReqData)),
    {LM, ReqData}.

static_file(ReqData) ->
    Filename = static_filename(ReqData),
    {ok, Response} = file:read_file(Filename),
    ET = hash_body(Response),
    ReqData1 = wrq:set_resp_header("ETag", webmachine_util:quoted_string(ET), ReqData),
    {Response, ReqData1}.

%% ====================================================================
%% Private
%% ====================================================================

static_filename(ReqData) ->
    Resource = wrq:disp_path(ReqData),
    Resource1 = case Resource of
        "" -> "index.html";
        "/" -> "index.html";
        R -> R
    end,
    filename:join([?STATIC_ROOT, Resource1]).

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:hash(sha,Body))).
