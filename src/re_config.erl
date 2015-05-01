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

-module(re_config).
-export([
    dispatch/0, 
    web_config/0,
    target_node/0,
    web_root/0]).
-include("riak_explorer.hrl").

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
        {[?RE_BASE_ROUTE], re_wm_explore, []},
        {[?RE_BASE_ROUTE, resource], re_wm_explore, []},
        {['*'], re_wm_static, [{root, filename:join([web_root()])}]}
    ]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].

target_node() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, TargetNode} = application:get_env(App, target_node),
    TargetNode.

web_root() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, WebRoot} = application:get_env(App, web_root),
    WebRoot.