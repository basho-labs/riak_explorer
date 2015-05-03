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
-export([dispatch/0, 
         web_config/0,
         url/0,
         url/2,
         target_node/0,
         web_root/0]).

-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
        {[?RE_BASE_ROUTE], re_wm_explore, []},
        {[?RE_BASE_ROUTE, resource], re_wm_explore, []},
        {[?RE_RIAK_PROXY_ROUTE, node, '*'], re_wm_riak_proxy, []},
        {['*'], re_wm_static, [{web_root, web_root()}]}
    ]).

host_port() ->
    case application:get_env(riak_explorer, host) of
        {ok, {_, _} = HostPort} -> HostPort;
        undefined -> {"0.0.0.0", 9000}
    end.

web_config() ->
    {Ip, Port} = host_port(),
    WebConfig0 = [
        {ip, Ip},
        {port, Port},
        {nodelay, true},
        {log_dir, "log"},
        {dispatch, dispatch()}
    ],
    WebConfig1 = case application:get_env(riak_explorer, ssl) of
        {ok, SSLOpts} -> 
            WebConfig0 ++ [{ssl, true}, {ssl_opts, SSLOpts}];
        undefined -> 
            WebConfig0
    end,
    WebConfig1.

url() ->
    {Ip, Port} = host_port(),
    url(Ip, Port).

url(Ip, Port) ->
    "http://" ++ Ip ++ ":" ++ integer_to_list(Port) ++ "/".

target_node() ->
    {ok, TargetNode} = application:get_env(riak_explorer, riak_node),
    list_to_atom(TargetNode).

web_root() ->
    "priv/ember_riak_explorer/dist".