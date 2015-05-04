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
-export([resources/0,
         dispatch/0,
         routes/0,
         formatted_routes/0,
         web_config/0,
         url/0,
         url/2,
         target_node/0,
         web_root/0]).

-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

resources() ->
    [
        re_wm_key,
        re_wm_bucket,
        re_wm_bucket_type,
        re_wm_index,
        re_wm_schema,
        re_wm_search,
        re_wm_stats,
        re_wm_node,
        re_wm_cluster,
        re_wm_base,
        re_wm_riak_proxy,
        re_wm_static
    ].

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() -> lists:flatten(dispatch(resources(), [])).

dispatch([], Accum) ->
    lists:reverse(Accum);
dispatch([M | Rest], Accum) ->
    dispatch(Rest, [M:dispatch() | Accum]).

routes() -> routes(resources(), []).

routes([], Accum) ->
    lists:reverse(Accum);
routes([M | Rest], Accum) ->
    routes(Rest, [M:routes() | Accum]).

formatted_routes() ->
    [{<<"routes">>, formatted_routes(resources(), [])}].

formatted_routes([], Accum) ->
    lists:reverse(Accum);
formatted_routes([M | Rest], Accum) ->
    ModuleRoutes = [{handler, list_to_binary(atom_to_list(M))},
                    {routes, format_routes(M:routes(), [])},
                    {resources, M:resources()}],
    formatted_routes(Rest, [ModuleRoutes | Accum]).



format_routes([], Accum) -> 
    lists:reverse(Accum);
format_routes([Route | Rest], Accum) ->
    format_routes(Rest, [format_route(Route, []) | Accum]).

format_route([], Accum) -> 
    list_to_binary(lists:flatten(Accum));
format_route([Piece | Rest], Accum) when is_list(Piece) ->
    format_route(Rest, Accum ++ "/" ++ Piece);
format_route([Piece | Rest], Accum) when is_atom(Piece) ->
    format_route(Rest, Accum ++ "/$" ++ atom_to_list(Piece)).

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