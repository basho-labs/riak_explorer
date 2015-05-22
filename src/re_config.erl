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
-export([data_dir/0,
         resources/0,
         dispatch/0,
         development_mode/0,
         development_mode/1,
         routes/0,
         props/0,
         formatted_routes/0,
         format_route/2,
         web_config/0,
         url/0,
         url/2,
         clusters/0,
         cluster/1,
         riak_node/0, riak_node/1,
         web_root/0]).

-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

data_dir() ->
    {ok, Dir} = application:get_env(riak_explorer, platform_data_dir),
    Dir.

resources() ->
    [
        re_wm_key,
        re_wm_bucket,
        re_wm_bucket_type,
        % re_wm_index,
        % re_wm_schema,
        % re_wm_search,
        re_wm_node,
        re_wm_cluster,
        re_wm_base,
        re_wm_riak_proxy,
        re_wm_static
    ].

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() -> lists:flatten(dispatch(resources(), [])).

development_mode() ->
    proplists:get_value(development_mode, cluster(default)).

development_mode(Cluster) ->
    proplists:get_value(development_mode, cluster(Cluster)).

props() ->
    props_to_bin(application:get_all_env(riak_explorer), []).

dispatch([], Accum) ->
    lists:reverse(Accum);
dispatch([M | Rest], Accum) ->
    dispatch(Rest, [M:dispatch() | Accum]).

routes() -> routes(resources(), []).

formatted_routes() ->
    formatted_routes(resources(), []).

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

clusters() ->
    {ok, Clusters} = application:get_env(riak_explorer, clusters),
    Clusters.

cluster(Cluster) ->
    proplists:get_value(Cluster, clusters()).

riak_node() ->
    riak_node(default).

riak_node(Cluster) ->
    list_to_atom(proplists:get_value(riak_node, cluster(Cluster))).

web_root() ->
    "priv/ember_riak_explorer/dist".

%%%===================================================================
%%% Private
%%%===================================================================

md5_hex(S) ->
    list_to_binary(lists:flatten(
       [io_lib:format("~.16b",[N]) || N <- binary_to_list(erlang:md5(S))])).

routes([], Accum) ->
    lists:reverse(Accum);
routes([M | Rest], Accum) ->
    routes(Rest, [M:routes() | Accum]).

formatted_routes([], Accum) ->
    Accum;
formatted_routes([M | Rest], Accum) ->
    ModuleRoutes = format_routes({M, M:routes()}, []),
    formatted_routes(Rest, Accum ++ ModuleRoutes).

format_routes({_,[]}, Accum) -> 
    Accum;
format_routes({M, [Route | Rest]}, Accum) ->
    Path = format_route(Route, []),
    Result0 = [{id, md5_hex(Path)},{links, [{self, Path}]}],
    Result1 = case string:str(binary_to_list(Path), "$resource") of
        I when I > 0 -> Result0 ++ [{resources, proplists:get_keys(M:resources())}];
        _ -> Result0
    end,

    format_routes({M, Rest}, [Result1 | Accum]).

props_to_bin([], Accum) -> lists:reverse(Accum);
props_to_bin([{Name, {Host, Port}} | Rest], Accum) ->
    props_to_bin(Rest, [{Name, list_to_binary(url(Host, Port))} | Accum]);
props_to_bin([{Name, []} | Rest], Accum) ->
    props_to_bin(Rest, [{Name, []} | Accum]);
props_to_bin([{Name, [{_, _} | _]=Nested} | Rest], Accum) ->
    props_to_bin(Rest, [{Name, props_to_bin(Nested, [])} | Accum]);
props_to_bin([{Name, Value} | Rest], Accum) when is_list(Value) ->
    props_to_bin(Rest, [{Name, list_to_binary(Value)} | Accum]);
props_to_bin([{Name, Value} | Rest], Accum) ->
    props_to_bin(Rest, [{Name, Value} | Accum]).