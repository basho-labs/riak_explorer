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
-export([build_routes/2, build_routes/3,
         base_route/0, base_route/1,
         data_dir/0,
         resources/0,
         dispatch/0,
         is_standalone/0,
         development_mode/1,
         routes/0,
         props/0,
         formatted_routes/0,
         format_route/2,
         web_config/0,
         url/0, url/2,
         clusters/0, cluster/1,
         cluster_exists/1,
         riak_node/1,
         web_root/0]).

%%%===================================================================
%%% API
%%%===================================================================

build_routes(Base, Routes) ->
    build_prefixed_routes(base_route(Base), [], Routes, []).

build_routes(Base, Prefixes, Routes) ->
    build_routes(Base, Prefixes, Routes, []).

base_route() ->
    case is_standalone() of
        true -> "";
        false -> "admin"
    end.

base_route(SubRoute) ->
    case is_standalone() of
        true -> [SubRoute];
        false -> [base_route(), SubRoute]
    end.

data_dir() ->
    Def = "./data",
    Dir = application:get_env(riak_explorer, platform_data_dir, Def),
    Dir.

resources() ->
    [
        re_wm_key,
        re_wm_bucket,
        re_wm_bucket_type,
        re_wm_table,
        re_wm_node,
        re_wm_riak_config,
        re_wm_riak_log,
        re_wm_cluster,
        re_wm_base,
        re_wm_control,
        re_wm_riak_proxy,
        re_wm_static
    ].

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() -> lists:flatten(dispatch(resources(), [])).

development_mode(Cluster) ->
    case proplists:get_value(development_mode, cluster(Cluster)) of
        true -> true;
        false -> false;
        _ -> true
    end.

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
    case default_cluster_exists() of
        true ->
            Def = [{default,default_cluster_config()}],
            application:get_env(riak_explorer, clusters, Def);
        _ ->
            proplists:delete(default, application:get_env(riak_explorer, clusters, []))
    end.

cluster_exists(Cluster) ->
    case cluster(Cluster) of
        [{error, not_found}] ->
            false;
        _ ->
            true
    end.

%% A little hacky, but in order to get rid of cuttlefish's auto-default cluster, we need to do something like this
default_cluster_exists() ->
    case is_standalone() of
        true ->
            {ok, EtcDir} = application:get_env(riak_explorer, platform_etc_dir),
            EtcFile = filename:join([EtcDir, "riak_explorer.conf"]),
            Proc = fun(Entry, Accum) ->
                case re:run(Entry, "^clusters.default.*$", []) of
                    {match, _} -> true;
                    _ -> Accum
                end
            end,
            re_file_util:for_each_line_in_file(EtcFile, Proc, read, false);
        false -> true
    end.

default_cluster_config() ->
    DefNode = case is_standalone() of
        true -> "riak@127.0.0.1";
        false -> atom_to_list(node())
    end,
    [{riak_node,DefNode}, {development_mode,true}].

cluster(undefined) ->
    default_cluster_config();
cluster(Cluster) ->
    proplists:get_value(Cluster, clusters(), [{error, not_found}]).

riak_node(Cluster) ->
    case cluster(Cluster) of
        [{error, not_found}] ->
            [{error, not_found, [{error, <<"Cluster doesn't exist.">>}]}];
        C ->
            case proplists:get_value(riak_node, C) of
                undefined ->
                    [{error, not_found, [{error, <<"Attribute 'riak_node' missing for cluster.">>}]}];
                N ->
                    list_to_atom(N)
            end
    end.

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

is_standalone() ->
    case code:ensure_loaded(riak_core) of
        {module,riak_core} -> false;
        _ -> true
    end.

build_routes(_, [], _, Acc) ->
    Acc;
build_routes(Base, [P|Prefixes], Routes, Acc) ->
    PRoutes = build_prefixed_routes(base_route(Base), P, Routes, []),
    build_routes(Base, Prefixes, Routes, Acc ++ PRoutes).

build_prefixed_routes(_, _, [], Acc) ->
    lists:reverse(Acc);
build_prefixed_routes(Base, Prefix, [R|Routes], Acc) ->
    R0 = Base ++ Prefix ++ R,
    build_prefixed_routes(Base, Prefix, Routes, [R0|Acc]).
