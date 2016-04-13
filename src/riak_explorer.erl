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

-module(riak_explorer).

-compile({no_auto_import,[nodes/1]}).

-behaviour(application).

-export([start/2, 
         stop/1]).

-export([cluster_config/1,
         cluster_configs/0,
         clusters/0]).

-export([is_riak/0,
         url/0,
         url/2,
         props/0,
         host_port/0,
         data_dir/0]).
        
%%%===================================================================
%%% API
%%%===================================================================

-spec cluster_config(re_cluster:re_cluster()) -> {error, not_found} | re_cluster:re_cluster_props().
cluster_config(undefined) ->
    cluster_config(default);
cluster_config(C) ->
    proplists:get_value(C, cluster_configs(), {error, not_found}).
    
-spec cluster_configs() -> [{re_cluster:re_cluster(), re_cluster:re_cluster_props()}].
cluster_configs() ->
    case application:get_env(riak_explorer, clusters) of
        undefined ->
            [{default, []}];
        {ok, Clusters} ->
            Clusters1 = lists:keysort(1, Clusters),
            Clusters2 = 
                case should_clean_default_cluster() of
                    true ->
                        proplists:delete(default, Clusters1);
                    false ->
                        Clusters1
                end,
            case length(Clusters2) of
                0 -> [{default, []}];
                _ -> Clusters2
            end
    end.

-spec clusters() -> [{re_cluster:re_cluster(), re_cluster:re_cluster_props()}].
clusters() ->
    [re_cluster:from_props(C, Props) || {C, Props} <- cluster_configs()].

-spec is_riak() -> boolean().
is_riak() ->
    case code:ensure_loaded(riak_core) of
        {module,riak_core} -> true;
        _ -> false
    end.

-spec url() -> string().
url() ->
    {Ip, Port} = host_port(),
    url(Ip, Port).

-spec url(string(), integer()) -> string().
url(Ip, Port) ->
    case re_wm:base_route() of
        [] ->
            "http://" ++ Ip ++ ":" ++ integer_to_list(Port) ++ "/";
        [R] ->
            "http://" ++ Ip ++ ":" ++ integer_to_list(Port) ++ "/" ++ R ++ "/"
    end.

-spec props() -> [{atom(), atom() | binary()}].
props() ->
    props_to_bin(application:get_all_env(riak_explorer), []).

-spec host_port() -> {string(), integer()}.
host_port() ->
    case application:get_env(riak_explorer, host) of
        {ok, {_, _} = HostPort} -> HostPort;
        undefined -> {"0.0.0.0", 9000}
    end.

-spec data_dir() -> string().
data_dir() ->
    Def = "./data",
    Dir = application:get_env(riak_explorer, platform_data_dir, Def),
    Dir.

%%%===================================================================
%%% Callbacks
%%%===================================================================

start(_Type, _StartArgs) ->
    re_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Private
%%%===================================================================

-spec should_clean_default_cluster() -> boolean().
should_clean_default_cluster() ->
    {ok, EtcDir} = application:get_env(riak_explorer, platform_etc_dir),
    EtcFile = filename:join([EtcDir, "riak_explorer.conf"]),
    Proc = fun(Entry, Accum) ->
                   case re:run(Entry, "^clusters.default.*$", []) of
                       {match, _} -> false;
                       _ -> Accum
                   end
           end,
    re_file_util:for_each_line_in_file(EtcFile, Proc, read, true).

-spec props_to_bin([{atom(), term()}], [{atom(), atom() | binary()}]) -> [{atom(), atom() | binary()}].
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
