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

-module(re_riak_patch).
-export([
  version/0,
  bucket_types/0,
  riak_version/0,
  tail_log/2,
  get_log_files/0,
  get_config/1,
  get_config_files/0,
  effective_config/0]).

-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Increment this when code changes
version() -> 8.

bucket_types() ->
  It = riak_core_bucket_type:iterator(),
  List0 = [[{name, <<"default">>},{props, [{active, true} | format_props(riak_core_bucket_props:defaults(), [])]}]],
  fetch_types(It, List0).

riak_version() ->
    LibDir = app_helper:get_env(riak_core, platform_lib_dir),
    EnvFile = filename:join([LibDir, "env.sh"]),
    {ok, Device} = file:open(EnvFile, read),
    Proc = fun(Entry, Accum) ->
        case re:run(Entry, "^APP_VERSION=(.*)", [{capture, all_but_first, list}]) of
            {match, [Version]} -> list_to_binary(Version);
            _ -> Accum
        end
    end,
    for_each_line(Device, Proc, undefined).

tail_log(Name, NumLines) ->
    LogDir = app_helper:get_env(riak_core, platform_log_dir),
    LogFile = filename:join([LogDir, Name]),
    case file:open(LogFile, read) of
        {ok, Device} ->
            Proc = fun(Entry, Accum) -> [Entry|Accum] end,
            Lines0 = for_each_line(Device, Proc, []),
            Count = length(Lines0),
            Lines1 = lists:nthtail(max(Count-NumLines, 0), Lines0),
            {Count, lists:map(fun(Line) -> list_to_binary(re:replace(Line, "(^\\s+)|(\\s+$)", "", [global,{return,list}])) end, Lines1)};
        _ ->
            [{error, not_found}]
    end.

get_config_files() ->
    EtcDir = app_helper:get_env(riak_core, platform_etc_dir),

    case file:list_dir(EtcDir) of
        {ok,Files} -> Files;
        _ -> []
    end.

get_log_files() ->
    LogDir = app_helper:get_env(riak_core, platform_log_dir),

    case file:list_dir(LogDir) of
        {ok,Files} -> Files;
        _ -> []
    end.

get_config(Name) ->
    EtcDir = app_helper:get_env(riak_core, platform_etc_dir),
    EtcFile = filename:join([EtcDir, Name]),
    case file:open(EtcFile, read) of
        {ok, Device} ->
            Proc = fun(Entry, Accum) ->
                B = re:replace(Entry, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                [list_to_binary(B)|Accum]
            end,
            Lines = for_each_line(Device, Proc, ""),
            lists:reverse(Lines);
        _ ->
            [{error, not_found}]
    end.

effective_config() ->
    EtcDir = app_helper:get_env(riak_core, platform_etc_dir),
    Conf = load_riak_conf(EtcDir),
    Schema = load_schema(),
    {AppConfigExists, _} = check_existence(EtcDir, "app.config"),
    {VMArgsExists, _} = check_existence(EtcDir, "vm.args"),
    case {AppConfigExists, VMArgsExists} of
        {false, false} ->
            AdvConfig = load_advanced_config(EtcDir),
            EffectiveString = string:join(cuttlefish_effective:build(Conf, Schema, AdvConfig), "\n"),
            EffectiveProplist = generate_effective_proplist(EffectiveString),
            case AdvConfig of
                [] -> [{config, EffectiveProplist}];
                _ -> [{config, EffectiveProplist}, {advanced_config, format_value(AdvConfig)}]
            end;
        _ ->
            {error,legacy_config}
    end.

%%%===================================================================
%%% Private
%%%===================================================================
check_existence(EtcDir, Filename) ->
    FullName = filename:join(EtcDir, Filename), %% Barfolomew
    Exists = filelib:is_file(FullName),
    lager:info("Checking ~s exists... ~p", [FullName, Exists]),
    {Exists, FullName}.

load_riak_conf(EtcDir) ->
    case check_existence(EtcDir, "riak.conf") of
        {true, ConfFile} ->
            cuttlefish_conf:files([ConfFile]);
        _ ->
            {error, riak_conf_not_found}
    end.

load_schema() ->
    SchemaDir = app_helper:get_env(riak_core, platform_lib_dir),
    SchemaFiles = [filename:join(SchemaDir, Filename) || Filename <- filelib:wildcard("*.schema", SchemaDir)],
    SortedSchemaFiles = lists:sort(fun(A,B) -> A < B end, SchemaFiles),
    cuttlefish_schema:files(SortedSchemaFiles).

load_advanced_config(EtcDir) ->
    case check_existence(EtcDir, "advanced.config") of
        {true, AdvancedConfigFile} ->
            case file:consult(AdvancedConfigFile) of
                {ok, [AdvancedConfig]} ->
                    AdvancedConfig;
                {error, Error} ->
                    [],
                    lager:error("Error parsing advanced.config: ~s", [file:format_error(Error)])
            end;
        _ ->
            []
    end.

generate_effective_proplist(EffectiveString) ->
    EffectivePropList = conf_parse:parse(EffectiveString),
    lists:foldl(
      fun({Variable, Value}, Acc) ->
              Var = list_to_binary(string:join(Variable, ".")),
              Val = case Value of
                        S when is_list(S) -> list_to_binary(S);
                        _ -> Value
                    end,
              [{Var, Val} | Acc]
      end,
      [],
      EffectivePropList
     ).

for_each_line(Device, Proc, Accum) ->
  case io:get_line(Device, "") of
      eof  ->
          file:close(Device), Accum;
      Line ->
          NewAccum = Proc(Line, Accum),
          for_each_line(Device, Proc, NewAccum)
  end.

fetch_types(It, Acc) ->
    case riak_core_bucket_type:itr_done(It) of
        true ->
            riak_core_bucket_type:itr_close(It),
            lists:reverse(Acc);
        _ ->
            {Type, Props} = riak_core_bucket_type:itr_value(It),
            T = [{name, Type},{props, format_props(Props, [])}],
            fetch_types(riak_core_bucket_type:itr_next(It), [T | Acc])
    end.

format_props([], Acc) ->
  lists:reverse(Acc);
format_props([{Name, Val} | Rest], Acc) ->
  format_props(Rest, [{Name, format_value(Val)} | Acc]).

format_list([], Acc) ->
  lists:reverse(Acc);
format_list([Val | Rest], Acc) ->
  format_list(Rest, [format_value(Val) | Acc]).

format_value(Val) when is_list(Val) ->
  format_list(Val, []);
format_value(Val) when is_number(Val) ->
  Val;
format_value(Val) when is_binary(Val) ->
  Val;
format_value(true) ->
  true;
format_value(false) ->
  false;
format_value(Val) ->
  list_to_binary(lists:flatten(io_lib:format("~p", [Val]))).
