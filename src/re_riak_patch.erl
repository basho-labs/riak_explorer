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
  get_config/1]).

-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Increment this when code changes
version() -> 3.

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
    for_each_line(Device, Proc, notfound).

tail_log(Name, NumLines) ->
    LogDir = app_helper:get_env(riak_core, platform_log_dir),
    LogFile = filename:join([LogDir, Name]),
    TotalLines = count_lines(LogFile),
    case file:open(LogFile, read) of
        {error,enoent} ->
            {0, []};
        {ok, Device} ->
            Proc = fun(Entry, {Current,Num,Total,Accum}) ->
            case {Current, Num, Total} of
                {C, N, T} when C > T -> {C+1, N, T, Accum};
                {C, N, T} when C =< (T - N) -> {C+1, N, T, Accum};
                {C, N, T} when C > (T - N) ->
                    B = re:replace(Entry, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                    {C+1, N, T, [list_to_binary(B)|Accum]}
            end
        end,
        {_,_,_,Lines} = for_each_line(Device, Proc, {0,NumLines,TotalLines,[]}),
        {TotalLines, Lines}
    end.

get_config(Name) ->
    EtcDir = app_helper:get_env(riak_core, platform_etc_dir),
    EtcFile = filename:join([EtcDir, Name]),
    case file:open(EtcFile, read) of
        {error,enoent} ->
            [<<"">>];
        {ok, Device} ->
            Proc = fun(Entry, Accum) ->
                B = re:replace(Entry, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                [list_to_binary(B)|Accum]
            end,
            Lines = for_each_line(Device, Proc, ""),
            lists:reverse(Lines)
    end.


%%%===================================================================
%%% Private
%%%===================================================================

count_lines(Name) ->
    case file:open(Name, read) of
        {error,enoent} ->
            0;
        {ok, Device} ->
            Proc = fun(_, NumLines) ->
                NumLines + 1
            end,
            for_each_line(Device, Proc, 0)
    end.

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
        false ->
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
