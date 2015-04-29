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
-include("riak_explorer.hrl").
-export([
  ping/0,
  home/0,
  list_types/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

home() ->
  [{message, <<"riak_explorer api">>}].

ping() ->
  [{message, <<"pong">>}].

list_types() ->
  It = riak_core_bucket_type:iterator(),
  List0 = [[{name, <<"default">>},{props, [{active, true} | format_props(riak_core_bucket_props:defaults(), [])]}]],
  List1 = fetch_types(It, List0),
  [{data, List1}].

%%%===================================================================
%%% Private
%%%===================================================================

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

-ifdef(TEST).

home_test() ->
    Expected = [{message, <<"riak_explorer api">>}],
    ?assertEqual(Expected, home()).

ping_test() ->
    Expected = [{message, <<"pong">>}],
    ?assertEqual(Expected, ping()).

-endif.