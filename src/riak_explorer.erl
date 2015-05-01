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
  start/0,
  ping/0,
  home/0,
  list_types/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
    _ = ibrowse:start(),
    _ = [ application:start(Dep) || Dep <- resolve_deps(riak_explorer),
                                           not is_otp_base_app(Dep) ],
    ok.

home() ->
  [{message, <<"riak_explorer api">>}].

ping() ->
  [{message, <<"pong">>}].

list_types() ->
  riak(re_riak_patch, list_types, []).

%%%===================================================================
%%% Private
%%%===================================================================

-spec dep_apps(atom()) -> [atom()].
dep_apps(App) ->
    application:load(App),
    {ok, Apps} = application:get_key(App, applications),
    Apps.

-spec all_deps(atom(), [atom()]) -> [atom()].
all_deps(App, Deps) ->
    [[ all_deps(Dep, [App|Deps]) || Dep <- dep_apps(App),
                                           not lists:member(Dep, Deps)], App].

-spec resolve_deps(atom()) -> [atom()].
resolve_deps(App) ->
    DepList = all_deps(App, []),
    {AppOrder, _} = lists:foldl(fun(A,{List,Set}) ->
                                        case sets:is_element(A, Set) of
                                            true ->
                                                {List, Set};
                                            false ->
                                                {List ++ [A], sets:add_element(A, Set)}
                                        end
                                end,
                                {[], sets:new()},
                                lists:flatten(DepList)),
    AppOrder.

-spec is_otp_base_app(atom()) -> boolean().
is_otp_base_app(kernel) -> true;
is_otp_base_app(stdlib) -> true;
is_otp_base_app(_) -> false.

riak(M,F,A) ->
  rpc:block_call(re_config:target_node(), M, F, A, 5000).


-ifdef(TEST).

home_test() ->
    Expected = [{message, <<"riak_explorer api">>}],
    ?assertEqual(Expected, home()).

ping_test() ->
    Expected = [{message, <<"pong">>}],
    ?assertEqual(Expected, ping()).

-endif.