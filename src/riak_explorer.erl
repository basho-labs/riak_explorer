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
-export([ping/0,
         home/0,
         routes/0]).

-include("riak_explorer.hrl").

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

routes() ->
    re_config:formatted_routes().

%%%===================================================================
%%% Private
%%%===================================================================


-ifdef(TEST).

home_test() ->
    Expected = [{message, <<"riak_explorer api">>}],
    ?assertEqual(Expected, home()).

ping_test() ->
    Expected = [{message, <<"pong">>}],
    ?assertEqual(Expected, ping()).

-endif.