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
         routes/0,
         props/0,
         jobs/0,
         jobs_for_resource/2, jobs_for_resource/3]).

-include("riak_explorer.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

ping() ->
    [{ping, [{id,<<"ping">>}, {message, <<"pong">>}]}].

routes() ->
    [{routes, re_config:formatted_routes()}].

props() ->
    [{props, re_config:props()}].

jobs() ->
    [{jobs, re_job_manager:get_jobs()}].

jobs_for_resource(_Node, _BucketType) ->
    Jobs = case re_job_manager:get(buckets) of
        [{error, not_found}] -> [];
        J -> [J]
    end,
    [{jobs, Jobs}].

jobs_for_resource(_Node, _BucketType, _Bucket) ->
    Jobs = case re_job_manager:get(keys) of
        [{error, not_found}] -> [];
        J -> [J]
    end,
    [{jobs, Jobs}].

%%%===================================================================
%%% Private
%%%===================================================================


-ifdef(TEST).

ping_test() ->
    Expected = [{message, <<"pong">>}],
    ?assertEqual(Expected, ping()).

-endif.