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

-module(re_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-include("riak_explorer.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
    case re_config:development_mode() of
        true -> lager:warning("Development mode is enabled");
        _ -> ok
    end,

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [re_config:web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    JobManager = {re_job_manager,
           {re_job_manager, start_link, []},
           permanent, 5000, worker, [re_job_manager]},
    Processes = [Web, JobManager],
    {ok, { {one_for_one, 10, 10}, Processes} }.
