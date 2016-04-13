%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.
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
%%
%% @doc Application supervisor.

-module(riak_control_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RiakControlSession={riak_control_session,
                         {riak_control_session, start_link, []},
                         permanent,
                         5000,
                         worker,
                         [riak_control_session]},

    %% determine if riak_control is enabled or not
    RiakControlProcesses = case app_helper:get_env(riak_control, enabled, false) of
        true ->
            Resources = [riak_control_wm_gui,
                         riak_control_wm_cluster,
                         riak_control_wm_nodes,
                         riak_control_wm_partitions],
            Routes = lists:append([Resource:routes() || Resource <- Resources]),
            _ = [webmachine_router:add_route(R) || R <- Routes],

            %% start riak control
            % {ok, { {one_for_one, 5, 10}, [RiakControlSession] } };
            [RiakControlSession];
        _ ->
            % {ok, { {one_for_one, 5, 10}, [] } }
            []
    % end.
    end,

    %%TODO: make a better application start entry point in riak_explorer to avoid this

    %% In order to easily inject Riak Explorer into a riak installation,
    %% we're hijacking the old riak_control_sup module's init/1 to start up
    %% Riak Explorer.

    RexRoutes = lists:reverse(re_wm:dispatch()),
    _ = [webmachine_router:add_route(R) || R <- RexRoutes],

    JobManager = {re_job_manager,
           {re_job_manager, start_link, []},
           permanent, 5000, worker, [re_job_manager]},
    Processes = [JobManager] ++ RiakControlProcesses,
    {ok, { {one_for_one, 10, 10}, Processes} }.
    %% End Riak Explorer injection
