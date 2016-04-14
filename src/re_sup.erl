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
-export([start_link/1]).
-export([add_wm_routes/0, supervisor_specs/1]).
-export([init/1]).


%%%===================================================================
%%% API
%%%===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

add_wm_routes() ->
    [webmachine_router:add_route(R) || R <- lists:reverse(re_wm:dispatch())].

supervisor_specs(Args) ->
    Mode = proplists:get_value(mode, Args, "standalone"),
    JobManager = {re_job_manager,
                  {re_job_manager, start_link, []},
                  permanent, 5000, worker, [re_job_manager]},
    case Mode of
        "riak" ->
            [JobManager];
        _ ->
            Web = {webmachine_mochiweb,
                   {webmachine_mochiweb, start, [web_config()]},
                   permanent, 5000, worker, [mochiweb_socket_server]},
            [Web, JobManager]
    end.
            
%%%===================================================================
%%% Callbacks
%%%===================================================================

init(Args) ->
    Specs = supervisor_specs(Args),
    {ok, { {one_for_one, 10, 10}, Specs} }.

%% ====================================================================
%% Private
%% ====================================================================

web_config() ->
    {Ip, Port} = riak_explorer:host_port(),
    WebConfig0 = [
        {ip, Ip},
        {port, Port},
        {nodelay, true},
        {log_dir, "log"},
        {dispatch, re_wm:dispatch()}
    ],
    WebConfig1 = case application:get_env(riak_explorer, ssl) of
        {ok, SSLOpts} ->
            WebConfig0 ++ [{ssl, true}, {ssl_opts, SSLOpts}];
        undefined ->
            WebConfig0
    end,
    WebConfig1.
