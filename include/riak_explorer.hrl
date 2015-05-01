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

%%%===================================================================
%%% Types
%%%===================================================================

-type component() :: explorer.

%%%===================================================================
%%% Macros
%%%===================================================================

-define(RE_BASE_ROUTE, "explore").
-define(RE_RIAK_PROXY_ROUTE, "riak").
-define(RE_APP_NAME, riak_explorer).
-define(RE_SVC_NAME, riak_explorer).
-define(RE_ENABLED, app_helper:get_env(?RE_APP_NAME, enabled, false)).
-define(FMT(S, Args), lists:flatten(io_lib:format(S, Args))).
-define(TIME_ELAPSED(StartTime), timer:now_diff(os:timestamp(), StartTime)).

%%%===================================================================
%%% Logging
%%%===================================================================

-define(DEBUG(Fmt, Args), lager:debug(Fmt, Args)).
-define(ERROR(Fmt), lager:error(Fmt)).
-define(ERROR(Fmt, Args), lager:error(Fmt, Args)).
-define(INFO(Fmt, Args), lager:info(Fmt, Args)).
-define(WARN(Fmt, Args), lager:warning(Fmt, Args)).
