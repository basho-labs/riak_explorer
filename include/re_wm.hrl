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

-define(ACCEPT(T), {T, accept_content}).
-define(PROVIDE(T), {T, provide_content}).
-define(JSON_TYPE, "application/json").
-define(TEXT_TYPE, "plain/text").
-define(OCTET_TYPE, "application/octet-stream").
-define(FORM_TYPE, "application/x-www-form-urlencoded").
-define(PROVIDE_TEXT, [{?TEXT_TYPE, provide_text_content}]).
-define(ACCEPT_TEXT, [?ACCEPT(?FORM_TYPE),
                      ?ACCEPT(?OCTET_TYPE),
                      ?ACCEPT(?TEXT_TYPE),
                      ?ACCEPT(?JSON_TYPE)]).

-record(route, {base = [] :: [string()],
                path :: [string() | atom()],
                available = true :: {module(), atom()} | boolean(),
                methods = ['GET'] :: [atom()],
                accepts = [] :: [{string(), atom()}],
                provides = [?PROVIDE(?JSON_TYPE)] :: [{string(), atom()}] |
                           {module(), atom()},
                exists = true :: {module(), atom()} | boolean(),
                content = [{success, true}] :: {module(), atom()} |
                          nonempty_list(),
                accept = ?ACCEPT_TEXT :: {module(), atom()} | undefined,
                delete :: {module(), atom()} | undefined,
                post_create = false :: boolean(),
                post_path :: {module(), atom()} | undefined,
                last_modified :: {module(), atom()} | undefined}).

-type route() :: #route{}.
