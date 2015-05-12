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

-module(re_job_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("riak_explorer.hrl").

-record(job, {pid, status, meta}).
-record(state, {jobs=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Id, {M, F, A}) ->
    gen_server:call(?MODULE, {create, Id, {M, F, A}}).

get_jobs() ->
    gen_server:call(?MODULE, {get_jobs}).

get(Id) ->
    gen_server:call(?MODULE, {get_info, Id}).

set_meta(Id, Meta) ->
    gen_server:cast(?MODULE, {set_meta, Id, Meta}).

% cancel(Id) ->

error(Id, Meta) ->
    gen_server:cast(?MODULE, {error, Id, Meta}).

finish(Id) ->
    gen_server:cast(?MODULE, {finish, Id}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_Args) ->
    {ok, channels()}.

handle_call({create, Id, {M, F, A}}, _From, State#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J#job{status=in_progress} -> 
            {reply, {error, already_started}, State};
        _ ->
            Pid = spawn(M, F, A),
            J = #job{pid=Pid, status=in_progress, meta=[]},
            {reply, ok, State#state{jobs=put_job(Id, Jobs, J, [])}};
        _ ->
    end.

% handle_call({get_info, Id}, _From, State#state{jobs=Jobs}) ->
%     case proplists:get_value(Id, Jobs) of
%         J -> 
%             {reply, {J#job.status, J#job.meta}, State};
%         _ ->
%             {reply, {error, not_found}, State};
%     end.

handle_call({get_info, Id}, _From, State#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J -> 
            {reply, {J#job.status, J#job.meta}, State};
        _ ->
            {reply, {error, not_found}, State};
    end.

handle_cast({set_meta, Id, Meta}, State#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J -> 
            {noreply, State#{
                jobs=put_job(Id, Jobs, J#job{meta=Meta}, [])}}.
        _ ->
            {noreply, State}
    end.

handle_cast({error, Id, Meta}, State#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J -> 
            {noreply, State#{
                jobs=put_job(Id, Jobs, J#job{status=error, meta=Meta}, [])}}.
        _ ->
            {noreply, State}
    end.

handle_cast({finish, Id}, State#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J -> 
            {noreply, State#{
                jobs=put_job(Id, Jobs, J#job{status=done}, [])}}.
        _ ->
            {noreply, State}
    end.

%%%===================================================================
%%% Private
%%%===================================================================

put_job(Id, [], Job, Accum) ->
    case proplists:is_defined(Id, Accum) of
        true -> lists:reverse(Accum);
        false -> lists:reverse([{Id, Job}|Accum])
    end;
put_job(Id, [{Id, _}|Rest], Job, Accum) ->
    update_job(Id, Rest, Job, [{Id, Job}|Accum]);
put_job(_, [{Id, Job}|Rest], _, Accum) ->
    update_job(Id, Rest, Job, [{Id, Job}]).