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
-export([create/2, get_jobs/0, get/1, set_meta/2, error/2, finish/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

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

error(Id, Meta) ->
    gen_server:cast(?MODULE, {error, Id, Meta}).

finish(Id) ->
    gen_server:cast(?MODULE, {finish, Id}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({create, Id, {M, F, A}}, _From, State=#state{jobs=Jobs}) ->
    lager:info("Creating job: ~p, {~p, ~p, ~p}. Existing Jobs: ~p.", [Id, M, F, A, Jobs]),
    case proplists:get_value(Id, Jobs) of
        #job{status=in_progress} ->
            {reply, [{error, already_started}], State};
        _ ->
            Pid = spawn(M, F, A),
            J = #job{pid=Pid, status=in_progress, meta=[]},
            {reply, ok, State#state{jobs=put_job(Id, Jobs, J, [])}}
    end;

handle_call({get_jobs}, _From, State=#state{jobs=Jobs}) ->
    R = lists:map(fun({Id, #job{status=S,meta=M}}) -> [{id, Id},{status, S},{meta, M}] end, Jobs),
    {reply, R, State};

handle_call({get_info, Id}, _From, State=#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J=#job{} ->
            {reply, [{id, Id}, {status, J#job.status}, {meta, J#job.meta}], State};
        _ ->
            {reply, [{error, not_found}], State}
    end.

handle_cast({set_meta, Id, Meta}, State=#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J=#job{} ->
            {noreply, State#state{
                jobs=put_job(Id, Jobs, J#job{meta=Meta}, [])}};
        _ ->
            {noreply, State}
    end;

handle_cast({error, Id, Meta}, State=#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J=#job{} ->
            {noreply, State#state{
                jobs=put_job(Id, Jobs, J#job{status=error, meta=Meta}, [])}};
        _ ->
            {noreply, State}
    end;

handle_cast({finish, Id}, State=#state{jobs=Jobs}) ->
    case proplists:get_value(Id, Jobs) of
        J=#job{} ->
            {noreply, State#state{
                jobs=put_job(Id, Jobs, J#job{status=done}, [])}};
        _ ->
            {noreply, State}
    end.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    lager:error("Job manager terminated, reason: ~p", [Reason]),
    ok.

%%%===================================================================
%%% Private
%%%===================================================================

put_job(Id, [], Job, Accum) ->
    case proplists:is_defined(Id, Accum) of
        true ->
            lists:reverse(Accum);
        false ->
            lists:reverse([{Id, Job}|Accum])
    end;
put_job(Id, [{Id, _}|Rest], Job, Accum) ->
    put_job(Id, Rest, Job, [{Id, Job}|Accum]);
put_job(Id, [Old|Rest], Job, Accum) ->
    put_job(Id, Rest, Job, [Old|Accum]).
