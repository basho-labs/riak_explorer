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

-behaviour(supervisor).

-export([start_link/0]).

-export([add_job/2,
         get_job/1,
         get_jobs/0]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

-spec add_job(atom(), {module(), atom(), [term()]}) -> ok | {error, term()}.
add_job(Id, MFA) ->
    case get_job_pid(Id) of
        {ok, Pid} ->
            re_job:start_job(Pid, MFA);
        {error, not_found} ->
            JobSpec = 
                {Id,
                 {re_job, start_link, [Id]},
                 transient, 5000, worker, [re_job]},
            case supervisor:start_child(?MODULE, JobSpec) of
                {ok, Pid} ->
                    re_job:start_job(Pid, MFA);
                {error, {already_started, Pid}} ->
                    re_job:start_job(Pid, MFA);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec get_job(atom()) -> {error, term()} | term().
get_job(Id) ->
    case get_job_pid(Id) of
        {ok, Pid} ->
            re_job:get_info(Pid);
        {error, not_found} ->
            {error, not_found}
    end.

-spec get_jobs() -> [{atom(), term()}].
get_jobs() ->
    [ {Id, re_job:get_info(Pid)} 
      || {Id, Pid} <- get_job_pids() ].

-spec get_job_pid(rms_node:key()) -> {error, not_found} | {ok, pid()}.
get_job_pid(Id) ->
    case lists:keyfind(Id, 1, get_job_pids()) of
        {_, Pid} ->
            {ok, Pid};
        false ->
            {error, not_found}
    end.

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec init({}) ->
                  {ok, {{supervisor:strategy(), 1, 1}, [supervisor:child_spec()]}}.
init({}) ->
    {ok, {{one_for_one, 1, 1}, []}}.

%% get_jobs() ->
%%     gen_server:call(?MODULE, {get_jobs}).

%% get(Id) ->
%%     gen_server:call(?MODULE, {get_info, Id}).

%% set_meta(Id, Meta) ->
%%     gen_server:cast(?MODULE, {set_meta, Id, Meta}).

%% error(Id, Meta) ->
%%     gen_server:cast(?MODULE, {error, Id, Meta}).

%% finish(Id) ->
%%     gen_server:cast(?MODULE, {finish, Id}).

%% init(_Args) ->
%%     process_flag(trap_exit, true),
%%     {ok, #state{}}.

%% handle_call({create, Id, {M, F, A}}, _From, State=#state{jobs=Jobs}) ->
%%     lager:info("Creating job: ~p, {~p, ~p, ~p}. Existing Jobs: ~p.", [Id, M, F, A, Jobs]),
%%     case proplists:get_value(Id, Jobs) of
%%         #job{status=in_progress} ->
%%             {reply, [{error, already_started}], State};
%%         _ ->
%%             Pid = spawn(M, F, A),
%%             J = #job{pid=Pid, status=in_progress, meta=[]},
%%             {reply, ok, State#state{jobs=put_job(Id, Jobs, J, [])}}
%%     end;

%% handle_call({get_jobs}, _From, State=#state{jobs=Jobs}) ->
%%     R = lists:map(fun({Id, #job{status=S,meta=M}}) -> [{id, Id},{status, S},{meta, M}] end, Jobs),
%%     {reply, R, State};

%% handle_call({get_info, Id}, _From, State=#state{jobs=Jobs}) ->
%%     case proplists:get_value(Id, Jobs) of
%%         J=#job{} ->
%%             {reply, [{id, Id}, {status, J#job.status}, {meta, J#job.meta}], State};
%%         _ ->
%%             {reply, [{error, not_found}], State}
%%     end.

%% handle_cast({set_meta, Id, Meta}, State=#state{jobs=Jobs}) ->
%%     case proplists:get_value(Id, Jobs) of
%%         J=#job{} ->
%%             {noreply, State#state{
%%                 jobs=put_job(Id, Jobs, J#job{meta=Meta}, [])}};
%%         _ ->
%%             {noreply, State}
%%     end;

%% handle_cast({error, Id, Meta}, State=#state{jobs=Jobs}) ->
%%     case proplists:get_value(Id, Jobs) of
%%         J=#job{} ->
%%             {noreply, State#state{
%%                 jobs=put_job(Id, Jobs, J#job{status=error, meta=Meta}, [])}};
%%         _ ->
%%             {noreply, State}
%%     end;

%% handle_cast({finish, Id}, State=#state{jobs=Jobs}) ->
%%     case proplists:get_value(Id, Jobs) of
%%         J=#job{} ->
%%             {noreply, State#state{
%%                 jobs=put_job(Id, Jobs, J#job{status=done}, [])}};
%%         _ ->
%%             {noreply, State}
%%     end.

%% handle_info({'EXIT', _Pid, _Reason}, State) ->
%%     {noreply, State}.

%% code_change(_OldVsn, State, _Extra) ->
%%     {ok, State}.

%% terminate(Reason, _State) ->
%%     lager:error("Job manager terminated, reason: ~p", [Reason]),
%%     ok.

%%%===================================================================
%%% Private
%%%===================================================================

%% put_job(Id, [], Job, Accum) ->
%%     case proplists:is_defined(Id, Accum) of
%%         true ->
%%             lists:reverse(Accum);
%%         false ->
%%             lists:reverse([{Id, Job}|Accum])
%%     end;
%% put_job(Id, [{Id, _}|Rest], Job, Accum) ->
%%     put_job(Id, Rest, Job, [{Id, Job}|Accum]);
%% put_job(Id, [Old|Rest], Job, Accum) ->
%%     put_job(Id, Rest, Job, [Old|Accum]).

-spec get_job_pids() -> [{atom(), pid()}].
get_job_pids() ->
    [ {Id, Pid} || {Id, Pid, _, _} <- supervisor:which_children(?MODULE) ].
