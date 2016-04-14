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

-module(re_job).

-behaviour(gen_fsm).

-export([start_link/0]).

-export([start_job/2,
         get_info/1,
         set_meta/2,
         set_error/2,
         set_finish/1]).

-export([ready/2,
         ready/3,
         started/2, 
         started/3,
         failed/2,
         failed/3,
         finished/2,
         finished/3]).

-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-record(state, {
          proc :: pid() | undefined,
          meta :: term() | undefined,
          error :: term() | undefined
         }).

-type(state_name() :: atom()).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

-spec start_job(pid(), term()) -> {error, term()} | ok.
start_job(Pid, MFA) ->
    gen_fsm:sync_send_event(Pid, {start_job, MFA}).

-spec get_info(pid()) -> {error, term()} | [{atom(), term()}].
get_info(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_info).

-spec set_meta(pid(), term()) -> {error, term()} | ok.
set_meta(Pid, Meta) ->
    gen_fsm:sync_send_event(Pid, {set_meta, Meta}).

-spec set_error(pid(), term()) -> {error, term()} | ok.
set_error(Pid, Error) ->
    gen_fsm:sync_send_all_state_event(Pid, {set_error, Error}).

-spec set_finish(pid()) -> {error, term()} | ok.
set_finish(Pid) ->
    gen_fsm:sync_send_event(Pid, set_finish).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec init({atom(), {module(), atom(), [term()]}}) -> {ok, atom(), #state{}}.
init([]) ->
    {ok, ready, #state{}}.

-type(async_reply() ::
        {next_state, state_name(), #state{}} |
        {next_state, state_name(), #state{}, timeout()} |
        {stop, term(), #state{}}).

-spec ready(term(), #state{}) -> async_reply().
ready(_Event, State) ->
    {stop, {error, unhandled_event}, State}.

-spec started(term(), #state{}) -> async_reply().
started(_Event, State) ->
    {stop, {error, unhandled_event}, State}.

-spec failed(term(), #state{}) -> async_reply().
failed(_Event, State) ->
    {stop, {error, unhandled_event}, State}.

-spec finished(term(), #state{}) -> async_reply().
finished(_Event, State) ->
    {stop, {error, unhandled_event}, State}.

-type(sync_reply() ::
        {next_state, state_name(), #state{}} |
        {next_state, state_name(), #state{}, timeout()} |
        {reply, term(), state_name(), #state{}} |
        {reply, term(), state_name(), #state{}, timeout()} |
        {stop, term(), #state{}} |
        {stop, term(), term(), #state{}}).

-spec ready(term(), pid(), #state{}) -> sync_reply().
ready({start_job, MFA}, _From, _State) ->
    do_start_job(MFA, #state{});
ready(_Event, _From, State) ->
    {reply, {error, unhandled_event}, ready, State}.

-spec started(term(), pid(), #state{}) -> sync_reply().
started({start_job, _}, _From, State) ->
    {reply, {error, already_started}, started, State};
started({set_meta, Meta}, _From, State) ->
    State1 = State#state{meta=Meta},
    {reply, ok, started, State1};
started(set_finish, _From, State) ->
    {reply, ok, finished, State};
started(_Event, _From, State) ->
    {reply, {error, unhandled_event}, ready, State}.

-spec failed(term(), pid(), #state{}) -> sync_reply().
failed({start_job, MFA}, _From, _State) ->
    do_start_job(MFA, #state{});
failed(_Event, _From, State) ->
    {reply, {error, unhandled_event}, ready, State}.

-spec finished(term(), pid(), #state{}) -> sync_reply().
finished({start_job, MFA}, _From, _State) ->
    do_start_job(MFA, #state{});
finished(_Event, _From, State) ->
    {reply, {error, unhandled_event}, ready, State}.

-spec handle_event(term(), state_name(), #state{}) -> async_reply().
handle_event(_Event, _StateName, State) ->
    {stop, {error, unhandled_event}, State}.

-spec handle_sync_event(term(), pid(), state_name(), #state{}) ->
                               sync_reply().
handle_sync_event(get_info, _From, StateName, 
                  State=#state{meta=Meta, error=Error}) ->
    Info = [{state, StateName},
            {meta, Meta},
            {error, Error}],
    {reply, Info, StateName, State};
handle_sync_event({set_error, Error}, _From, _, State) ->
    State1 = State#state{error=Error},
    {reply, ok, failed, State1};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, unhandled_event}, StateName, State}.

-spec handle_info(term(), state_name(), #state{}) -> async_reply().
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec terminate(term(), state_name(), #state{}) -> ok.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(term(), state_name(), #state{}, term()) ->
                         {ok, state_name(), #state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Private
%%%===================================================================

do_start_job({M, F, A}, State) ->
    case erlang:apply(M, F, A) of
        P when is_pid(P) ->
            State1 = State#state{proc = P},
            {reply, ok, started, State1};
        {error, Reason} ->
            {reply, {error, Reason}, ready, State}
    end.
