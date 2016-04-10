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

-export([start_link/2]).

-export([get_info/1,
         set_meta/2,
         set_error/2,
         set_finish/1]).

-export([ready/2,
         ready/3,
         started/2, 
         started/3,
         error/2,
         error/3,
         finished/2,
         finished/3]).

-export([init/1, 
         handle_event/3,
         handle_sync_event/4, 
         handle_info/3, 
         terminate/3, 
         code_change/4]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom(), {module(), atom(), [term()]}) -> 
                        {ok, pid()} | {error, term()}.
start_link(Id, MFA) ->
    gen_fsm:start_link(?MODULE, {Id, MFA}, []).

get_info(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, get_info).

set_meta(Pid, Meta) ->
    case gen_fsm:sync_send_event(Pid, {status_update, TaskStatus, Reason}) of
        ok ->
            ok;
        {error, E} ->
            {error, E}
    end.

set_error/2,
set_finish/1]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec init({atom(), {module(), atom(), [term()]}}) -> {ok, atom(), #state{}}.
init({_Id, _MFA}) ->
    %% Start the job, probably don't need ready
    {ok, ready, #state{}}.

-type(async_reply() ::
        {next_state, atom(), #state{}} |
        {next_state, atom(), #state{}, timeout()} |
        {stop, term(), #state{}}).

-spec ready(term(), #state{}) -> async_reply().
ready(_Event, State) ->
    {next_state, ready, State}.

-spec started(term(), #state{}) -> async_reply().
started(_Event, State) ->
    {next_state, ready, State}.

-spec error(term(), #state{}) -> async_reply().
error(_Event, State) ->
    {next_state, ready, State}.

-spec finished(term(), #state{}) -> async_reply().
finished(_Event, State) ->
    {next_state, ready, State}.

-type(sync_reply() ::
        {next_state, atom(), #state{}} |
        {next_state, atom(), #state{}, timeout()} |
        {reply, term(), atom(), #state{}} |
        {reply, term(), atom(), #state{}, timeout()} |
        {stop, term(), #state{}} |
        {stop, term(), term(), #state{}}).

-spec ready(term(), pid(), #state{}) -> sync_reply().
ready(_Event, _From, State) ->
    {reply, ok, ready, State}.

-spec started(term(), pid(), #state{}) -> sync_reply().
started(_Event, _From, State) ->
    {reply, ok, ready, State}.

-spec error(term(), pid(), #state{}) -> sync_reply().
error(_Event, _From, State) ->
    {reply, ok, ready, State}.

-spec finished(term(), pid(), #state{}) -> sync_reply().
finished(_Event, _From, State) ->
    {reply, ok, ready, State}.

-spec handle_event(term(), atom(), #state{}) -> async_reply().
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

-spec handle_sync_event(term(), pid(), atom(), #state{}) ->
                               sync_reply().
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

-spec handle_info(term(), atom(), #state{}) -> async_reply().
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

-spec terminate(term(), atom(), #state{}) -> ok.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(term(), atom(), #state{}, term()) ->
                         {ok, atom(), #state{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Private
%%%===================================================================
