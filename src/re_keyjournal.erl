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

-module(re_keyjournal).
-export([clean/1,
         read/3,
         write/1,
         handle_list/1]).

-include("riak_explorer.hrl").

%%%===================================================================
%%% API
%%%===================================================================

clean({Operation, Node, Path}) ->
   Dir = re_file_util:ensure_data_dir([atom_to_list(Operation), atom_to_list(Node)] ++ Path),
   {ok, Files} = file:list_dir(Dir),
   case Files of
      [File|_] -> 
         DirFile = filename:join([Dir, File]),
         file:delete(DirFile),
         true;
      [] ->
         false
   end.

read({Operation, Node, Path}, Start, Rows) ->
   Dir = re_file_util:ensure_data_dir([atom_to_list(Operation), atom_to_list(Node)] ++ Path),
   {ok, Files} = file:list_dir(Dir),
   case Files of
      [File|_] -> 
         DirFile = filename:join([Dir, File]),
         {Total, ResultCount, _S, _E, Entries} = entries_from_file(DirFile, Start - 1, Rows),
         [{Operation, [{total, Total},{count, ResultCount},{created, list_to_binary(File)},{Operation, Entries}]}];
      [] ->
         write({Operation, Node, Path})
   end.

write({Operation, Node, BucketType}) ->
   case whereis(Operation) of
      undefined -> 
         Pid = spawn(?MODULE, handle_list, [{Operation, Node, BucketType}]),
         register(Operation, Pid),
         true;
      _ -> false
   end.

%%%===================================================================
%%% Callbacks
%%%===================================================================

% handle_list({Operation, Node, [BucketType, Bucket]}) ->
handle_list({Operation, Node, [BucketType]=Path}) ->
   C = re_riak:client(Node),
   case riakc_pb_socket:stream_list_buckets(C, list_to_binary(BucketType)) of
      {ok, ReqId} ->
         Dir = re_file_util:ensure_data_dir([atom_to_list(Operation), atom_to_list(Node)] ++ Path),
         TimeStamp = timestamp_string(),
         FileName = filename:join([Dir, TimeStamp]),
         file:write_file(FileName, "", [write]),
         lager:info("list_buckets started for file: ~p at: ~p", [FileName, TimeStamp]),
         {ok, Device} = file:open(FileName, [append]),
         write_loop(ReqId, Device);
      Error ->
         lager:error(atom_to_list(Operation) ++ " list failed for path: ~p with reason: ~p", [Path, Error])
   end.

write_loop(ReqId, Device) ->
   lager:info("in loop: ~p", [ReqId]),
    receive
        {ReqId, done} -> 
            lager:info("list_buckets finished for file"),
            file:close(Device);
        {ReqId, {error, Reason}} -> 
            lager:error("list_buckets failed for file with reason: ~p", [Reason]),
            file:close(Device);
        {ReqId, {_, Res}} -> 
            io:fwrite(Device, Res ++ "~n", []),
            write_loop(ReqId, Device)
    end.

%%%===================================================================
%%% Private
%%%===================================================================

timestamp_string() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(now()),
    lists:flatten(io_lib:fwrite("~4..0B~2.10.0B~2.10.0B~2B~2.10.0B~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])).

entries_from_file(File, Start, Rows) ->
   re_file_util:for_each_line_in_file(File,
      fun(Bucket, {T, RC, S, E, Accum}) ->
         lager:info("accum: ~p", [Accum]),
         case should_add_entry(T, S, E) of
            true -> 
               B = re:replace(Bucket, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
               {T + 1, RC + 1, S, E, [list_to_binary(B)|Accum]};
            _ -> {T + 1, RC, S, E, Accum}
         end
      end, [read], {0, 0, Start, Start+Rows-1,[]}).

should_add_entry(Total, _Start, Stop) when Total > Stop -> false;
should_add_entry(Total, Start, _Stop) when Total >= Start -> true;
should_add_entry(_Total, _Start, _Stop) -> false.
