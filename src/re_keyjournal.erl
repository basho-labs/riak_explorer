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
         read_cache/3,
         write/1,
         write_cache/2,
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

read(Meta, Start, Rows) ->
   case read_cache(Meta, Start, Rows) of
      false -> write(Meta);
      CacheData -> CacheData
   end.

read_cache({Operation, Node, Path}, Start, Rows) ->
   Dir = re_file_util:ensure_data_dir([atom_to_list(Operation), atom_to_list(Node)] ++ Path),
   {ok, Files} = file:list_dir(Dir),
   case Files of
      [File|_] -> 
         DirFile = filename:join([Dir, File]),
         {Total, ResultCount, _S, _E, Entries} = entries_from_file(DirFile, Start - 1, Rows - 1),
         [{Operation, [{total, Total},{count, ResultCount},{created, list_to_binary(File)},{Operation, Entries}]}];
      [] -> false
   end.

write({Operation, Node, Path}) ->
   case whereis(Operation) of
      undefined -> 
         Pid = spawn(?MODULE, handle_list, [{Operation, Node, Path}]),
         register(Operation, Pid),
         true;
      _ -> false
   end.

write_cache({Operation, Node, Path}, Objects) ->
   Dir = re_file_util:ensure_data_dir([atom_to_list(Operation), atom_to_list(Node)] ++ Path),
   {ok, Files} = file:list_dir(Dir),
   DirFile = case Files of
      [File|_] -> filename:join([Dir, File]);
      [] -> filename:join([Dir, timestamp_string()])
   end,

   {ok, Device} = file:open(DirFile, [append]),
   update_cache(Objects, Device),
   file:close(Device).

%%%===================================================================
%%% Callbacks
%%%===================================================================

handle_list({buckets, Node, [BucketType]}=Meta) ->
   C = re_riak:client(Node),
   Stream = riakc_pb_socket:stream_list_buckets(C, list_to_binary(BucketType)),
   handle_stream(Meta, Stream);
handle_list({keys, Node, [BucketType, Bucket]}=Meta) ->
   C = re_riak:client(Node),
   B = {list_to_binary(BucketType), list_to_binary(Bucket)},
   Stream = riakc_pb_socket:stream_list_keys(C, B),
   handle_stream(Meta, Stream).

handle_stream({Operation, Node, Path}, {ok, ReqId}) ->
   Dir = re_file_util:ensure_data_dir([atom_to_list(Operation), atom_to_list(Node)] ++ Path),
   TimeStamp = timestamp_string(),
   FileName = filename:join([Dir, TimeStamp]),
   file:write_file(FileName, "", [write]),
   lager:info("list started for file: ~p at: ~p", [FileName, TimeStamp]),
   {ok, Device} = file:open(FileName, [append]),
   write_loop(ReqId, Device);
handle_stream({Operation, _Node, Path}, Error) ->
   lager:error(atom_to_list(Operation) ++ " list failed for path: ~p with reason: ~p", [Path, Error]).

write_loop(ReqId, Device) ->
    receive
        {ReqId, done} -> 
            lager:info("list finished for file"),
            file:close(Device);
        {ReqId, {error, Reason}} -> 
            lager:error("list failed for file with reason: ~p", [Reason]),
            file:close(Device);
        {ReqId, {_, Res}} -> 
            Device1 = case Res of
               "" -> Device;
               Entries -> update_cache(Entries, Device)
            end,
            write_loop(ReqId, Device1)
    end.

%%%===================================================================
%%% Private
%%%===================================================================

update_cache([], Device) ->
   Device;
update_cache([Object|Rest], Device) ->
   io:fwrite(Device, binary_to_list(Object) ++ "~n", []),
   update_cache(Rest, Device).
   
timestamp_string() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(now()),
    lists:flatten(io_lib:fwrite("~4..0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B",[Year, Month, Day, Hour, Min, Sec])).

entries_from_file(File, Start, Rows) ->
   re_file_util:for_each_line_in_file(File,
      fun(Entry, {T, RC, S, E, Accum}) ->
         case should_add_entry(T, S, E) of
            true -> 
               B = re:replace(Entry, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
               {T + 1, RC + 1, S, E, [list_to_binary(B)|Accum]};
            _ -> {T + 1, RC, S, E, Accum}
         end
      end, [read], {0, 0, Start, Start+Rows,[]}).

should_add_entry(Total, _Start, Stop) when Total > Stop -> false;
should_add_entry(Total, Start, _Stop) when Total >= Start -> true;
should_add_entry(_Total, _Start, _Stop) -> false.
