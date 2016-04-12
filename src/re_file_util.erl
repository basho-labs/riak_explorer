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

-module(re_file_util).

-export([find_single_file/1,
         clean_dir/1,
         partial_file/3,
         timestamp_string/0,
         timestamp_human/1,
         sort_file/1,
         add_slash/1,
         ensure_data_dir/1,
         for_each_line_in_file/4]).

%%%===================================================================
%%% API
%%%===================================================================

-spec find_single_file(string()) -> {error, term()} | string().
find_single_file(Dir) ->
    case file:list_dir(Dir) of
        {ok, [File|_]} ->
            File;
        {ok, []} -> 
            {error, not_found};
        {error, Reason} -> 
            {error, Reason}
    end.

-spec clean_dir(string()) -> {error, term()} | ok.
clean_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, OldFiles} ->
            [ file:delete(filename:join([Dir, F])) || F <- OldFiles ],
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec partial_file(string(), non_neg_integer(), non_neg_integer()) -> 
                          {non_neg_integer(), non_neg_integer(), 
                           non_neg_integer(), non_neg_integer(), 
                           [string()]}.
partial_file(File, Start, Rows) ->
   {T, RC, S, E, LinesR} = for_each_line_in_file(File,
      fun(Entry, {T, RC, S, E, Accum}) ->
         case should_add_entry(T, S, E) of
            true ->
               B = re:replace(Entry, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
               {T + 1, RC + 1, S, E, [list_to_binary(B)|Accum]};
            _ -> {T + 1, RC, S, E, Accum}
         end
     end, [read], {0, 0, Start, Start+Rows,[]}),
   {T, RC, S, E, lists:reverse(LinesR)}.

-spec timestamp_string() -> string().
timestamp_string() ->
    %% change erlang:now() to erlang:timestamp() for R18 in the future
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(erlang:now()),
    lists:flatten(io_lib:fwrite("~4..0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B",[Year, Month, Day, Hour, Min, Sec])).

-spec timestamp_human(string()) -> string().
timestamp_human(Time) ->
   Year = lists:sublist(Time, 4),
   Month = lists:sublist(Time, 5, 2),
   Day = lists:sublist(Time, 7, 2),
   Hour = lists:sublist(Time, 9, 2),
   Min = lists:sublist(Time, 11, 2),
   Sec = lists:sublist(Time, 13, 2),
   lists:flatten(io_lib:fwrite("~s-~s-~s ~s:~s:~s",[Year, Month, Day, Hour, Min, Sec])).

-spec sort_file(string()) -> ok.
sort_file(Filename) ->
    UnsortedLines = re_file_util:for_each_line_in_file(Filename,
       fun(Entry, Accum) -> [string:strip(Entry, both, $\n)| Accum] end,
       [read], []),
    SortedLines = lists:sort(UnsortedLines),
    NewFile = string:join(SortedLines, io_lib:nl()),
    file:write_file(Filename, NewFile, [write]),
    ok.


add_slash(Str) ->
    case lists:last(Str) of
        47 -> Str;
        _ -> Str ++ "/"
    end.

ensure_data_dir(Path) ->
    DataPath = add_slash(filename:join([re_config:data_dir() | Path])),
    filelib:ensure_dir(DataPath),
    DataPath.

for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).
 
%%%===================================================================
%%% Private
%%%===================================================================

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> 
            file:close(Device), Accum;
        Line -> 
            NewAccum = Proc(Line, Accum),
            for_each_line(Device, Proc, NewAccum)
    end.

should_add_entry(Total, _Start, Stop) when Total > Stop -> false;
should_add_entry(Total, Start, _Stop) when Total >= Start -> true;
should_add_entry(_Total, _Start, _Stop) -> false.
