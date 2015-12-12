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

-include("riak_explorer.hrl").
-export([add_slash/1,ensure_data_dir/1,for_each_line_in_file/4]).

%%%===================================================================
%%% API
%%%===================================================================

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