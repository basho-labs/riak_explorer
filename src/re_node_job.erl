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
-module(re_node_job).

-export([start_list_buckets/4,
         start_list_keys/5,
         start_delete_bucket/5]).

-export([init_list_buckets/1,
         init_list_keys/1,
         init_delete_bucket/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_list_buckets(re_cluster:re_cluster(), re_node:re_node(), binary(), [term()]) ->
                                pid().
start_list_buckets(Cluster, Node, BucketType, Options) ->
    spawn(?MODULE, init_list_buckets, [[self(), Cluster, Node, BucketType, Options]]).

-spec start_list_keys(re_cluster:re_cluster(), re_node:re_node(), binary(), binary(), [term()]) ->
                                pid().
start_list_keys(Cluster, Node, BucketType, Bucket, Options) ->
    spawn(?MODULE, init_list_keys, [[self(), Cluster, Node, BucketType, Bucket, Options]]).

-spec start_delete_bucket(re_cluster:re_cluster(), re_node:re_node(), binary(), binary(), [term()]) ->
                                 pid().
start_delete_bucket(Cluster, Node, BucketType, Bucket, Options) ->
    spawn(?MODULE, init_delete_bucket, [[self(), Cluster, Node, BucketType, Bucket, Options]]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec init_list_buckets([term()]) -> {error, term()} | ok.
init_list_buckets([From, Cluster, Node, BucketType, Options]) ->
    Dir = re_file_util:ensure_data_dir(
                    ["buckets", atom_to_list(Cluster), binary_to_list(BucketType)]),
    C = re_node:client(Node),
    Req = riakc_pb_socket:stream_list_buckets(C, BucketType),
    do_list(From, Req, Dir, Options).

-spec init_list_keys([term()]) -> {error, term()} | ok.
init_list_keys([From, Cluster, Node, BucketType, Bucket, Options]) ->
    Dir = re_file_util:ensure_data_dir(
                    ["keys", atom_to_list(Cluster), binary_to_list(BucketType),
                     binary_to_list(Bucket)]),
    C = re_node:client(Node),
    Req = riakc_pb_socket:stream_list_keys(C, {BucketType, Bucket}),
    do_list(From, Req, Dir, Options).

-spec init_delete_bucket([term()]) -> {error, term()} | ok.
init_delete_bucket([From, Cluster, Node, BucketType, Bucket, Options]) ->
    Dir = re_file_util:ensure_data_dir(
                    ["keys", atom_to_list(Cluster), binary_to_list(BucketType),
                     binary_to_list(Bucket)]),
    case re_file_util:find_single_file(Dir) of
        {error, Reason} -> 
            {error, Reason};
        File ->
            DirFile = filename:join([Dir, File]),
            C = re_node:client(Node),
            Fun = 
                fun(Entry0, {Oks0, Errors0}) ->
                        RT = BucketType,
                        RB = Bucket,
                        RK = list_to_binary(re:replace(Entry0, "(^\\s+)|(\\s+$)", "", [global,{return,list}])),
                        {Oks1,Errors1} = 
                            case riakc_pb_socket:delete(C, {RT,RB}, RK) of
                                ok ->
                                    riakc_pb_socket:get(C, {RT,RB}, RK),
                                    {Oks0+1, Errors0};
                                {error, Reason} ->
                                    lager:warning("Failed to delete types/~p/buckets/~p/keys/~p with reason ~p", 
                                                  [RT, RB, RK, Reason]),
                                    {Oks0, Errors0+1}
                            end,
                        re_job:set_meta(From, [{oks, Oks1},{errors,Errors1}]),
                        {Oks1,Errors1}
                end,
            {Os,Es} = re_file_util:for_each_line_in_file(DirFile, Fun, [read], {0, 0}),
            lager:info("Completed deletion of types/~p/buckets/~p with ~p successful deletes and ~p errors", 
                       [BucketType, Bucket, Os, Es]),
            re_job:set_finish(From),
            case proplists:get_value(refresh_cache, Options, false) of
                true ->
                    re_node:clean_buckets_cache(Cluster, BucketType),
                    re_node:clean_keys_cache(Cluster, BucketType, Bucket),
                    ok;
                false ->
                    ok
            end
    end.

%%%===================================================================
%%% Private
%%%===================================================================

do_list(From, {ok, ReqId}, Dir, Options) ->
    re_file_util:clean_dir(Dir),
    TimeStamp = re_file_util:timestamp_string(),
    Filename = filename:join([Dir, TimeStamp]),
    file:write_file(Filename, "", [write]),
    lager:info("List started for file: ~p.", [Filename]),
    {ok, Device} = file:open(Filename, [append]),
    case write_loop(From, ReqId, Device, []) of
        ok -> 
            case proplists:get_value(sort, Options, true) of
                true -> 
                    re_file_util:sort_file(Filename);
                _ -> 
                    ok
            end,
            lager:info("File ~p written.", [Filename]),
            re_job:set_finish(From),
            file:close(Device),
            ok;
        {error, Reason} ->
            re_job:set_error(From, Reason),
            file:close(Device),
            {error, Reason}
    end;
do_list(From, {error, Reason}, Dir, _) ->
    re_job:set_error(From, Reason),
    lager:error("Bucket list failed for dir: ~p with reason: ~p", [Dir, Reason]),
    {error, Reason}.

-spec write_loop(pid(), term(), term(), term()) ->
                        {error, term()} | ok.
write_loop(From, ReqId, Device, Meta) ->
    case re_job:set_meta(From, Meta) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            receive
                {ReqId, done} ->
                    ok;
                {ReqId, {error, Reason}} ->
                    {error, Reason};
                {ReqId, {_, Entries}} ->
                    case length(Entries) > 0 of
                        true ->
                            Entries1 = [ binary_to_list(E) || E <- Entries ],
                            io:fwrite(Device, string:join(Entries1, io_lib:nl()), [])
                    end,
                    Meta1 = [{count, proplists:get_value(count, Meta, 0) + length(Entries)}],
                    write_loop(From, ReqId, Device, Meta1)
            end
                
    end.
