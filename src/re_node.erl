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

-module(re_node).

-type(re_node() :: node()).
-export_type([re_node/0]).

-type(re_node_type() :: oss | ee | ts | ts_ee).
-export_type([re_node_type/0]).

-type(re_node_prop() ::
        {id, node()} |
        {riak_type, re_node_type() | unavailable} |
        {riak_version, binary() | unavailable} |
        {available, boolean()}).

-type(re_node_props() :: [re_node_prop()]).
-export_type([re_node_props/0]).

-type(ts_table() :: string() | binary()).
-type(ts_key() :: [term()]).
-type(ts_result() :: [{atom(), term()}]).

-export([exists/1,
         props/1,
         type/1,
         version/1,
         available/1,
         ring_members/1]).

-export([delete_bucket/4,
         delete_bucket/5,
         get_ts/3,
         put_ts/3,
         query_ts/2]).

-export([list_buckets/4,
         list_buckets_cache/4,
         clean_buckets_cache/2,
         put_buckets_cache/3,
         list_keys/5,
         list_keys_cache/5,
         clean_keys_cache/3,
         put_keys_cache/4,
         list_keys_ts/4,
         list_keys_ts_cache/4,
         put_keys_ts_cache/3,
         clean_keys_ts_cache/2]).

-export([log_files/1,
         log_file/3,
         log_file_exists/2,
         config_file/2,
         config_files/1,
         config_file_exists/2,
         config/1,
         http_listener/1,
         pb_listener/1,
         bucket_type_exists/2,
         bucket_type/2,
         bucket_types/1,
         table_exists/2,
         table/2,
         tables/1,
         create_bucket_type/3]).

-export([command/4,
         client/1]).

-export([pb_messages_create/2,
         pb_messages_exists/2,
         pb_messages/2]).

-include_lib("gpb/include/gpb.hrl").
%%%===================================================================
%%% API
%%%===================================================================

-spec exists(re_node()) -> boolean().
exists(N) ->
    available(N).

-spec props(re_node()) -> re_node_props().
props(N) ->
    [{id,N},
     {riak_type, type(N)},
     {riak_version, version(N)},
     {available, available(N)}].

-spec type(re_node()) -> re_node_type() | unavailable.
type(N) ->
    case {command(N, re_riak_patch, is_enterprise, []),
          command(N, re_riak_patch, is_timeseries, [])} of
        {false, false} -> oss;
        {true, false} -> ee;
        {false, true} -> ts;
        {true, true} -> ts_ee;
        _ -> unavailable
    end.

-spec version(re_node()) -> binary() | unavailable.
version(N) ->
    case command(N, re_riak_patch, riak_version, []) of
        {error, _} ->
            unavailable;
        Version ->
            Version
    end.

-spec available(re_node()) -> boolean().
available(N) ->
    is_atom(command(N, erlang, node, [])).

-spec ring_members(re_node()) -> {error, term()} | [re_node()].
ring_members(N) ->
    case command(N, riak_core_ring_manager, get_my_ring, []) of
        {error, Reason} ->
            {error, Reason};
        {ok, MyRing} ->
            case command(N, riak_core_ring, all_members, [MyRing]) of
                {error, Reason} ->
                    {error, Reason};
                Members ->
                    lists:sort(Members)
            end
    end.

-spec get_ts(re_node(), ts_table(), ts_key()) ->
                    {error, term()} | ts_result().
get_ts(Node, Table, Key) ->
    C = client(Node),
    case riakc_ts:get(C, Table, Key, []) of
        {ok, {Fields, Rows}} ->
            [{fields, Fields},{rows, Rows}];
        {error, Reason} ->
            {error, Reason}
    end.

-spec put_ts(re_node(), ts_table(), [term()]) ->
                    {error, term()} | ok.
put_ts(Node, Table, Rows) ->
    C = client(Node),
    Rows1 = [ list_to_tuple([ case V of
                    null -> undefined;
                    _ -> V
                end || V <- Vs ]) || Vs <- Rows ],
    lager:info("Putting Rows: ~p into table ~p", [Rows1, Table]),
    case riakc_ts:put(C, Table, Rows1) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec query_ts(re_node(), binary()) ->
                      {error, term()} | ts_result().
query_ts(Node, Query) ->
    C = client(Node),
    QueryStr = binary_to_list(Query),
    lager:info("Query: ~p", [QueryStr]),
    R = riakc_ts:query(C, QueryStr),
    lager:info("Results: ~p", [R]),
    {Fields, Rows} =
        case R of
            {ok, {[],[]}} ->
                {[],[]};
            {error, Reason} ->
                {error, Reason};
            {ok, {Fields1, TupleRows}} ->
                TupleToRowFun =
                    fun(Tuple) ->
                            [element(I,Tuple)
                             || I <- lists:seq(1,tuple_size(Tuple))]
                    end,
                Rows1 = lists:map(TupleToRowFun, TupleRows),
                {Fields1, Rows1}
        end,
    case {Fields, Rows} of
        {error, Reason1} ->
            {error, Reason1};
        _ ->
            [{fields, Fields},{rows, Rows}]
    end.

-spec list_keys_ts(re_cluster:re_cluster(), re_node(), binary(), [term()]) -> ts_result().
list_keys_ts(Cluster, Node, Table, Options) ->
    case re_cluster:development_mode(Cluster) of
        true ->
            re_job_manager:add_job(
              list_keys_ts,
              {re_node_job,
               start_list_keys_ts,
               [Cluster, Node, Table, Options]});
        false ->
            {error, developer_mode_off}
    end.

-spec delete_bucket(re_cluster:re_cluster(), re_node(), binary(), binary()) ->
                           {error, term()} | ok.
delete_bucket(Cluster, Node, BucketType, Bucket) ->
    delete_bucket(Cluster, Node, BucketType, Bucket, [{refresh_cache, true}]).

-spec delete_bucket(re_cluster:re_cluster(), re_node(), binary(), binary(), binary()) ->
                           {error, term()} | ok.
delete_bucket(Cluster, Node, BucketType, Bucket, Options) ->
    case re_cluster:development_mode(Cluster) of
        true ->
            re_job_manager:add_job(
              delete_bucket,
              {re_node_job,
               start_delete_bucket,
               [Cluster, Node, BucketType, Bucket, Options]});
        false ->
            lager:warning("Failed request to delete types/~p/buckets/~p because developer mode is off", [BucketType, Bucket]),
            {error, developer_mode_off}
    end.

-spec list_buckets(re_cluster:re_cluster(), re_node(), binary(), [term()]) ->
                          {error, term()} | ok.
list_buckets(Cluster, Node, BucketType, Options) ->
    case re_cluster:development_mode(Cluster) of
        true ->
            re_job_manager:add_job(
              list_buckets,
              {re_node_job,
               start_list_buckets,
               [Cluster, Node, BucketType, Options]});
        false ->
            {error, developer_mode_off}
    end.

-spec list_buckets_cache(re_cluster:re_cluster(),
                         binary(), non_neg_integer(), non_neg_integer()) ->
                          {error, term()} | ok.
list_buckets_cache(Cluster, BucketType, Start, Rows) ->
    Dir = re_file_util:ensure_data_dir(["buckets", atom_to_list(Cluster), binary_to_list(BucketType)]),
    case re_file_util:find_single_file(Dir) of
        {error, Reason} ->
            {error, Reason};
        File ->
            DirFile = filename:join([Dir, File]),
            {Total, ResultCount, _, _, Entries} = re_file_util:partial_file(DirFile, Start - 1, Rows - 1),
            [{total, Total},
             {count, ResultCount},
             {created, list_to_binary(re_file_util:timestamp_human(File))},
             {buckets, Entries}]
    end.

-spec clean_buckets_cache(re_cluster:re_cluster(), binary()) -> {error, term()} | ok.
clean_buckets_cache(Cluster, BucketType) ->
    Dir = re_file_util:ensure_data_dir(
            ["buckets", atom_to_list(Cluster), binary_to_list(BucketType)]),
    re_file_util:clean_dir(Dir).

-spec put_buckets_cache(re_cluster:re_cluster(), binary(), [binary()]) ->
                               {error, term()} | ok.
put_buckets_cache(Cluster, BucketType, Buckets) ->
    Dir = re_file_util:ensure_data_dir(["buckets", atom_to_list(Cluster), binary_to_list(BucketType)]),
    DirFile =
        case re_file_util:find_single_file(Dir) of
            {error, not_found} ->
                filename:join([Dir, re_file_util:timestamp_string()]);
            {error, Reason} ->
                {error, Reason};
            File ->
                filename:join([Dir, File])
        end,
    case DirFile of
        {error, Reason1} ->
            {error, Reason1};
        _ ->
            {ok, Device} = file:open(DirFile, [append]),
            StrBuckets = [binary_to_list(B) || B <- Buckets],
            io:fwrite(Device, string:join(StrBuckets, io_lib:nl()), []),
            file:close(Device),
            ok
    end.

-spec list_keys(re_cluster:re_cluster(), re_node(), binary(), binary(), [term()]) ->
                          {error, term()} | ok.
list_keys(Cluster, Node, BucketType, Bucket, Options) ->
    case re_cluster:development_mode(Cluster) of
        true ->
            re_job_manager:add_job(
              list_keys,
              {re_node_job,
               start_list_keys,
               [Cluster, Node, BucketType, Bucket, Options]});
        false ->
            {error, developer_mode_off}
    end.

-spec list_keys_cache(re_cluster:re_cluster(),
                         binary(), binary(), non_neg_integer(), non_neg_integer()) ->
                          {error, term()} | ok.
list_keys_cache(Cluster, BucketType, Bucket, Start, Rows) ->
    Dir = re_file_util:ensure_data_dir(["keys", atom_to_list(Cluster), binary_to_list(BucketType),
                                        binary_to_list(Bucket)]),
    case re_file_util:find_single_file(Dir) of
        {error, Reason} ->
            {error, Reason};
        File ->
            DirFile = filename:join([Dir, File]),
            {Total, ResultCount, _, _, Entries} = re_file_util:partial_file(DirFile, Start - 1, Rows - 1),
            [{total, Total},
             {count, ResultCount},
             {created, list_to_binary(re_file_util:timestamp_human(File))},
             {keys, Entries}]
    end.

-spec list_keys_ts_cache(re_cluster:re_cluster(),
                         binary(), non_neg_integer(), non_neg_integer()) ->
                          {error, term()} | ok.
list_keys_ts_cache(Cluster, Table, Start, Rows) ->
    Dir = re_file_util:ensure_data_dir(["keys_ts", atom_to_list(Cluster), binary_to_list(Table)]),
    case re_file_util:find_single_file(Dir) of
        {error, Reason} ->
            {error, Reason};
        File ->
            DirFile = filename:join([Dir, File]),
            {Total, ResultCount, _, _, Entries} = re_file_util:partial_file(DirFile, Start - 1, Rows - 1),
            [{total, Total},
             {count, ResultCount},
             {created, list_to_binary(re_file_util:timestamp_human(File))},
             {keys, Entries}]
    end.

-spec put_keys_ts_cache(re_cluster:re_cluster(), binary(), [binary()]) ->
                               {error, term()} | ok.
put_keys_ts_cache(Cluster, Table, Keys) ->
    Dir = re_file_util:ensure_data_dir(["keys_ts", atom_to_list(Cluster), binary_to_list(Table)]),
    DirFile =
        case re_file_util:find_single_file(Dir) of
            {error, not_found} ->
                filename:join([Dir, re_file_util:timestamp_string()]);
            {error, Reason} ->
                {error, Reason};
            File ->
                filename:join([Dir, File])
        end,
    case DirFile of
        {error, Reason1} ->
            {error, Reason1};
        _ ->
            {ok, Device} = file:open(DirFile, [append]),
            StrKeys = [binary_to_list(list_to_binary(mochijson2:encode(K))) || K <- Keys],
            io:fwrite(Device, string:join(StrKeys, io_lib:nl()), []),
            file:close(Device),
            ok
    end.

-spec clean_keys_ts_cache(re_cluster:re_cluster(), binary()) -> {error, term()} | ok.
clean_keys_ts_cache(Cluster, Table) ->
    Dir = re_file_util:ensure_data_dir(
            ["keys_ts", atom_to_list(Cluster), binary_to_list(Table)]),
    re_file_util:clean_dir(Dir).

-spec clean_keys_cache(re_cluster:re_cluster(), binary(), binary()) -> {error, term()} | ok.
clean_keys_cache(Cluster, BucketType, Bucket) ->
    Dir = re_file_util:ensure_data_dir(
            ["buckets", atom_to_list(Cluster), binary_to_list(BucketType),
             binary_to_list(Bucket)]),
    re_file_util:clean_dir(Dir).

-spec put_keys_cache(re_cluster:re_cluster(), binary(), binary(), [binary()]) ->
                               {error, term()} | ok.
put_keys_cache(Cluster, BucketType, Bucket, Keys) ->
    Dir = re_file_util:ensure_data_dir(["keys", atom_to_list(Cluster), binary_to_list(BucketType),
                                        binary_to_list(Bucket)]),
    DirFile =
        case re_file_util:find_single_file(Dir) of
            {error, not_found} ->
                filename:join([Dir, re_file_util:timestamp_string()]);
            {error, Reason} ->
                {error, Reason};
            File ->
                filename:join([Dir, File])
        end,
    case DirFile of
        {error, Reason1} ->
            {error, Reason1};
        _ ->
            {ok, Device} = file:open(DirFile, [append]),
            StrKeys = [binary_to_list(K) || K <- Keys],
            io:fwrite(Device, string:join(StrKeys, io_lib:nl()), []),
            file:close(Device),
            ok
    end.

-spec log_files(re_node()) -> {error, term()} | [{atom(), term()}].
log_files(Node) ->
    case command(Node, re_riak_patch, get_log_files, []) of
        {error, Reason} ->
            {error, Reason};
        Files ->
            lists:map(fun(N) -> [{id, list_to_binary(N)}] end, Files)
    end.

-spec log_file_exists(re_node(), string()) -> boolean().
log_file_exists(Node, File) ->
    case command(Node, re_riak_patch, get_log_files, []) of
        {error, _} ->
            false;
        Files ->
            lists:member(File, Files)
    end.

-spec log_file(re_node(), string(), non_neg_integer()) -> {error, term()} | [{atom(), term()}].
log_file(Node, File, NumLines) ->
    case re:run(File, ".log(.[0-9])?$") of
        {match,_} ->
            case command(Node, re_riak_patch, tail_log, [File, NumLines]) of
                {error, _} ->
                    {error, not_found};
                {Total, Lines} ->
                    [{total_lines, Total},{lines, Lines}]
            end;
        _ ->
            {error, not_found}
    end.

-spec config_files(re_node()) -> {error, term()} | [{atom(), term()}].
config_files(Node) ->
    case command(Node, re_riak_patch, get_config_files, []) of
        {error, Reason} ->
            {error, Reason};
        Files ->
            lists:map(fun(N) -> [{id, list_to_binary(N)}] end, Files)
    end.

-spec config_file_exists(re_node(), string()) -> boolean().
config_file_exists(Node, File) ->
    case command(Node, re_riak_patch, get_config_files, []) of
        {error, Reason} ->
            {error, Reason};
        Files ->
            lists:member(File, Files)
    end.

-spec config_file(re_node(), string()) -> {error, term()} | [{atom(), term()}].
config_file(Node, File) ->
    case command(Node, re_riak_patch, get_config, [File]) of
        {error, _} ->
            [{error, not_found}];
        Lines ->
            [{lines, Lines}]
    end.

-spec config(re_node()) -> {error, term()} | [{atom(), term()}].
config(Node) ->
    try
        case command(Node, re_riak_patch, effective_config, []) of
            {error, Reason} ->
                {error, Reason};
            Config ->
                Config
        end
    catch
        Exception:Reason1 ->
            Error = list_to_binary(io_lib:format("~p:~p", [Exception,Reason1])),
            lager:info("~p:~p", [Exception, Reason1]),
            lager:info("Backtrace: ~p", [erlang:get_stacktrace()]),
            {error, Error}
    end.

-spec http_listener(re_node()) -> {error, term()} | binary().
http_listener(Node) ->
    NodeStr = atom_to_list(Node),
    [_,Addr] = string:tokens(NodeStr, "@"),
    case command(Node, riak_api_web, get_listeners, []) of
        {error, Reason} ->
            {error, Reason};
        [{http,{_,Port}}|_] ->
            list_to_binary(Addr ++ ":" ++ integer_to_list(Port))
    end.

-spec pb_listener(re_node()) -> {error, term()} | binary().
pb_listener(Node) ->
    NodeStr = atom_to_list(Node),
    [_,Addr] = string:tokens(NodeStr, "@"),
    case command(Node, application, get_env, [riak_api, pb]) of
        {error, Reason} ->
            {error, Reason};
        {ok,[{_,Port}]} ->
            list_to_binary(Addr ++ ":" ++ integer_to_list(Port))
    end.

-spec bucket_type_exists(re_node(), binary()) -> boolean().
bucket_type_exists(Node, BucketType) ->
    case bucket_type(Node, BucketType) of
        {error, _} ->
            false;
        _ ->
            true
    end.

-spec bucket_type(re_node(), binary()) -> {error, term()} | [{atom(), term()}].
bucket_type(Node, BucketType) ->
    case bucket_types(Node) of
        {error, Reason} ->
            {error, Reason};
        Ts ->
            FlatProps = lists:flatten(
                          [proplists:get_value(props, Prop) ||
                              Prop <- Ts,
                              BucketType =:= proplists:get_value(id, Prop)]),
            case FlatProps of
                [] ->
                    {error, not_found};
                Props ->
                    [{id,BucketType}, {props, Props}]
            end
    end.

-spec bucket_types(re_node()) -> {error, term()} | [{atom(), term()}].
bucket_types(Node) ->
    case command(Node, re_riak_patch, bucket_types, []) of
        {error, Reason} ->
            {error, Reason};
        List ->
            List1 = [ [{id, Name},{props, Props}]
                      || [{name, Name},{props, Props}] <- List ],
            lists:sort(fun([{id, N1}|_], [{id, N2}|_]) ->
                               string:to_lower(binary_to_list(N1)) < string:to_lower(binary_to_list(N2)) end, List1)
    end.

-spec create_bucket_type(re_node(), binary(), binary()) -> {error, term()} | [{atom(), term()}].
create_bucket_type(Node, BucketType, RawValue) ->
    {Created, Active} =
        case bucket_type(Node, BucketType) of
            {error, _} -> {false, false};
            Type ->
                Props = proplists:get_value(props, Type),
                {true, proplists:get_value(active, Props, false)}
        end,

    lager:info("Creating bucket type ~p with props ~p", [BucketType, RawValue]),

    case {Created, Active} of
        {false, _} ->
            bucket_type_action(Node, BucketType, RawValue, [create, activate], []);
        {true, false} ->
            bucket_type_action(Node, BucketType, RawValue, [activate], []);
        {true, true} ->
            bucket_type_action(Node, BucketType, RawValue, [update], [])
    end.

-spec table_exists(re_node(), binary()) -> boolean().
table_exists(Node, Table) ->
    case table(Node, Table) of
        {error, _} -> false;
        _ -> true
    end.

-spec table(re_node(), binary()) -> {error, term()} | [{atom(), term()}].
table(Node, Table) ->
    case tables(Node) of
        {error, Reason} ->
            {error, Reason};
        Ts ->
            FlatProps = lists:flatten(
                          [proplists:get_value(props, Prop) ||
                              Prop <- Ts,
                              Table =:= proplists:get_value(id, Prop)]),
            case FlatProps of
                [] ->
                    {error, not_found};
                Props ->
                    [{id,Table}, {props, Props}]
            end
    end.

-spec tables(re_node()) -> {error, term()} | [{atom(), term()}].
tables(Node) ->
    case bucket_types(Node) of
        {error, Reason} ->
            {error, Reason};
        List ->
            lists:filtermap(
              fun(Type) ->
                      Props = proplists:get_value(props, Type),
                      proplists:get_value(ddl, Props) =/= undefined
              end, List)
    end.

-spec command(re_node(), module(), atom(), [term()]) -> {error, term()} | term().
command(N, M, F, A) ->
    case riak_explorer:is_riak() and is_self(N) of
        true ->
            local_command(M, F, A);
        _ ->
            remote_command(N, M, F, A)
    end.

-spec client(re_node()) -> {error, term()} | pid().
client(Node) ->
    case command(Node, application, get_env, [riak_api, pb]) of
        {error, Reason} ->
            {error, Reason};
        {ok,[{Ip,Port}]} ->
            case riakc_pb_socket:start_link(Ip, Port) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Pid} ->
                    Pid
            end
    end.

%% ProtoBuf Messages Schema Generation
%% TODO: Put this in the appropriate place
-define(PB_MESSAGES_BUCKET, <<"_basho_riak_explorer">>).
-define(PB_MESSAGES_FILES_BUCKET, <<"pb_files">>).

%% Check if bucket type exists PB_MESSAGES_BUCKET type exists
ensure_messages_bucket_type_exists(Node) ->
    case bucket_type_exists(Node, ?PB_MESSAGES_BUCKET) of
        false ->
            %% RawValue is the Props, I think, so what's appropriate here?
            case create_bucket_type(Node, ?PB_MESSAGES_BUCKET, <<"">>) of
                [{success, true}, _] -> ok;
                [Error, _]-> Error
            end;
        true -> ok
    end.

-spec pb_messages_create(re_node(), string()) -> {error, term()} | term().
pb_messages_create(Node, Messages) ->
    %% TODO: Using Node later
    lager:info("Creating messages on ~p", [Node]),
    case gpb_scan:string(Messages) of
        {ok, Tokens, _} ->
            case gpb_parse:parse(Tokens ++ [{'$end', length(Messages) + 1}]) of
                {ok, Result} ->
                    Hash = list_to_binary(mochihex:to_hex(binary_to_list(crypto:hash(sha, Messages)))),
                    case ensure_messages_bucket_type_exists(Node) of
                        ok ->
                            pb_message_store(Node, Hash, Result),
                            Hash;
                        Error ->
                            lager:info("Error creating bucket type"),
                            {error, Error}
                    end;
               {error, {LNum, _Module, EMsg}} ->
                    lager:info("Parse error on line ~w:~n  ~p~n",
                               [LNum, {Tokens, EMsg}]),
                    Error = lists:flatten(io_lib:format("Error parsing \"~s\" on line ~p. Reason: ~s.",
                                                        [Messages, LNum, lists:flatten(EMsg)])),
                    {error, Error}
            end;
        {error, Reason} ->
            lager:info("Scan error:~n  ~p~n", [Reason]),
            {error, Reason}
    end.

pb_message_store(Node, Hash, Messages) ->
    case pb_messages_exists(Node, Hash) of
        true ->
            ok;
        false ->
            ResultObj = riakc_obj:new({?PB_MESSAGES_BUCKET, ?PB_MESSAGES_FILES_BUCKET},
                                      Hash,
                                      term_to_binary(Messages)),
            riakc_pb_socket:put(client(Node), ResultObj)
    end.

-spec pb_messages_exists(re_node(), binary()) -> boolean().
pb_messages_exists(Node, Hash) ->
    case pb_messages(Node, Hash) of
        {error, _} ->
            false;
        _ ->
            true
    end.

-spec pb_messages(re_node(), binary()) -> {error, term()} | [{atom(), term()}].
pb_messages(Node, Hash) ->
    case riakc_pb_socket:get(client(Node),
                             {?PB_MESSAGES_BUCKET, ?PB_MESSAGES_FILES_BUCKET},
                             Hash) of
        {ok, Obj} ->
            Messages = binary_to_term(riakc_obj:get_value(Obj)),
            Formatted = format_messages(Messages),
            Formatted;
        {error, Error} ->
            {error, Error}
    end.

format_field(Field) ->
    format_field(Field, []).
format_field([], Accum) ->
    lists:reverse(Accum);
format_field([{type, {ref, [Ref]}}|Rest], Accum) ->
    format_field(Rest, [{type, [{ref, [Ref]}]}|Accum]);
format_field([Prop|Rest], Accum) ->
    format_field(Rest, [Prop|Accum]).

format_field_proplist(Fields) ->
    format_field_proplist(Fields, []).
format_field_proplist([], Accum) ->
    lists:reverse(Accum);
format_field_proplist([Field|Rest], Accum) ->
    Formatted = format_field(Field),
    format_field_proplist(Rest, [Formatted|Accum]).

format_messages(Messages) ->
    format_messages(Messages, [], [], []).
format_messages([], MsgsAccum, EnumAccum, MetaAccum) ->
    [{messages, lists:reverse(MsgsAccum)},
     {enums, lists:reverse(EnumAccum)},
     {metadata, lists:reverse(MetaAccum)}];
format_messages([Element|Rest], MsgsAccum, EnumAccum, MetaAccum) ->
    case Element of
        {{msg, MsgName}, Fields} ->
            FieldProps = gpb:field_records_to_proplists(Fields),
            Message = [{name, MsgName}, {fields, format_field_proplist(FieldProps)}],
            format_messages(Rest, [Message|MsgsAccum], EnumAccum, MetaAccum);
        {{enum, EnumName}, Values} ->
            Enum = [{name, EnumName}, {values, Values}],
            format_messages(Rest, MsgsAccum, [Enum|EnumAccum], MetaAccum);
        MetaData ->
            format_messages(Rest, MsgsAccum, EnumAccum, [MetaData|MetaAccum])
    end.

%%%===================================================================
%%% Private
%%%===================================================================

-spec remote_command(re_node(), module(), atom(), [term()]) -> {error, term()} | term().
remote_command(N, re_riak_patch, F, A) ->
    case ensure_patch_loaded(N) of
        {error, Reason} ->
            {error, Reason};
        {module, re_riak_patch} ->
            safe_rpc(N, re_riak_patch, F, A, 60000)
    end;
remote_command(N, M, F, A) ->
    safe_rpc(N, M, F, A, 60000).

-spec local_command(module(), atom(), [term()]) -> term().
local_command(M, F, A) ->
    erlang:apply(M, F, A).

-spec safe_rpc(re_node(), module(), atom(), [term()]) -> {error, term()} | term().
safe_rpc(N, M, F, A) ->
    safe_rpc(N, M, F, A, 10000).

-spec safe_rpc(re_node(), module(), atom(), [term()], timeout()) -> {error, term()} | term().
safe_rpc(N, M, F, A, Timeout) ->
    Result = try rpc:call(N, M, F, A, Timeout) of
                 R ->
                     R
             catch
                 'EXIT':{noproc, _NoProcDetails} ->
                     {badrpc, rpc_process_down};
                 'EXIT':{timeout, _} ->
                     {badrpc, timeout}
             end,
    case Result of
        {badrpc, Reason} -> {error, Reason};
        _ -> Result
    end.

-spec is_self(re_node()) -> boolean().
is_self(N) -> is_self(N, node()).

-spec is_self(re_node(), atom()) -> boolean().
is_self(N, N) -> true;
is_self(_, _) -> false.

-spec ensure_patch_loaded(re_node()) -> {error, term()} | {module, module()}.
ensure_patch_loaded(N) ->
    case safe_rpc(N, code, ensure_loaded, [re_riak_patch]) of
        {module, re_riak_patch} ->
            LocalVersion = re_riak_patch:version(),
            CommandVersion = safe_rpc(N, re_riak_patch, version, []),
            case LocalVersion =:= CommandVersion of
                false -> load_patch(N);
                _ -> {module, re_riak_patch}
            end;
        _ ->
            load_patch(N)
    end.

-spec load_patch(re_node()) -> {error, term()} | {module, module()}.
load_patch(N) ->
    lager:info("Loading re_riak_patch module into ~p.", [N]),
    {Mod, Bin, _} = code:get_object_code(re_riak_patch),
    safe_rpc(N, code, load_binary, [Mod, "/tmp/re_riak_patch.beam", Bin], 60000).

bucket_type_action(_Node, _BucketType, _RawValue, [], Accum) ->
    [{success, true},{actions, lists:reverse(Accum)}];
bucket_type_action(Node, BucketType, RawValue, [create|Rest], Accum) ->
    Props = case RawValue of
        <<>> -> "";
        P -> P
    end,
    case command(Node, re_riak_patch, bucket_type_create, [[BucketType, Props]]) of
        {error, Reason} -> {error, Reason};
        C -> bucket_type_action(Node, BucketType, RawValue, Rest, [{create, C}|Accum])
    end;
bucket_type_action(Node, BucketType, RawValue, [activate|Rest], Accum) ->
    case command(Node, re_riak_patch, bucket_type_activate, [[BucketType]]) of
        {error, Reason} -> {error, Reason};
        A -> bucket_type_action(Node, BucketType, RawValue, Rest, [{activate, A}|Accum])
    end;
bucket_type_action(Node, BucketType, RawValue, [update|Rest], Accum) ->
    case command(Node, re_riak_patch, bucket_type_update, [[BucketType, RawValue]]) of
        {error, Reason} -> {error, Reason};
        U -> bucket_type_action(Node, BucketType, RawValue, Rest, [{update, U}|Accum])
    end.
