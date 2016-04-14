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

-module(re_wm_explore).

-export([routes/0]).

-export([home/1,
         ping/1,
         routes/1,
         props/1,
         jobs/1]).

-export([clusters/1,
         cluster/1]).

-export([nodes/1,
         node/1,
         node_config/1,
         node_config_files/1,
         node_config_file_exists/1,
         node_config_file/1,
         node_log_files/1,
         node_log_file_exists/1,
         node_log_file/1,
         tables/1,
         table_put/1,
         tables_query/1,
         table_exists/1,
         table/1,
         table_query/1]).

-export([bucket_types/1,
         bucket_type_exists/1,
         bucket_type/1,
         bucket_type_put/1,
         bucket_type_jobs/1]).

-export([refresh_buckets/1,
         buckets/1,
         buckets_delete/1,
         buckets_put/1,
         bucket_exists/1,
         bucket/1,
         bucket_delete/1,
         bucket_jobs/1]).

-export([refresh_keys/1,
         keys/1,
         keys_delete/1,
         keys_put/1]).

-define(BASE, "explore").
-define(EXPLORE_BASE, [?BASE]).

-define(CLUSTER_BASE, [?BASE, "clusters", cluster]).
-define(CLUSTER_NODE, ?CLUSTER_BASE ++ ["nodes", node]).
-define(BASE_NODE, [?BASE, "nodes", node]).
-define(NODE_BASE, [?CLUSTER_BASE, ?CLUSTER_NODE, ?BASE_NODE]).

-define(CLUSTER_TYPE, ?CLUSTER_BASE ++ ["bucket_types", bucket_type]).
-define(CLUSTER_NODE_TYPE, ?CLUSTER_BASE ++ ["nodes", node, "bucket_types", bucket_type]).
-define(BASE_TYPE, [?BASE, "nodes", node, "bucket_types", bucket_type]).
-define(BUCKET_TYPE_BASE, [?CLUSTER_TYPE, ?CLUSTER_NODE_TYPE, ?BASE_TYPE]).

-define(CLUSTER_BUCKET, ?CLUSTER_TYPE ++ ["buckets", bucket]).
-define(CLUSTER_NODE_BUCKET, ?CLUSTER_NODE_TYPE ++ ["buckets", bucket]).
-define(BASE_BUCKET, ?BASE_TYPE ++ ["buckets", bucket]).
-define(BUCKET_BASE, [?CLUSTER_BUCKET, ?CLUSTER_NODE_BUCKET, ?BASE_BUCKET]).

-include_lib("webmachine/include/webmachine.hrl").
-include("re_wm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec routes() -> [route()].
routes() ->
    [%% Base
     #route{base=[], path=[?EXPLORE_BASE], content={?MODULE,home}},
     #route{base=[?EXPLORE_BASE], path=[["ping"]], content={?MODULE,ping}},
     #route{base=[?EXPLORE_BASE], path=[["routes"]], content={?MODULE,routes}},
     #route{base=[?EXPLORE_BASE], path=[["props"]], content={?MODULE,props}},
     #route{base=[?EXPLORE_BASE], path=[["jobs"]], content={?MODULE,jobs}},
     %% Cluster
     #route{base=[?EXPLORE_BASE], 
            path=[["clusters"]],
            content={?MODULE,clusters}},
     #route{base=[?EXPLORE_BASE], 
            path=[["clusters",cluster]], 
            exists={re_wm,rd_cluster_exists},
            content={?MODULE,cluster}},
     %% Node
     #route{base=[?EXPLORE_BASE,?CLUSTER_BASE],
            path=[["nodes"]], 
            exists={re_wm,rd_cluster_exists},
            content={?MODULE,nodes}},
     #route{base=?NODE_BASE,
            path=[], 
            exists={re_wm,rd_node_exists},
            content={?MODULE,node}},
     #route{base=?NODE_BASE,
            path=[["config"]], 
            exists={re_wm,rd_node_exists},
            content={?MODULE,node_config}},
     #route{base=?NODE_BASE,
            path=[["config", "files"]], 
            exists={re_wm,rd_node_exists},
            content={?MODULE,node_config_files}},
     #route{base=?NODE_BASE,
            path=[["config", "files", file]],
            provides=?PROVIDE_TEXT ++ [?PROVIDE(?JSON_TYPE)],
            exists={?MODULE,node_config_file_exists},
            content={?MODULE,node_config_file}},
     #route{base=?NODE_BASE,
            path=[["log", "files"]], 
            exists={re_wm,rd_node_exists},
            content={?MODULE,node_log_files}},
     #route{base=?NODE_BASE,
            path=[["log", "files", file]], 
            provides=?PROVIDE_TEXT ++ [?PROVIDE(?JSON_TYPE)],
            exists={?MODULE,node_log_file_exists},
            content={?MODULE,node_log_file}},
     #route{base=?NODE_BASE,
            path=[["tables"]], 
            exists={re_wm,rd_node_exists},
            content={?MODULE,tables}},
     #route{base=?NODE_BASE,
            path=[["tables", "query"]], 
            methods=['POST'],
            exists={re_wm,rd_node_exists},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,tables_query}},
     #route{base=?NODE_BASE,
            path=[["tables", table]], 
            methods=['PUT','GET'],
            exists={?MODULE,table_exists},
            content={?MODULE,table},
            accepts = ?ACCEPT_TEXT,
            accept = {?MODULE, table_put}},
     #route{base=?NODE_BASE,
            path=[["tables", table, "query"]], 
            methods=['POST'],
            exists={?MODULE,table_exists},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,table_query}},
     %% Bucket Type
     #route{base=?NODE_BASE,
            path=[["bucket_types"]], 
            exists={re_wm,rd_node_exists},
            content={?MODULE,bucket_types}},
     #route{base=?BUCKET_TYPE_BASE,
            path=[], 
            methods=['PUT','GET'],
            exists={?MODULE,bucket_type_exists},
            content={?MODULE,bucket_type},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,bucket_type_put}},
     #route{base=?BUCKET_TYPE_BASE,
            path=[["jobs"]], 
            exists={?MODULE,bucket_type_exists},
            content={?MODULE,bucket_type_jobs}},
     %% Bucket
     #route{base=?BUCKET_TYPE_BASE,
            path=[["refresh_buckets", "source", "riak_kv"]],
            methods=['POST'],
            exists={?MODULE,bucket_type_exists},
            accept={?MODULE,refresh_buckets}},
     #route{base=?BUCKET_TYPE_BASE,
            path=[["buckets"]],
            provides=?PROVIDE_TEXT ++ [?PROVIDE(?JSON_TYPE)],
            methods=['PUT','GET','DELETE'],
            exists={?MODULE,bucket_type_exists},
            content={?MODULE,buckets},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,buckets_put},
            delete={?MODULE,buckets_delete}},
     #route{base=?BUCKET_TYPE_BASE,
            path=[["buckets",bucket]],
            methods=['GET', 'DELETE'],
            exists={?MODULE,bucket_exists},
            content={?MODULE,bucket},
            delete={?MODULE,bucket_delete}},
     #route{base=?BUCKET_TYPE_BASE,
            path=[["buckets",bucket,"jobs"]], 
            exists={?MODULE,bucket_exists},
            content={?MODULE,bucket_jobs}},
     %% Key
     #route{base=?BUCKET_BASE,
            path=[["refresh_keys", "source", "riak_kv"]],
            methods=['POST'],
            exists={?MODULE,bucket_exists},
            accept={?MODULE,refresh_keys}},
     #route{base=?BUCKET_BASE,
            path=[["keys"]],
            provides=?PROVIDE_TEXT ++ [?PROVIDE(?JSON_TYPE)],
            methods=['PUT','GET','DELETE'],
            exists={?MODULE,bucket_exists},
            content={?MODULE,keys},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,keys_put},
            delete={?MODULE,keys_delete}}
    ].

%%%===================================================================
%%% Callbacks
%%%===================================================================

%%% Explore

-spec home(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
home(ReqData) ->
    re_wm:rd_content(<<"riak_explorer api">>, ReqData).

-spec ping(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
ping(ReqData) ->
    re_wm:rd_content(pong, ReqData).

-spec routes(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
routes(ReqData) ->
    Formatted = 
        [  [{methods, Methods},
            {base, [  [ case is_atom(Part) of
                            true -> Part;
                            false -> list_to_binary(Part) 
                        end|| Part <- Base]
                      || Base <- Bases]},
            {path, [  [ case is_atom(Part) of
                            true -> Part;
                            false -> list_to_binary(Part) 
                        end|| Part <- Path]
                      || Path <- Paths]}]
           || #route{base=Bases,path=Paths,methods=Methods} <- re_wm:routes()],
    re_wm:rd_content(Formatted, ReqData).

-spec props(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
props(ReqData) ->
    re_wm:rd_content(riak_explorer:props(), ReqData).

-spec jobs(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
jobs(ReqData) ->
    Jobs = case re_job_manager:get_jobs() of
        {error, not_found} -> [];
        J -> [J]
    end,
    re_wm:rd_content(Jobs, ReqData).

%%% Cluster

-spec clusters(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
clusters(ReqData) ->
    re_wm:rd_content(riak_explorer:clusters(), ReqData).

-spec cluster(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
cluster(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    re_wm:rd_content(re_cluster:props(C), ReqData).

%%% Node

-spec nodes(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
nodes(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    re_wm:rd_content(re_cluster:nodes(C), ReqData).

-spec node(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
node(ReqData) ->
    N = re_wm:rd_node(ReqData),
    re_wm:rd_content(re_node:props(N), ReqData).

-spec node_config(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
node_config(ReqData) ->
    N = re_wm:rd_node(ReqData),
    re_wm:rd_content(re_node:config(N), ReqData).

-spec node_config_files(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
node_config_files(ReqData) ->
    N = re_wm:rd_node(ReqData),
    re_wm:rd_content(re_node:config_files(N), ReqData).

-spec node_config_file_exists(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
node_config_file_exists(ReqData) ->
    N = re_wm:rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    case re_wm:rd_node_exists(ReqData) of
        {true,_} ->
            {re_node:config_file_exists(N, F), ReqData};
        _ ->
            {false, ReqData}
    end.

-spec node_config_file(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
node_config_file(ReqData) ->
    N = re_wm:rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    Result = re_node:config_file(N, F),
    re_wm:rd_maybe_text(Result, ReqData).

-spec node_log_files(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
node_log_files(ReqData) ->
    N = re_wm:rd_node(ReqData),
    re_wm:rd_content(re_node:log_files(N), ReqData).

-spec node_log_file_exists(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
node_log_file_exists(ReqData) ->
    N = re_wm:rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    case re_wm:rd_node_exists(ReqData) of
        {true,_} ->
            {re_node:log_file_exists(N, F), ReqData};
        _ ->
            {false, ReqData}
    end.

-spec node_log_file(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
node_log_file(ReqData) ->
    Rows = list_to_integer(wrq:get_qs_value("rows","1000",ReqData)),
    N = re_wm:rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    Result = re_node:log_file(N, F, Rows),
    re_wm:rd_maybe_text(Result, ReqData).

-spec tables(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
tables(ReqData) ->
    N = re_wm:rd_node(ReqData),
    re_wm:rd_content(re_node:tables(N), ReqData).

-spec tables_query(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
tables_query(ReqData) ->
    N = re_wm:rd_node(ReqData),
    Query = wrq:req_body(ReqData),
    Response = re_node:query_ts(N, Query),
    re_wm:add_content(Response, ReqData).

-spec table_exists(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.    
table_exists(ReqData) ->
    N = re_wm:rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    case re_wm:rd_node_exists(ReqData) of
        {true,_} ->
            {re_node:table_exists(N, Table), ReqData};
        _ ->
            {false, ReqData}
    end.

-spec table(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
table(ReqData) ->
    N = re_wm:rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    re_wm:rd_content(re_node:table(N, Table), ReqData).

-spec table_put(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
table_put(ReqData) ->
    N = re_wm:rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    RawRows = wrq:req_body(ReqData),
    Rows = mochijson2:decode(RawRows),
    case re_node:put_ts(N, Table, Rows) of
        [{ts, [{success, true}]}] ->
            {true, ReqData};
        _ ->
            {false, ReqData}
    end.

-spec table_query(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
table_query(ReqData) ->
    N = re_wm:rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    QueryRaw = wrq:req_body(ReqData),
    Query = mochijson2:decode(QueryRaw),
    Response = re_node:get_ts(N, Table, Query),
    re_wm:add_content(Response, ReqData).

%%% Bucket Type

-spec bucket_types(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
bucket_types(ReqData) ->
    N = re_wm:rd_node(ReqData),
    re_wm:rd_content(re_node:bucket_types(N), ReqData).

-spec bucket_type_exists(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
bucket_type_exists(ReqData) ->
    N = re_wm:rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    case re_wm:rd_node_exists(ReqData) of
        {true,_} ->
            {re_node:bucket_type_exists(N, T), ReqData};
        _ ->
            {false, ReqData}
    end.

-spec bucket_type(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.    
bucket_type(ReqData) ->
    N = re_wm:rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    re_wm:rd_content(re_node:bucket_type(N, T), ReqData).

-spec bucket_type_put(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
bucket_type_put(ReqData) ->
    N = re_wm:rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    RawValue = wrq:req_body(ReqData),
    case re_node:create_bucket_type(N, T, RawValue) of
        {error, Reason} ->
            {false, re_wm:add_error(Reason, ReqData)};
        Response ->
            re_wm:add_content(Response, ReqData)
    end.

-spec bucket_type_jobs(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
bucket_type_jobs(ReqData) ->
    Jobs = case re_job_manager:get_job(list_buckets) of
        {error, not_found} -> [];
        J -> [J]
    end,
    re_wm:rd_content(Jobs, ReqData).

%%% Bucket

-spec refresh_buckets(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
refresh_buckets(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    N = re_wm:rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    Sort = list_to_atom(wrq:get_qs_value("sort","true",ReqData)),
    Options = [{sort, Sort}],
    JobsPath = string:substr(wrq:path(ReqData),1,
                             string:str(wrq:path(ReqData), 
                                        "refresh_buckets") - 1) ++ "jobs",
    JobResponse = re_node:list_buckets(C, N, T, Options),
    set_jobs_response(JobResponse, JobsPath, ReqData).

-spec buckets(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
buckets(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    Start = list_to_integer(wrq:get_qs_value("start","0",ReqData)),
    Rows = list_to_integer(wrq:get_qs_value("rows","1000",ReqData)),
    Result = re_node:list_buckets_cache(C, T, Start, Rows),
    re_wm:rd_maybe_text(buckets, Result, ReqData).

-spec buckets_delete(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
buckets_delete(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    case re_node:clean_buckets_cache(C, T) of
        ok ->
            {true, ReqData};
        {error, Reason} ->
            {false, 
             re_wm:add_error(Reason, ReqData)}
    end.

-spec buckets_put(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
buckets_put(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    RawValue = wrq:req_body(ReqData),
    Buckets = case wrq:get_req_header("Content-Type", ReqData) of
                  "application/json" ->
                      {struct, [{<<"buckets">>, B}]} = mochijson2:decode(RawValue),
                      B;
                  _ ->
                      BucketsStr = string:tokens(RawValue, "\n"),
                      lists:map(fun(B) -> list_to_binary(B) end, BucketsStr)
              end,
    case re_node:put_buckets_cache(C, T, Buckets) of
        ok ->
            {true, ReqData};
        {error, Reason} ->
            {false, 
             re_wm:add_error(Reason, ReqData)}
    end.

-spec bucket_exists(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
bucket_exists(ReqData) ->
    bucket_type_exists(ReqData).

-spec bucket(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
bucket(ReqData) ->
    B = list_to_binary(wrq:path_info(bucket, ReqData)),
    re_wm:rd_content(
      [{id,B}, {props, []}], ReqData).

-spec bucket_delete(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
bucket_delete(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    N = re_wm:rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    B = list_to_binary(wrq:path_info(bucket, ReqData)),
    JobsPath = wrq:path(ReqData) ++ "/jobs",
    JobResponse = re_node:delete_bucket(C, N, T, B),
    set_jobs_response(JobResponse, JobsPath, ReqData).

-spec bucket_jobs(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
bucket_jobs(ReqData) ->
    KeysJobs = case re_job_manager:get_job(list_keys) of
        {error, not_found} -> [];
        KJ -> [KJ]
    end,
    DeleteBucketJobs = case re_job_manager:get_job(delete_bucket) of
        {error, not_found} -> [];
        DJ -> [DJ]
    end,
    re_wm:rd_content(
      KeysJobs ++ DeleteBucketJobs, ReqData).

-spec refresh_keys(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
refresh_keys(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    N = re_wm:rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    B = list_to_binary(wrq:path_info(bucket, ReqData)),
    Sort = list_to_atom(wrq:get_qs_value("sort","true",ReqData)),
    Options = [{sort, Sort}],
    JobsPath = string:substr(wrq:path(ReqData),1,
                             string:str(wrq:path(ReqData), 
                                        "refresh_keys") - 1) ++ "jobs",
    JobResponse = re_node:list_keys(C, N, T, B, Options),
    set_jobs_response(JobResponse, JobsPath, ReqData).

-spec keys(#wm_reqdata{}) -> {term(), #wm_reqdata{}}.
keys(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    B = list_to_binary(wrq:path_info(bucket, ReqData)),
    Start = list_to_integer(wrq:get_qs_value("start","0",ReqData)),
    Rows = list_to_integer(wrq:get_qs_value("rows","1000",ReqData)),
    Result = re_node:list_keys_cache(C, T, B, Start, Rows),
    re_wm:rd_maybe_text(keys, Result, ReqData).

-spec keys_delete(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
keys_delete(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    B = list_to_binary(wrq:path_info(bucket, ReqData)),
    case re_node:clean_keys_cache(C, T, B) of
        ok ->
            {true, ReqData};
        {error, Reason} ->
            {false, 
             re_wm:add_error(Reason, ReqData)}
    end.

-spec keys_put(#wm_reqdata{}) -> {boolean(), #wm_reqdata{}}.
keys_put(ReqData) ->
    C = re_wm:rd_cluster(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    B = list_to_binary(wrq:path_info(bucket, ReqData)),
    RawValue = wrq:req_body(ReqData),
    Keys = case wrq:get_req_header("Content-Type", ReqData) of
               "application/json" ->
                   {struct, [{<<"keys">>, K}]} = mochijson2:decode(RawValue),
                   K;
               _ ->
                   KeysStr = string:tokens(RawValue, "\n"),
                   lists:map(fun(K) -> list_to_binary(K) end, KeysStr)
           end,
    case re_node:put_keys_cache(C, T, B, Keys) of
        ok ->
            {true, ReqData};
        {error, Reason} ->
            {false, 
             re_wm:add_error(Reason, ReqData)}
    end.

%% ====================================================================
%% Private
%% ====================================================================

-spec set_jobs_response(term(), string(), #wm_reqdata{}) -> 
                               {{halt, 202|102|403}, #wm_reqdata{}}.
set_jobs_response(ok, JobsPath, ReqData) ->
    ReqData1 = wrq:set_resp_headers([{"Location",JobsPath}], ReqData),
    {{halt, 202}, ReqData1};
set_jobs_response({error, already_started}, JobsPath, ReqData) ->
    ReqData1 = wrq:set_resp_headers([{"Location",JobsPath}], ReqData),
    {{halt, 102}, ReqData1};
set_jobs_response({error, developer_mode_off}, _, ReqData) ->
    {{halt, 403}, ReqData};
set_jobs_response({error, Reason}, _, ReqData) ->
    {false, re_wm:add_error(Reason, ReqData)}.
