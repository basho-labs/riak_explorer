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

-export([ping/1,
         routes/1,
         props/1,
         jobs/1]).

-export([clusters/1,
         cluster_exists/1,
         cluster/1]).

-export([nodes/1,
         node_exists/1,
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

-define(BASE, "explore").
-define(EXPLORE_BASE, [?BASE]).
-define(CLUSTER_BASE, [?BASE, "clusters", cluster]).
-define(NODE_BASE, [?CLUSTER_BASE,
                    ?CLUSTER_BASE ++ ["nodes", node], 
                    [?BASE, "nodes", node]]).
-define(BUCKET_TYPE_BASE, [?CLUSTER_BASE ++ ["bucket_types", bucket_type],
                    ?CLUSTER_BASE ++ ["nodes", node, "bucket_types", bucket_type], 
                    [?BASE, "nodes", node, "bucket_types", bucket_type]]).

-include_lib("webmachine/include/webmachine.hrl").
-include("re_wm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec routes() -> [re_wm_resource:route()].
routes() ->
    [%% Base
     #route{base=[?EXPLORE_BASE], path=["ping"], content={?MODULE,ping}},
     #route{base=[?EXPLORE_BASE], path=["routes"], content={?MODULE,routes}},
     #route{base=[?EXPLORE_BASE], path=["props"], content={?MODULE,props}},
     #route{base=[?EXPLORE_BASE], path=["jobs"], content={?MODULE,jobs}},
     %% Cluster
     #route{base=[?EXPLORE_BASE], 
            path=["clusters"],
            content={?MODULE,clusters}},
     #route{base=[?EXPLORE_BASE], 
            path=["clusters",cluster], 
            exists={?MODULE,cluster_exists},
            content={?MODULE,cluster}},
     %% Node
     #route{base=[?EXPLORE_BASE,?CLUSTER_BASE],
            path=["nodes"], 
            exists={?MODULE,cluster_exists},
            content={?MODULE,nodes}},
     #route{base=?NODE_BASE,
            path=[], 
            exists={?MODULE,node_exists},
            content={?MODULE,node}},
     #route{base=?NODE_BASE,
            path=["config"], 
            exists={?MODULE,node_exists},
            content={?MODULE,node_config}},
     #route{base=?NODE_BASE,
            path=["config", "files"], 
            exists={?MODULE,node_exists},
            content={?MODULE,node_config_files}},
     #route{base=?NODE_BASE,
            path=["config", "files", file], 
            exists={?MODULE,node_config_file_exists},
            content={?MODULE,node_config_file}},
     #route{base=?NODE_BASE,
            path=["log", "files"], 
            exists={?MODULE,node_exists},
            content={?MODULE,node_log_files}},
     #route{base=?NODE_BASE,
            path=["log", "files", file], 
            exists={?MODULE,node_log_file_exists},
            content={?MODULE,node_log_file}},
     #route{base=?NODE_BASE,
            path=["tables"], 
            exists={?MODULE,node_exists},
            content={?MODULE,tables}},
     #route{base=?NODE_BASE,
            path=["tables", "query"], 
            methods=['POST'],
            exists={?MODULE,node_exists},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,tables_query}},
     #route{base=?NODE_BASE,
            path=["tables", table], 
            methods=['PUT','GET'],
            exists={?MODULE,table_exists},
            content={?MODULE,table},
            accepts = ?ACCEPT_TEXT,
            accept = {?MODULE, table_put}},
     #route{base=?NODE_BASE,
            path=["tables", table, "query"], 
            methods=['POST'],
            exists={?MODULE,table_exists},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,table_query}},
     %% Bucket Type
     #route{base=?NODE_BASE,
            path=["bucket_types"], 
            exists={?MODULE,node_exists},
            content={?MODULE,bucket_types}},
     #route{base=?BUCKET_TYPE_BASE,
            path=[], 
            methods=['PUT','GET'],
            exists={?MODULE,bucket_type_exists},
            content={?MODULE,bucket_type},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,bucket_type_put}},
     #route{base=?BUCKET_TYPE_BASE,
            path=["jobs"], 
            exists={?MODULE,bucket_type_exists},
            content={?MODULE,bucket_type_jobs}},
     %% Bucket
     #route{base=?BUCKET_TYPE_BASE,
            path=["refresh_buckets", "source", "riak_kv"],
            methods=['POST'],
            exists={?MODULE,bucket_type_exists},
            content={?MODULE,refresh_buckets}},
     #route{base=?BUCKET_TYPE_BASE,
            path=["buckets"],
            methods=['PUT','GET','DELETE'],
            exists={?MODULE,bucket_type_exists},
            content={?MODULE,buckets},
            accepts=?ACCEPT_TEXT,
            accept={?MODULE,buckets_put},
            delete={?MODULE,buckets_delete}},
     #route{base=?BUCKET_TYPE_BASE,
            path=["buckets",bucket],
            methods=['GET', 'DELETE'],
            exists={?MODULE,bucket_exists},
            content={?MODULE,bucket},
            delete={?MODULE,bucket_delete}},
     #route{base=?BUCKET_TYPE_BASE,
            path=["buckets",bucket,"jobs"], 
            exists={?MODULE,bucket_exists},
            content={?MODULE,bucket_jobs}}
    ].

%%%===================================================================
%%% Callbacks
%%%===================================================================

%%% Explore

ping(ReqData) ->
    {[{ping, pong}], ReqData}.

routes(ReqData) ->
    {[{routes, re_config:formatted_routes()}], ReqData}.

props(ReqData) ->
    {[{props, re_config:props()}], ReqData}.

jobs(ReqData) ->
    {[{jobs, re_job_manager:get_jobs()}], ReqData}.

%%% Cluster

clusters(ReqData) ->
    {[{clusters, re_riak:clusters()}], ReqData}.

cluster_exists(ReqData) ->
    C = rd_cluster(ReqData),
    {re_config:cluster_exists(C), ReqData}.

cluster(ReqData) ->
    C = rd_cluster(ReqData),
    {re_riak:cluster(C), ReqData}.

%%% Node

nodes(ReqData) ->
    C = rd_cluster(ReqData),
    {re_riak:nodes(C), ReqData}.

node_exists(ReqData) ->
    C = rd_cluster(ReqData),
    N = rd_node(ReqData),
    case cluster_exists(ReqData) of
        {true,_} ->
            {re_riak:node_exists(C, N), ReqData};
        _ ->
            {false, ReqData}
    end.

node(ReqData) ->
    N = rd_node(ReqData),
    {re_riak:node_info(N), ReqData}.

node_config(ReqData) ->
    N = rd_node(ReqData),
    {re_riak:node_config(N), ReqData}.

node_config_files(ReqData) ->
    N = rd_node(ReqData),
    {re_riak:config_files(N), ReqData}.

node_config_file_exists(ReqData) ->
    N = rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    case node_exists(ReqData) of
        {true,_} ->
            {re_riak:config_file_exists(N, F), ReqData};
        _ ->
            {false, ReqData}
    end.

node_config_file(ReqData) ->
    N = rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    {re_riak:config_file(N, F), ReqData}.

node_log_files(ReqData) ->
    N = rd_node(ReqData),
    {re_riak:log_files(N), ReqData}.

node_log_file_exists(ReqData) ->
    N = rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    case node_exists(ReqData) of
        {true,_} ->
            {re_riak:log_file_exists(N, F), ReqData};
        _ ->
            {false, ReqData}
    end.

node_log_file(ReqData) ->
    Rows = list_to_integer(wrq:get_qs_value("rows","1000",ReqData)),
    N = rd_node(ReqData),
    F = wrq:path_info(file, ReqData),
    {re_riak:log_file(N, F, Rows), ReqData}.

tables(ReqData) ->
    N = rd_node(ReqData),
    {re_riak:tables(N), ReqData}.

tables_query(ReqData) ->
    N = rd_node(ReqData),
    Query = wrq:req_body(ReqData),
    Response = re_riak:query_ts(N, Query),
    {true, wrq:append_to_response_body(mochijson2:encode(Response), ReqData)}.
    
table_exists(ReqData) ->
    N = rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    case node_exists(ReqData) of
        {true,_} ->
            {re_riak:table_exists(N, Table), ReqData};
        _ ->
            {false, ReqData}
    end.

table(ReqData) ->
    N = rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    {re_riak:table(N, Table), ReqData}.

table_put(ReqData) ->
    N = rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    RawRows = wrq:req_body(ReqData),
    Rows = mochijson2:decode(RawRows),
    case re_riak:put_ts(N, Table, Rows) of
        [{ts, [{success, true}]}] ->
            {true, ReqData};
        _ ->
            {false, ReqData}
    end.

table_query(ReqData) ->
    N = rd_node(ReqData),
    Table = list_to_binary(wrq:path_info(table, ReqData)),
    QueryRaw = wrq:req_body(ReqData),
    Query = mochijson2:decode(QueryRaw),
    Response = re_riak:get_ts(N, Table, Query),
    {true, wrq:append_to_response_body(mochijson2:encode(Response), ReqData)}.

%%% Bucket Type

bucket_types(ReqData) ->
    N = rd_node(ReqData),
    {re_riak:bucket_types(N), ReqData}.

bucket_type_exists(ReqData) ->
    N = rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    case node_exists(ReqData) of
        {true,_} ->
            {re_riak:bucket_type_exists(N, T), ReqData};
        _ ->
            {false, ReqData}
    end.
    
bucket_type(ReqData) ->
    N = rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    {re_riak:bucket_type(N, T), ReqData}.
    
bucket_type_put(ReqData) ->
    N = rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    RawValue = wrq:req_body(RD),

    case re_riak:create_bucket_type(N, T, RawValue) of
        [{error, _, Message}] ->
            {false, wrq:append_to_response_body(mochijson2:encode(Message), ReqData)};
        Response ->
            {true, wrq:append_to_response_body(mochijson2:encode(Response), ReqData)}
    end.

bucket_type_jobs(_ReqData) ->
    Jobs = case re_job_manager:get(buckets) of
        [{error, not_found}] -> [];
        J -> [J]
    end,
    {[{jobs, Jobs}], ReqData}.

%%% Bucket

refresh_buckets(ReqData) ->
    C = rd_cluster(ReqData),
    N = rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    Sort = list_to_atom(wrq:get_qs_value("sort","true",ReqData)),
    Options = [{sort, Sort}],
    JobsPath = string:substr(wrq:path(ReqData),1,
                             string:str(wrq:path(ReqData), 
                                        "refresh_buckets") - 1) ++ "jobs",
    JobResponse = re_riak:list_buckets(C, N, T, Options),
    set_jobs_response(JobResponse, JobsPath, ReqData).

buckets(ReqData) ->
    C = rd_cluster(ReqData),
    N = rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    Start = list_to_integer(wrq:get_qs_value("start","0",ReqData)),
    Rows = list_to_integer(wrq:get_qs_value("rows","1000",ReqData)),
    {re_riak:list_buckets_cache(C, N, T, Start, Rows), ReqData}.

buckets_delete(ReqData) ->
    C = rd_cluster(ReqData),
    N = rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    JobsPath = wrq:path(ReqData) ++ "/jobs",
    JobResponse = re_riak:clean_buckets(C, N, T),
    set_jobs_response(JobResponse, JobsPath, ReqData).

buckets_put(ReqData) ->
    C = rd_cluster(ReqData),
    N = rd_node(ReqData),
    T = list_to_binary(wrq:path_info(bucket_type, ReqData)),
    RawValue = wrq:req_body(ReqData),
    Buckets = case wrq:get_req_header("Content-Type", ReqData) of
                  "application/json" ->
                      {struct, [{<<"buckets">>, B}]} = mochijson2:decode(RawValue),
                      B;
                  "plain/text" ->
                      BucketsStr = string:tokens(RawValue, "\n"),
                      lists:map(fun(B) -> list_to_binary(B) end, BucketsStr)
              end,
    case re_riak:put_buckets(C, N, T, Buckets) of
        ok ->
            {true, ReqData};
        _ ->
            {false, ReqData}
    end.
    
bucket_exists(ReqData) ->
    Id = list_to_binary(Bucket),
    Response = [{buckets, [{id,Id}, {props, []}]}],

bucket(ReqData) ->
    Id = list_to_binary(Bucket),
    Response = [{buckets, [{id,Id}, {props, []}]}],

bucket_delete(ReqData) ->
    re_riak:delete_bucket(Cluster, Node, BucketType, Bucket));

bucket_jobs(_ReqData) ->
    %%TODO need to add filter criteria potentially
    KeysJobs = case re_job_manager:get(keys) of
        [{error, not_found}] -> [];
        KJ -> [KJ]
    end,
    DeleteBucketJobs = case re_job_manager:get(delete_bucket) of
        [{error, not_found}] -> [];
        DJ -> [DJ]
    end,
    [{jobs, KeysJobs ++ DeleteBucketJobs}].

%% ====================================================================
%% Private
%% ====================================================================

rd_cluster(ReqData) ->
    re_wm_util:maybe_atomize(wrq:path_info(cluster, ReqData)).

rd_node(ReqData) ->
    C = rd_cluster(ReqData),
    re_wm_util:node_from_context(C, wrq:path_info(node, ReqData)).

set_jobs_response(ok, JobsPath, ReqData) ->
    ReqData1 = wrq:set_resp_headers([{"Location",JobsPath}], ReqData),
    {{halt, 202}, ReqData1};
set_jobs_response([{error, already_started}], JobsPath, ReqData) ->
    ReqData1 = wrq:set_resp_headers([{"Location",JobsPath}], ReqData),
    {{halt, 102}, ReqData1};
set_jobs_response({error, developer_mode_off}, _, ReqData) ->
    {{halt, 403}, ReqData}.
