%% @doc Test the API in various ways.
-module(re_wm_explore_test).

-compile(export_all).
-ifdef(integration_test).
-include_lib("eunit/include/eunit.hrl").
-include("re_wm.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
re_wm_explore_test_() ->
    {setup,
     %% Setup
     fun () ->
             {RiakType, _} = riak_type(),
             if RiakType == ts ->
                     ?debugFmt("On TS, so creating bucket", []),
                     {ok, _, _} = ret:http(put,
                                           "http://localhost:9000/explore/clusters/default/bucket_types/GeoCheckin",
                                           <<"{\"props\":{\"table_def\": \"CREATE TABLE GeoCheckin (myfamily varchar not null, myseries varchar not null, time timestamp not null, myint sint64 not null, mytext varchar not null, myfloat double not null, mybool boolean not null, PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), myfamily, myseries, time))\"}}">>);
                true -> ?debugFmt("On KV, skipping bucket creation", [])
             end,
             {ok, Code, _} = ret:http(get, "http://localhost:9000/explore/clusters/default/pb-messages/messages/4200c1fb013c0db5a5722d3c791ec43a1d997640"),
             case Code of
                 "404" -> false;
                 _ -> true
             end
     end,
     %% No cleanup
     fun (_) -> ok end,
     %% Tests
     fun (State) ->
             {timeout, 60, [
                            all_routes(State)
                           ]}
     end}.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

all_routes(State) ->
    Routes = re_wm:routes(),
    RiakType = riak_type(),
    lists:flatten([ [ assert_paths(State, Method, Base, Paths, RiakType, [])
        || Method <- Methods ]
      || #route{base=[Base|_],path=Paths,methods=Methods} <- Routes ]).

assert_paths(_, _, _, [], _, Accum) -> lists:reverse(Accum);
assert_paths(State, Method, Base, [Path|Paths], RiakType, Accum) ->
    case is_testable_path(Base, Path, RiakType) of
        true ->
            Url = ret:url(to_path_str(Base) ++ "/" ++ to_path_str(Path)),
            Body = path_body(Method, Path),
            Headers = path_headers(Method, Path),
            {ok, Code, Content} = ret:http(Method, Url, Body, Headers),
            ExpectedCode = path_code(State, Method, Path),
            assert_paths(State, Method, Base, Paths, RiakType, [?_assertEqual({ExpectedCode, Method, Url, Content}, {Code, Method, Url, Content})|Accum]);
        false ->
            assert_paths(State, Method, Base, Paths, RiakType, Accum)
    end.


to_path_str(["config", "files", file]) ->
    string:join(["config", "files", "riak.conf"], "/");
to_path_str(["log", "files", file]) ->
    string:join(["log", "files", "console.log"], "/");
to_path_str(Path) ->
    string:join([ path_part(P, Path) || P <- Path ], "/").

path_part(P, _) when is_list(P) -> P;
path_part(cluster, _) -> "default";
path_part(node, _) -> "riak@127.0.0.1";
path_part(bucket_type, _) -> "mytype";
path_part(bucket, _) -> "test";
path_part(table, _) -> "GeoCheckin";
path_part(arg1, _) -> "riak@127.0.0.1";
path_part(arg2, _) -> "riak@127.0.0.1";
path_part('*',["clusters",cluster,'*']) -> "ping";
path_part('*',["nodes",node,'*']) -> "ping";
path_part(pb_file,["messages",pb_file]) -> "4200c1fb013c0db5a5722d3c791ec43a1d997640";
path_part('*',['*']) -> "index.html".

path_body('PUT', ["keys"]) ->
    <<"{\"keys\":[\"test\"]}">>;
path_body('PUT', ["tables", table, "keys"]) ->
    <<"{\"keys\":[[\"family1\", \"series1\", 25]]}">>;
path_body('PUT', ["buckets"]) ->
    <<"{\"buckets\":[\"test\"]}">>;
path_body('PUT', ["bucket_types", bucket_type]) ->
    <<"{\"props\":{\"n_val\":3}}">>;
path_body('PUT', ["tables", table]) ->
   <<"[[\"family1\", \"series1\", 25, \"hot\", 23]]">>;
path_body('POST', ["tables", "query"]) ->
    <<"select * from GeoCheckin where time > 24 and time < 26 and myfamily = 'family1' and myseries = 'series1'">>;
path_body('POST', ["tables", table, "query"]) ->
   <<"[\"family2\", \"series99\", 26]">>;
path_body('POST', ["create"]) ->
    <<"------------------------RiakExplorerTest\r\nContent-Disposition: form-data; name=\"file\"; filename=\"test.proto\"\r\nContent-Type: application/octet-stream\r\n\r\nmessage m1 {\n  repeated uint32 i   = 1;\n  required bool   b   = 2;\n  required eee    e   = 3;\n  required submsg sub = 4;\n}\nmessage submsg {\n  required string s = 1;\n  required bytes  b = 2;\n}\nenum eee {\n  INACTIVE = 0;\n  ACTIVE   = 1;\n}\n\r\n------------------------RiakExplorerTest--\r\n">>;
path_body(_, _) ->
   <<>>.

path_code(_, 'POST', ["tables", "query"]) -> "200";
path_code(_, 'POST', ["tables", table, "query"]) -> "200";
%% If the file has already been uploaded
path_code(true, 'POST', ["create"]) -> "204";
path_code(false, 'POST', ["create"]) -> "200";
path_code(_, 'POST', _) -> "202";
path_code(_, 'GET', ["staged-leave"]) -> "500";
path_code(_, 'GET', ["commit"]) -> "500";
path_code(_, 'GET', ["join", arg1]) -> "500";
path_code(_, 'GET', ["leave", arg1]) -> "500";
path_code(_, 'GET', ["staged-join", arg1]) -> "500";
path_code(_, 'GET', ["staged-leave", arg1]) -> "500";
path_code(_, 'GET', ["force-remove", arg1]) -> "500";
path_code(_, 'GET', ["repl-fullsync-start", arg1]) -> "500";
path_code(_, 'GET', _) -> "200";
path_code(_, 'DELETE', ["buckets",bucket]) -> "202";
path_code(_, 'DELETE', _) -> "204";
path_code(_, 'PUT', ["bucket_types", bucket_type]) -> "200";
path_code(_, 'PUT', _) -> "204".

path_headers('POST', ["create"]) ->
    [{"Content-Length", integer_to_list(byte_size(path_body('POST', ["create"])))},
     {"Content-Type", "multipart/form-data; boundary=----------------------RiakExplorerTest"}];
path_headers(_, _) ->
    [].


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

home() ->
    render_json([{explore, <<"riak_explorer api">>}]).

ping() ->
    render_json([{ping, pong}]).

render_json(Data) ->
    Body = binary_to_list(list_to_binary(mochijson2:encode(Data))),
    Body.

riak_type() ->
    {_, _, Data} = ret:http(get, "http://localhost:9000/explore/clusters/default"),
    {struct, JsonData} = mochijson2:decode(Data),
    {struct, Cluster} = proplists:get_value(<<"default">>, JsonData),
    RiakType = binary_to_list(proplists:get_value(<<"riak_type">>, Cluster)),
    case {lists:prefix("ts", RiakType),
          lists:suffix("ee", RiakType)} of
        {false, false} -> {kv, oss};
        {false, true} -> {kv, ee};
        {true, false} -> {ts, oss};
        {true, true} -> {ts, ee}
    end.

%% The '*repl*' paths are not testable when Riak OSS is being used
%% The '*tables*' paths are not testable when Riak KV is being used
is_testable_path(Base, [Path|_], RiakType) ->
    case {lists:prefix("repl", Path),
          lists:prefix("tables", Path),
          RiakType} of
        {true, _, {_, oss}} ->
            ?debugFmt("Skipping ~p/~p because we are on Riak OSS.~n", [Base, Path]),
            false;
        {_, true, {kv, _}} ->
            ?debugFmt("Skipping ~p/~p because we are on Riak KV.~n", [Base, Path]),
            false;
        _ -> true
    end.

-endif.
