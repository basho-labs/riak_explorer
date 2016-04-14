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
     fun () -> 
             {ok, "204", _} = ret:http(put, "http://localhost:8098/buckets/test/keys/test", <<"testing">>)
     end,
     fun (_) -> ok end,
     {timeout, 60, [
                    expected_data(),
                    assert_list_types(),
                    explore_routes()
                   ]}
    }.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

expected_data() ->
    [
     ?_assertEqual({ok, "200", home()}, ret:http(get, ret:url("explore"))),
     ?_assertEqual({ok, "200", ping()}, ret:http(get, ret:url("explore/ping")))
    ].

assert_list_types() ->
    {Ok, Code, Payload} = ret:http(get, ret:url("explore/clusters/default/bucket_types")),
    {_,[{<<"bucket_types">>, [{_,DefaultType} | _]}]} = mochijson2:decode(Payload),
    [DefaultName, {<<"props">>, _}] = DefaultType,
    Expected = {<<"id">>,<<"default">>},
    
    [
     ?_assertEqual(ok, Ok),
     ?_assertEqual("200", Code),
     ?_assertEqual(Expected, DefaultName)
    ].

explore_routes() ->
    Routes = re_wm_explore:routes(),
    lists:flatten([ [ assert_paths(Method, Base, Paths, []) 
        || Method <- Methods ]
      || #route{base=[Base|_],path=Paths,methods=Methods} <- Routes ]).

assert_paths(_, _, [], Accum) -> lists:reverse(Accum);
assert_paths('DELETE'=Method, Base, [Path|Paths], Accum) ->
    Url = ret:url(to_path_str(Base) ++ "/" ++ to_path_str(Path)),
    ?debugMsg("------------------------------------------------------"),
    ?debugFmt("Method: ~p, Url: ~p", [Method, Url]),
    {ok, Code, Content} = ret:http(Method, Url),
    ?debugFmt("Code: ~p, Content: ~p", [Code, Content]),
    ExpectedCode = path_code(Method, Path),
    assert_paths(Method, Base, Paths, [?_assertEqual(ExpectedCode, Code)|Accum]);
assert_paths('POST'=Method, Base, [Path|Paths], Accum) ->
    Url = ret:url(to_path_str(Base) ++ "/" ++ to_path_str(Path)),
    Body = path_body(Path),
    ?debugMsg("------------------------------------------------------"),
    ?debugFmt("Method: ~p, Url: ~p", [Method, Url]),
    {ok, Code, Content} = ret:http(Method, Url, Body),
    ?debugFmt("Code: ~p, Content: ~p", [Code, Content]),
    ExpectedCode = path_code(Method, Path),
    assert_paths(Method, Base, Paths, [?_assertEqual(ExpectedCode, Code)|Accum]);
assert_paths('GET'=Method, Base, [Path|Paths], Accum) ->
    Url = ret:url(to_path_str(Base) ++ "/" ++ to_path_str(Path)),
    {ok, Code, Content} = ret:http(Method, Url),
    ?debugMsg("------------------------------------------------------"),
    ?debugFmt("Method: ~p, Url: ~p", [Method, Url]),
    ?debugFmt("Code: ~p, Content: ~p", [Code, Content]),
    ExpectedCode = path_code(Method, Path),
    assert_paths(Method, Base, Paths, [?_assertEqual(ExpectedCode, Code)|Accum]);
assert_paths(_, _, _, _) -> [].

to_path_str(["config", "files", file]) ->
    string:join(["config", "files", "riak.conf"], "/");
to_path_str(["log", "files", file]) ->
    string:join(["log", "files", "console.log"], "/");
to_path_str(Path) ->
    string:join([ path_part(P) || P <- Path ], "/").

path_part(P) when is_list(P) -> P;
path_part(cluster) -> "default";
path_part(node) -> "riak@127.0.0.1";
path_part(bucket_type) -> "default";
path_part(bucket) -> "test";
path_part(table) -> "GeoCheckin".

path_body(["tables", "query"]) ->
    <<"select * from GeoCheckin where time > 24 and time < 26 and myfamily = 'family1' and myseries = 'series1'">>;
path_body(["tables", table, "query"]) ->
   <<"[\"family2\", \"series99\", 26]">>;
path_body(_) ->
   <<>>.

path_code('POST', ["tables", "query"]) -> "200";
path_code('POST', ["tables", table, "query"]) -> "200";
path_code('POST', _) -> "202";
path_code('GET', _) -> "200";
path_code('DELETE', ["buckets",bucket]) -> "202";
path_code('DELETE', _) -> "204".

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

-endif.
