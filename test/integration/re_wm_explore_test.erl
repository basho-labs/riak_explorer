%% @doc Test the API in various ways.
-module(re_wm_explore_test).

-compile(export_all).
-ifdef(integration_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
re_wm_explore_test_() ->
    {setup,
        fun () -> ok end,
        fun (_) -> ok end,
        {timeout, 60, [
            expected_data()
        ]}
    }.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

expected_data() ->
    [
     ?_assertEqual({ok, "200", home()}, ret:http(get, ret:url("/"))),
     ?_assertEqual({ok, "200", ping()}, ret:http(get, ret:url("/ping")))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

home() ->
  render_json([{message, <<"riak_explorer api">>}]).

ping() ->
  render_json([{message, <<"pong">>}]).

render_json(Data) ->
    Body = binary_to_list(list_to_binary(mochijson2:encode(Data))),
    Body.

-endif.