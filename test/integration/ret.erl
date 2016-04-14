%% @doc Test helper functions.
-module(ret).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-ifdef(integration_test).

fmt(S, Args) ->
    lists:flatten(io_lib:format(S, Args)).

re_host() ->
    case os:getenv("RE_HOST") of
        false -> "localhost";
        Host -> Host
    end.

re_port() ->
    case os:getenv("RE_PORT") of
        false -> 9000;
        Port -> list_to_integer(Port)
    end.

url(Path) ->
    fmt("http://~s:~B/~s", [re_host(), re_port(), Path]).

ensure_ibrowse() ->
    case whereis(ibrowse) of
        undefined -> ibrowse:start();
        Any when is_pid(Any)-> ok
    end.

http(Method, URL, Body, H0, ReturnHeader) ->
    Method1 = list_to_atom(string:to_lower(atom_to_list(Method))),
    ensure_ibrowse(),
    Opts = [],

    Headers = H0 ++ [
                     {"Content-Type", "application/json"}
                    ],

    Res = ibrowse:send_req(URL, Headers, Method1, Body, Opts),

    case ReturnHeader of
        true -> Res;
        _ ->
            {ok, S, _, B} = Res,
            {ok, S, B}
    end.

http(Method, URL) ->
    http(Method, URL, <<>>, [], false).
http(Method, URL, Body) ->
    http(Method, URL, Body, [], false).
http(Method, URL, Body, Headers) ->
    http(Method, URL, Body, Headers, false).

-endif.
