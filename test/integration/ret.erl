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

http(Method, URL, Body, [], ReturnHeader) ->
    Headers = [{"Content-Type", "application/json"}],
    http(Method, URL, Body, Headers, ReturnHeader);
http(Method, URL, Body, Headers, ReturnHeader) ->
    Method1 = list_to_atom(string:to_lower(atom_to_list(Method))),
    ensure_ibrowse(),
    Opts = [],

    Res = ibrowse:send_req(URL, Headers, Method1, Body, Opts),

    case ReturnHeader of
        true -> Res;
        _ ->
            case Res of
                {error, Reason} ->
                    {ok, 000, Reason};
                {ok, S, _, B} ->
                    {ok, S, B}
            end
    end.

http(Method, URL) ->
    http(Method, URL, <<>>, [], false).
http(Method, URL, Body) ->
    http(Method, URL, Body, [], false).
http(Method, URL, Body, Headers) ->
    http(Method, URL, Body, Headers, false).

-endif.
