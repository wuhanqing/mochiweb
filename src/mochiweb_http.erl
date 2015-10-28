%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP server.

-module(mochiweb_http).

-author('bob@mochimedia.com').

-export([start_link/2]).
-export([init/2, loop/2]).
-export([after_response/3, reentry/2]).
-export([parse_range_request/1, range_skip_length/2]).

-define(REQUEST_RECV_TIMEOUT, 300000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).

-ifdef(gen_tcp_r15b_workaround).
r15b_workaround() -> true.
-else.
r15b_workaround() -> false.
-endif.

start_link(Conn, Callback) ->
    {ok, spawn_link(?MODULE, init, [Conn, Callback])}.

init(Conn, Callback) ->
    {ok, NewConn} = Conn:wait(),
	loop(NewConn, Callback).

loop(Conn, Callback) ->
    ok = Conn:setopts([{packet, http}]),
    request(Conn, Callback).

request(Conn, Callback) ->
    ok = Conn:setopts([{active, once}]),
    receive
        {Protocol, _, {http_request, Method, Path, Version}} when Protocol == http orelse Protocol == ssl ->
            ok = Conn:setopts([{packet, httph}]),
            headers(Conn, {Method, Path, Version}, [], Callback, 0);
        {Protocol, _, {http_error, "\r\n"}} when Protocol == http orelse Protocol == ssl ->
            request(Conn, Callback);
        {Protocol, _, {http_error, "\n"}} when Protocol == http orelse Protocol == ssl ->
            request(Conn, Callback);
        {tcp_closed, _} ->
            Conn:close(),
            exit(normal);
        {ssl_closed, _} ->
            Conn:close(),
            exit(normal);
        Other ->
            handle_invalid_msg_request(Other, Conn)
    after ?REQUEST_RECV_TIMEOUT ->
        Conn:close(),
        exit(normal)
    end.

reentry(Conn, Callback) ->
    fun (Req) ->
            ?MODULE:after_response(Conn, Callback, Req)
    end.

headers(Conn, Request, Headers, _Callback, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok = Conn:setopts([{packet, raw}]),
    handle_invalid_request(Conn, Request, Headers);

headers(Conn, Request, Headers, Callback, HeaderCount) ->
    ok = Conn:setopts([{active, once}]),
    receive
        {Protocol, _, http_eoh} when Protocol == http orelse Protocol == ssl ->
            Req = new_request(Conn, Request, Headers),
            callback(Callback, Req),
            ?MODULE:after_response(Conn, Callback, Req);
        {Protocol, _, {http_header, _, Name, _, Value}} when Protocol == http orelse Protocol == ssl ->
            headers(Conn, Request, [{Name, Value} | Headers], Callback, 1 + HeaderCount);
        {tcp_closed, _} ->
            Conn:close(),
            exit(normal);
        Other ->
            handle_invalid_msg_request(Other, Conn, Request, Headers)
    after ?HEADERS_RECV_TIMEOUT ->
        Conn:close(),
        exit(normal)
    end.


-spec handle_invalid_msg_request(term(), esockd_connection:connection()) -> no_return().
handle_invalid_msg_request(Msg, Conn) ->
    handle_invalid_msg_request(Msg, Conn, {'GET', {abs_path, "/"}, {0,9}}, []).

-spec handle_invalid_msg_request(term(), esockd_connection:connection(), term(), term()) -> no_return().
handle_invalid_msg_request(Msg, Conn, Request, RevHeaders) ->

    case {Msg, r15b_workaround()} of
        {{tcp_error,_,emsgsize}, true} ->
            %% R15B02 returns this then closes the socket, so close and exit
            Conn:close(),
            exit(normal);
        _ ->
            handle_invalid_request(Conn, Request, RevHeaders)
    end.

-spec handle_invalid_request(esockd_connection:connection(), term(), term()) -> no_return().
handle_invalid_request(Conn, Request, RevHeaders) ->
    Req = new_request(Conn, Request, RevHeaders),
    Req:respond({400, [], []}),
    Conn:close(),
    exit(normal).

new_request(Conn, Request, RevHeaders) ->
    ok = Conn:setopts([{packet, raw}]),
    mochiweb:new_request({Conn, Request, lists:reverse(RevHeaders)}).

after_response(Conn, Callback, Req) ->
    case Req:should_close() of
        true ->
            Conn:close(),
            exit(normal);
        false ->
            Req:cleanup(),
            erlang:garbage_collect(),
            ?MODULE:loop(Conn, Callback)
    end.

parse_range_request("bytes=0-") ->
    undefined;
parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        RangeTokens = [string:strip(R) || R <- string:tokens(RangeString, ",")],
        Ranges = [R || R <- RangeTokens, string:len(R) > 0],
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when 0 =< Start, Start =< End, End < Size ->
            {Start, End - Start + 1};
        {Start, End} when 0 =< Start, Start =< End, End >= Size ->
            {Start, Size - Start};
        {_OutOfRange, _End} ->
            invalid_range
    end.

callback({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
callback({M, F}, Req) ->
    M:F(Req);
callback(Callback, Req) when is_function(Callback) ->
    Callback(Req).
