%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP server.

-module(mochiweb_http).

-author('bob@mochimedia.com').

-export([start_link/2]).
-export([init/2, loop/2]).
-export([after_response/2, reentry/1]).
-export([parse_range_request/1, range_skip_length/2]).

-define(REQUEST_RECV_TIMEOUT, 300000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).

-ifdef(gen_tcp_r15b_workaround).
r15b_workaround() -> true.
-else.
r15b_workaround() -> false.
-endif.

start_link(Socket, Callback) ->
	Pid = spawn_link(?MODULE, init, [Socket, Callback]),
	{ok, Pid}.

init(Socket, Callback) ->
	error_logger:info("~p: http request from ~p~n", [self(), mochiweb_socket:peername(Socket)]),
    %io:format("", [mochiweb_socket:peername(Socket)]),
	esockd_client:accepted(Socket),
	loop(Socket, Callback).

loop(Socket, Callback) ->
    ok = mochiweb_socket:setopts(Socket, [{packet, http}]),
    request(Socket, Callback).

request(Socket, Callback) ->
    ok = mochiweb_socket:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, {http_request, Method, Path, Version}} when Protocol == http orelse Protocol == ssl ->
            ok = mochiweb_socket:setopts(Socket, [{packet, httph}]),
            headers(Socket, {Method, Path, Version}, [], Callback, 0);
        {Protocol, _, {http_error, "\r\n"}} when Protocol == http orelse Protocol == ssl ->
            request(Socket, Callback);
        {Protocol, _, {http_error, "\n"}} when Protocol == http orelse Protocol == ssl ->
            request(Socket, Callback);
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        Other ->
            handle_invalid_msg_request(Other, Socket)
    after ?REQUEST_RECV_TIMEOUT ->
        mochiweb_socket:close(Socket),
        exit(normal)
    end.

reentry(Callback) ->
    fun (Req) ->
            ?MODULE:after_response(Callback, Req)
    end.

headers(Socket, Request, Headers, _Callback, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok = mochiweb_socket:setopts(Socket, [{packet, raw}]),
    handle_invalid_request(Socket, Request, Headers);

headers(Socket, Request, Headers, Callback, HeaderCount) ->
    ok = mochiweb_socket:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, http_eoh} when Protocol == http orelse Protocol == ssl ->
            Req = new_request(Socket, Request, Headers),
            callback(Callback, Req),
            ?MODULE:after_response(Callback, Req);
        {Protocol, _, {http_header, _, Name, _, Value}} when Protocol == http orelse Protocol == ssl ->
            headers(Socket, Request, [{Name, Value} | Headers], Callback,
                    1 + HeaderCount);
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        Other ->
            handle_invalid_msg_request(Other, Socket, Request, Headers)
    after ?HEADERS_RECV_TIMEOUT ->
        mochiweb_socket:close(Socket),
        exit(normal)
    end.


-spec handle_invalid_msg_request(term(), term()) -> no_return().
handle_invalid_msg_request(Msg, Socket) ->
    handle_invalid_msg_request(Msg, Socket, {'GET', {abs_path, "/"}, {0,9}}, []).

-spec handle_invalid_msg_request(term(), term(), term(), term()) -> no_return().
handle_invalid_msg_request(Msg, Socket, Request, RevHeaders) ->

    case {Msg, r15b_workaround()} of
        {{tcp_error,_,emsgsize}, true} ->
            %% R15B02 returns this then closes the socket, so close and exit
            mochiweb_socket:close(Socket),
            exit(normal);
        _ ->
            handle_invalid_request(Socket, Request, RevHeaders)
    end.

-spec handle_invalid_request(term(), term(), term()) -> no_return().
handle_invalid_request(Socket, Request, RevHeaders) ->
    Req = new_request(Socket, Request, RevHeaders),
    Req:respond({400, [], []}),
    mochiweb_socket:close(Socket),
    exit(normal).

new_request(Socket, Request, RevHeaders) ->
    ok = mochiweb_socket:setopts(Socket, [{packet, raw}]),
    mochiweb:new_request({Socket, Request, lists:reverse(RevHeaders)}).

after_response(Callback, Req) ->
    Socket = Req:get(socket),
    case Req:should_close() of
        true ->
            mochiweb_socket:close(Socket),
            exit(normal);
        false ->
            Req:cleanup(),
            erlang:garbage_collect(),
            ?MODULE:loop(Socket, Callback)
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
