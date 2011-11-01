%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP server.

-module(mochiweb_http).
-author('bob@mochimedia.com').
-export([start/1, start_link/1, stop/0, stop/1]).
-export([loop/2, loop/3]).
-export([after_response/2, reentry/1]).
-export([parse_range_request/1, range_skip_length/2]).

-define(REQUEST_RECV_TIMEOUT, 300000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).
-define(MAX_HEADER_BYTES, 256*1024).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).

parse_options(Options) ->
    {loop, HttpLoop} = proplists:lookup(loop, Options),
    LoopArgs = case proplists:lookup(max_header_bytes, Options) of
                   none ->
                       [HttpLoop];
                   {max_header_bytes, MaxHdrBytes} ->
                       [HttpLoop, MaxHdrBytes]
               end,
    Loop = {?MODULE, loop, LoopArgs},
    Options1 = [{loop, Loop} | proplists:delete(loop, Options)],
    Options2 = proplists:delete(max_header_bytes, Options1),
    mochilists:set_defaults(?DEFAULTS, Options2).

stop() ->
    mochiweb_socket_server:stop(?MODULE).

stop(Name) ->
    mochiweb_socket_server:stop(Name).

%% @spec start(Options) -> ServerRet
%%     Options = [option()]
%%     Option = {name, atom()} | {ip, string() | tuple()} | {backlog, integer()}
%%              | {nodelay, boolean()} | {acceptor_pool_size, integer()}
%%              | {ssl, boolean()} | {profile_fun, undefined | (Props) -> ok}
%%              | {link, false}
%% @doc Start a mochiweb server.
%%      profile_fun is used to profile accept timing.
%%      After each accept, if defined, profile_fun is called with a proplist of a subset of the mochiweb_socket_server state and timing information.
%%      The proplist is as follows: [{name, Name}, {port, Port}, {active_sockets, ActiveSockets}, {timing, Timing}].
%% @end
start(Options) ->
    mochiweb_socket_server:start(parse_options(Options)).

start_link(Options) ->
    mochiweb_socket_server:start_link(parse_options(Options)).

loop(Socket, Body) ->
    loop(Socket, Body, ?MAX_HEADER_BYTES).

loop(Socket, Body, MaxHdrBytes) ->
    ok = mochiweb_socket:setopts(Socket, [{packet, line}]),
    request(Socket, Body, MaxHdrBytes, <<>>).

request(Socket, Body, MaxHdrBytes, Prev) ->
    ok = mochiweb_socket:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, Bin}
          when (Protocol =:= tcp orelse Protocol =:= ssl)
               andalso (byte_size(Bin) + byte_size(Prev) < MaxHdrBytes) ->
            FullBin = <<Prev/binary, Bin/binary>>,
            case erlang:decode_packet(http, FullBin, []) of
                {ok, {http_request, Method, Path, Version}, <<>>} ->
                    collect_headers(Socket, {Method, Path, Version}, Body,
                                    MaxHdrBytes, ?MAX_HEADERS,
                                    byte_size(FullBin), 0, false, <<>>);
                {error, {http_error, "\r\n"}} ->
                    request(Socket, Body, MaxHdrBytes, <<>>);
                {error, {http_error, "\n"}} ->
                    request(Socket, Body, MaxHdrBytes, <<>>);
                {more, _} ->
                    request(Socket, Body, MaxHdrBytes, FullBin)
            end;
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        _Other ->
            handle_invalid_request(Socket)
    after ?REQUEST_RECV_TIMEOUT ->
            mochiweb_socket:close(Socket),
            exit(normal)
    end.

reentry(Body) ->
    fun (Req) ->
            ?MODULE:after_response(Body, Req)
    end.

collect_headers(Socket, Request, _Body, _MaxHdrBytes, MaxHdrCount,
                _HdrBytes, HdrCount, _Trunc, _Collected) when HdrCount >= MaxHdrCount ->
    %% Too many headers sent, bad request.
    handle_invalid_request(Socket, Request, []);
collect_headers(Socket, Request, Body, MaxHdrBytes, MaxHdrCount,
                HdrBytes, HdrCount, Trunc, Collected) ->
    ok = mochiweb_socket:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, More}
          when (Protocol =:= tcp orelse Protocol =:= ssl)
               andalso byte_size(More) + HdrBytes < MaxHdrBytes ->
            case Trunc of
                false when More =:= <<"\n">> orelse More =:= <<"\r\n">> ->
                    ok = mochiweb_socket:setopts(Socket, [{packet, raw}]),
                    parse_headers(Socket, Request, Body,
                                  <<Collected/binary, "\r\n">>, []);
                _ ->
                    LastByteIndex = byte_size(More) - 1,
                    NewHdrCount = case More of
                                      <<_:LastByteIndex/binary, $\n>> ->
                                          1 + HdrCount;
                                      _ ->
                                          HdrCount
                                  end,
                    collect_headers(
                      Socket, Request, Body, MaxHdrBytes, MaxHdrCount,
                      byte_size(More) + HdrBytes,
                      NewHdrCount,
                      false,
                      <<Collected/binary, More/binary>>)
            end;
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        _Other ->
            handle_invalid_request(Socket, Request, [])
    after ?HEADERS_RECV_TIMEOUT ->
            mochiweb_socket:close(Socket),
            exit(normal)
    end.

parse_headers(Socket, Request, Body, <<"\r\n">>, Headers) ->
    Req = new_request(Socket, Request, lists:reverse(Headers)),
    call_body(Body, Req),
    ?MODULE:after_response(Body, Req);
parse_headers(Socket, Request, Body, Bin, Headers) ->
    case erlang:decode_packet(httph, Bin, []) of
        {ok, {http_header, _, Name, _, Value}, More} ->
            parse_headers(Socket, Request, Body, More,
                          [{Name, Value} | Headers]);
        {more, _} ->
            handle_invalid_request(Socket, Request, Headers);
        {error, _Reason} ->
            mochiweb_socket:close(Socket),
            exit(normal)
    end.

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

-spec handle_invalid_request(term()) -> no_return().
handle_invalid_request(Socket) ->
    handle_invalid_request(Socket, {'GET', {abs_path, "/"}, {0,9}}, []),
    exit(normal).

-spec handle_invalid_request(term(), term(), term()) -> no_return().
handle_invalid_request(Socket, Request, RevHeaders) ->
    Req = new_request(Socket, Request, RevHeaders),
    Req:respond({400, [], []}),
    mochiweb_socket:close(Socket),
    exit(normal).

new_request(Socket, Request, RevHeaders) ->
    ok = mochiweb_socket:setopts(Socket, [{packet, raw}]),
    mochiweb:new_request({Socket, Request, lists:reverse(RevHeaders)}).

after_response(Body, Req) ->
    Socket = Req:get(socket),
    case Req:should_close() of
        true ->
            mochiweb_socket:close(Socket),
            exit(normal);
        false ->
            Req:cleanup(),
            ?MODULE:loop(Socket, Body)
    end.

parse_range_request("bytes=0-") ->
    undefined;
parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
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
        {_OutOfRange, _End} ->
            invalid_range
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

range_test() ->
    %% valid, single ranges
    ?assertEqual([{20, 30}], parse_range_request("bytes=20-30")),
    ?assertEqual([{20, none}], parse_range_request("bytes=20-")),
    ?assertEqual([{none, 20}], parse_range_request("bytes=-20")),

    %% trivial single range
    ?assertEqual(undefined, parse_range_request("bytes=0-")),

    %% invalid, single ranges
    ?assertEqual(fail, parse_range_request("")),
    ?assertEqual(fail, parse_range_request("garbage")),
    ?assertEqual(fail, parse_range_request("bytes=-20-30")),

    %% valid, multiple range
    ?assertEqual(
       [{20, 30}, {50, 100}, {110, 200}],
       parse_range_request("bytes=20-30,50-100,110-200")),
    ?assertEqual(
       [{20, none}, {50, 100}, {none, 200}],
       parse_range_request("bytes=20-,50-100,-200")),

    %% no ranges
    ?assertEqual([], parse_range_request("bytes=")),
    ok.

range_skip_length_test() ->
    Body = <<"012345678901234567890123456789012345678901234567890123456789">>,
    BodySize = byte_size(Body), %% 60
    BodySize = 60,

    %% these values assume BodySize =:= 60
    ?assertEqual({1,9}, range_skip_length({1,9}, BodySize)), %% 1-9
    ?assertEqual({10,10}, range_skip_length({10,19}, BodySize)), %% 10-19
    ?assertEqual({40, 20}, range_skip_length({none, 20}, BodySize)), %% -20
    ?assertEqual({30, 30}, range_skip_length({30, none}, BodySize)), %% 30-

    %% valid edge cases for range_skip_length
    ?assertEqual({BodySize, 0}, range_skip_length({none, 0}, BodySize)),
    ?assertEqual({0, BodySize}, range_skip_length({none, BodySize}, BodySize)),
    ?assertEqual({0, BodySize}, range_skip_length({0, none}, BodySize)),
    BodySizeLess1 = BodySize - 1,
    ?assertEqual({BodySizeLess1, 1},
                 range_skip_length({BodySize - 1, none}, BodySize)),

    %% out of range, return whole thing
    ?assertEqual({0, BodySize},
                 range_skip_length({none, BodySize + 1}, BodySize)),
    ?assertEqual({0, BodySize},
                 range_skip_length({none, -1}, BodySize)),

    %% invalid ranges
    ?assertEqual(invalid_range,
                 range_skip_length({-1, 30}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({0, BodySize + 1}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({-1, BodySize + 1}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({BodySize, 40}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({-1, none}, BodySize)),
    ?assertEqual(invalid_range,
                 range_skip_length({BodySize, none}, BodySize)),
    ok.

fake_req(ReqBytes) ->
    fake_req(?MAX_HEADER_BYTES, ReqBytes).

fake_req(MaxHdrBytes, ReqBytes) ->
    {ok, LS} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(LS),
    Self = self(),
    spawn_link(fun() ->
                       {ok, S} = gen_tcp:accept(LS),
                       try
                           loop(S, fun(Req) ->
                                           Self ! {link_header, Req:get_header_value("Link")},
                                           Req:ok({"text/plain", "ok"})
                                   end,
                                MaxHdrBytes)
                       after
                           gen_tcp:close(S),
                           gen_tcp:close(LS)
                       end
               end),
    {ok, S} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    try
        ok = gen_tcp:send(S, ReqBytes),
        inet:setopts(S, [{packet, http}]),
        {gen_tcp:recv(S, 0),
         receive {link_header, L} -> L after 0 -> undefined end}
    after
        gen_tcp:close(S)
    end.

loop_test_() ->
    Recbuf = 8192,
    [{"long request line (max = 256KiB)",
      ?_assertEqual(
         {{ok, {http_response, {1,1}, 200, "OK"}}, undefined},
         fake_req(
           "GET /" ++ string:chars($X, Recbuf) ++ " HTTP/1.1\r\n"
           ++ "Host: localhost\r\n\r\n"))},
     {"long header line (max = 256KiB)",
      ?_assertEqual(
         {{ok, {http_response, {1,1}, 200, "OK"}}, "/" ++ string:chars($X, Recbuf)},
         fake_req(
           "GET / HTTP/1.1\r\n"
           ++ "Host: localhost\r\n"
           ++ "Link: /" ++ string:chars($X, Recbuf) ++ "\r\n\r\n"))},
     {"incomplete req too long (max = 100B)",
      ?_assertEqual(
         {{ok, {http_response, {1,1}, 400, "Bad Request"}}, undefined},
         fake_req(
           100,
           "GET /" ++ string:chars($X, Recbuf) ++ " HTTP/1.1"))},
     {"incomplete req + header too long (max = 100B)",
      ?_assertEqual(
         {{ok, {http_response, {1,1}, 400, "Bad Request"}}, undefined},
         fake_req(
           100,
           "GET / HTTP/1.1\r\n"
           ++ "Host: localhost\r\n"
           ++ "Link: /" ++ string:chars($X, Recbuf)))},
     {"complete req + header too long (max = 100B)",
      ?_assertEqual(
         {{ok, {http_response, {1,1}, 400, "Bad Request"}}, undefined},
         fake_req(
           100,
           "GET /" ++ string:chars($X, 80) ++ " HTTP/1.1\r\n"
           ++ "Host: localhost\r\n\r\n"))}].

-endif.
