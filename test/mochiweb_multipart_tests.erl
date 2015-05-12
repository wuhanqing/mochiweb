-module(mochiweb_multipart_tests).

-import(mochiweb_multipart, [parse_multipart_request/2, get_boundary/1, parse_form/1, get_boundary/1,
		find_boundary/2, find_in_binary/2, parse_headers/1, flash_multipart_hack/1, parts_to_body/3,
		multipart_body/4, parts_to_multipart_body/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(mp, {state, boundary, length, buffer, callback, req}).

fake_request(Socket, ContentType, Length) ->
    mochiweb_request:new(esockd_transport, Socket,
                         'POST',
                         "/multipart",
                         {1,1},
                         mochiweb_headers:make(
                           [{"content-type", ContentType},
                            {"content-length", Length}])).

test_callback({body, <<>>}, Rest=[body_end | _]) ->
    %% When expecting the body_end we might get an empty binary
    fun (Next) -> test_callback(Next, Rest) end;
test_callback({body, Got}, [{body, Expect} | Rest]) when Got =/= Expect ->
    %% Partial response
    GotSize = size(Got),
    <<Got:GotSize/binary, Expect1/binary>> = Expect,
    fun (Next) -> test_callback(Next, [{body, Expect1} | Rest]) end;
test_callback(Got, [Expect | Rest]) ->
    ?assertEqual(Got, Expect),
    case Rest of
        [] ->
            ok;
        _ ->
            fun (Next) -> test_callback(Next, Rest) end
    end.

find_boundary_test() ->
    B = <<"\r\n--X">>,
    {next_boundary, 0, 7} = find_boundary(B, <<"\r\n--X\r\nRest">>),
    {next_boundary, 1, 7} = find_boundary(B, <<"!\r\n--X\r\nRest">>),
    {end_boundary, 0, 9} = find_boundary(B, <<"\r\n--X--\r\nRest">>),
    {end_boundary, 1, 9} = find_boundary(B, <<"!\r\n--X--\r\nRest">>),
    not_found = find_boundary(B, <<"--X\r\nRest">>),
    {maybe, 0} = find_boundary(B, <<"\r\n--X\r">>),
    {maybe, 1} = find_boundary(B, <<"!\r\n--X\r">>),
    P = <<"\r\n-----------------------------16037454351082272548568224146">>,
    B0 = <<55,212,131,77,206,23,216,198,35,87,252,118,252,8,25,211,132,229,
          182,42,29,188,62,175,247,243,4,4,0,59, 13,10,45,45,45,45,45,45,45,
          45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,
          49,54,48,51,55,52,53,52,51,53,49>>,
    {maybe, 30} = find_boundary(P, B0),
    not_found = find_boundary(B, <<"\r\n--XJOPKE">>),
    ok.

find_in_binary_test() ->
    {exact, 0} = find_in_binary(<<"foo">>, <<"foobarbaz">>),
    {exact, 1} = find_in_binary(<<"oo">>, <<"foobarbaz">>),
    {exact, 8} = find_in_binary(<<"z">>, <<"foobarbaz">>),
    not_found = find_in_binary(<<"q">>, <<"foobarbaz">>),
    {partial, 7, 2} = find_in_binary(<<"azul">>, <<"foobarbaz">>),
    {exact, 0} = find_in_binary(<<"foobarbaz">>, <<"foobarbaz">>),
    {partial, 0, 3} = find_in_binary(<<"foobar">>, <<"foo">>),
    {partial, 1, 3} = find_in_binary(<<"foobar">>, <<"afoo">>),
    ok.

parse_headers_test() ->
    ?assertEqual([], parse_headers(<<>>)).

flash_multipart_hack_test() ->
    Buffer = <<"prefix-">>,
    Prefix = <<"prefix">>,
    State = #mp{length=0, buffer=Buffer, boundary=Prefix},
    ?assertEqual(State,
                 flash_multipart_hack(State)).

parts_to_body_single_test() ->
    {HL, B} = parts_to_body([{0, 5, <<"01234">>}],
                            "text/plain",
                            10),
    [{"Content-Range", Range},
     {"Content-Type", Type}] = lists:sort(HL),
    ?assertEqual(
       <<"bytes 0-5/10">>,
       iolist_to_binary(Range)),
    ?assertEqual(
       <<"text/plain">>,
       iolist_to_binary(Type)),
    ?assertEqual(
       <<"01234">>,
       iolist_to_binary(B)),
    ok.

parts_to_body_multi_test() ->
    {[{"Content-Type", Type}],
     _B} = parts_to_body([{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
                        "text/plain",
                        10),
    ?assertMatch(
       <<"multipart/byteranges; boundary=", _/binary>>,
       iolist_to_binary(Type)),
    ok.

parts_to_multipart_body_test() ->
    {[{"Content-Type", V}], B} = parts_to_multipart_body(
                                   [{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
                                   "text/plain",
                                   10,
                                   "BOUNDARY"),
    MB = multipart_body(
           [{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
           "text/plain",
           "BOUNDARY",
           10),
    ?assertEqual(
       <<"multipart/byteranges; boundary=BOUNDARY">>,
       iolist_to_binary(V)),
    ?assertEqual(
       iolist_to_binary(MB),
       iolist_to_binary(B)),
    ok.

multipart_body_test() ->
    ?assertEqual(
       <<"--BOUNDARY--\r\n">>,
       iolist_to_binary(multipart_body([], "text/plain", "BOUNDARY", 0))),
    ?assertEqual(
       <<"--BOUNDARY\r\n"
         "Content-Type: text/plain\r\n"
         "Content-Range: bytes 0-5/10\r\n\r\n"
         "01234\r\n"
         "--BOUNDARY\r\n"
         "Content-Type: text/plain\r\n"
         "Content-Range: bytes 5-10/10\r\n\r\n"
         "56789\r\n"
         "--BOUNDARY--\r\n">>,
       iolist_to_binary(multipart_body([{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
                                       "text/plain",
                                       "BOUNDARY",
                                       10))),
    ok.

-endif.
