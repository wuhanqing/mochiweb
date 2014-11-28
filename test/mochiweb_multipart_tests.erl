-module(mochiweb_multipart_tests).

-import(mochiweb_multipart, [parse_multipart_request/2, get_boundary/1, parse_form/1, get_boundary/1,
		find_boundary/2, find_in_binary/2, parse_headers/1, flash_multipart_hack/1, parts_to_body/3,
		multipart_body/4, parts_to_multipart_body/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(mp, {state, boundary, length, buffer, callback, req}).

ssl_cert_opts() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    CertDir = filename:join([EbinDir, "..", "support", "test-materials"]),
    CertFile = filename:join(CertDir, "test_ssl_cert.pem"),
    KeyFile = filename:join(CertDir, "test_ssl_key.pem"),
    [{certfile, CertFile}, {keyfile, KeyFile}].

with_socket_server(Transport, ServerFun, ClientFun) ->
    ServerOpts0 = [{ip, "127.0.0.1"}, {port, 0}, {loop, ServerFun}],
    ServerOpts = case Transport of
        plain ->
            ServerOpts0;
        ssl ->
            ServerOpts0 ++ [{ssl, true}, {ssl_opts, ssl_cert_opts()}]
    end,
    {ok, Server} = mochiweb_socket_server:start_link(ServerOpts),
    Port = mochiweb_socket_server:get(Server, port),
    ClientOpts = [binary, {active, false}],
    {ok, Client} = case Transport of
        plain ->
            gen_tcp:connect("127.0.0.1", Port, ClientOpts);
        ssl ->
            ClientOpts1 = [{ssl_imp, new} | ClientOpts],
            {ok, SslSocket} = ssl:connect("127.0.0.1", Port, ClientOpts1),
            {ok, {ssl, SslSocket}}
    end,
    Res = (catch ClientFun(Client)),
    mochiweb_socket_server:stop(Server),
    Res.

fake_request(Socket, ContentType, Length) ->
    mochiweb_request:new(Socket,
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

parse3_http_test() ->
    parse3(plain).

parse3_https_test() ->
    parse3(ssl).

parse3(Transport) ->
    ContentType = "multipart/form-data; boundary=---------------------------7386909285754635891697677882",
    BinContent = <<"-----------------------------7386909285754635891697677882\r\nContent-Disposition: form-data; name=\"hidden\"\r\n\r\nmultipart message\r\n-----------------------------7386909285754635891697677882\r\nContent-Disposition: form-data; name=\"file\"; filename=\"test_file.txt\"\r\nContent-Type: text/plain\r\n\r\nWoo multiline text file\n\nLa la la\r\n-----------------------------7386909285754635891697677882--\r\n">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "hidden"}]}}]},
              {body, <<"multipart message">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", "test_file.txt"}]}},
                {"content-type", {"text/plain", []}}]},
              {body, <<"Woo multiline text file\n\nLa la la">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse2_http_test() ->
    parse2(plain).

parse2_https_test() ->
    parse2(ssl).

parse2(Transport) ->
    ContentType = "multipart/form-data; boundary=---------------------------6072231407570234361599764024",
    BinContent = <<"-----------------------------6072231407570234361599764024\r\nContent-Disposition: form-data; name=\"hidden\"\r\n\r\nmultipart message\r\n-----------------------------6072231407570234361599764024\r\nContent-Disposition: form-data; name=\"file\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n-----------------------------6072231407570234361599764024--\r\n">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "hidden"}]}}]},
              {body, <<"multipart message">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", ""}]}},
                {"content-type", {"application/octet-stream", []}}]},
              {body, <<>>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_form_http_test() ->
    do_parse_form(plain).

parse_form_https_test() ->
    do_parse_form(ssl).

do_parse_form(Transport) ->
    ContentType = "multipart/form-data; boundary=AaB03x",
    "AaB03x" = get_boundary(ContentType),
    Content = mochiweb_util:join(
                ["--AaB03x",
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--AaB03x",
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "",
                 "... contents of file1.txt ...",
                 "--AaB03x--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_form(Req),
                        [{"submit-name", "Larry"},
                         {"files", {"file1.txt", {"text/plain",[]},
                                    <<"... contents of file1.txt ...">>}
                         }] = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_http_test() ->
    do_parse(plain).

parse_https_test() ->
    do_parse(ssl).

do_parse(Transport) ->
    ContentType = "multipart/form-data; boundary=AaB03x",
    "AaB03x" = get_boundary(ContentType),
    Content = mochiweb_util:join(
                ["--AaB03x",
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--AaB03x",
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "",
                 "... contents of file1.txt ...",
                 "--AaB03x--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "submit-name"}]}}]},
              {body, <<"Larry">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "files"}, {"filename", "file1.txt"}]}},
                 {"content-type", {"text/plain", []}}]},
              {body, <<"... contents of file1.txt ...">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_partial_body_boundary_http_test() ->
   parse_partial_body_boundary(plain).

parse_partial_body_boundary_https_test() ->
   parse_partial_body_boundary(ssl).

parse_partial_body_boundary(Transport) ->
    Boundary = string:copies("$", 2048),
    ContentType = "multipart/form-data; boundary=" ++ Boundary,
    ?assertEqual(Boundary, get_boundary(ContentType)),
    Content = mochiweb_util:join(
                ["--" ++ Boundary,
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--" ++ Boundary,
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "",
                 "... contents of file1.txt ...",
                 "--" ++ Boundary ++ "--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "submit-name"}]}}]},
              {body, <<"Larry">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "files"}, {"filename", "file1.txt"}]}},
                {"content-type", {"text/plain", []}}
               ]},
              {body, <<"... contents of file1.txt ...">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_large_header_http_test() ->
    parse_large_header(plain).

parse_large_header_https_test() ->
    parse_large_header(ssl).

parse_large_header(Transport) ->
    ContentType = "multipart/form-data; boundary=AaB03x",
    "AaB03x" = get_boundary(ContentType),
    Content = mochiweb_util:join(
                ["--AaB03x",
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--AaB03x",
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "x-large-header: " ++ string:copies("%", 4096),
                 "",
                 "... contents of file1.txt ...",
                 "--AaB03x--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "submit-name"}]}}]},
              {body, <<"Larry">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "files"}, {"filename", "file1.txt"}]}},
                {"content-type", {"text/plain", []}},
                {"x-large-header", {string:copies("%", 4096), []}}
               ]},
              {body, <<"... contents of file1.txt ...">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

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

flash_parse_http_test() ->
    flash_parse(plain).

flash_parse_https_test() ->
    flash_parse(ssl).

flash_parse(Transport) ->
    ContentType = "multipart/form-data; boundary=----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5",
    "----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5" = get_boundary(ContentType),
    BinContent = <<"------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Filename\"\r\n\r\nhello.txt\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"success_action_status\"\r\n\r\n201\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"file\"; filename=\"hello.txt\"\r\nContent-Type: application/octet-stream\r\n\r\nhello\n\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Upload\"\r\n\r\nSubmit Query\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5--">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Filename"}]}}]},
              {body, <<"hello.txt">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "success_action_status"}]}}]},
              {body, <<"201">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", "hello.txt"}]}},
                {"content-type", {"application/octet-stream", []}}]},
              {body, <<"hello\n">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Upload"}]}}]},
              {body, <<"Submit Query">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

flash_parse2_http_test() ->
    flash_parse2(plain).

flash_parse2_https_test() ->
    flash_parse2(ssl).

flash_parse2(Transport) ->
    ContentType = "multipart/form-data; boundary=----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5",
    "----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5" = get_boundary(ContentType),
    Chunk = iolist_to_binary(string:copies("%", 4096)),
    BinContent = <<"------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Filename\"\r\n\r\nhello.txt\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"success_action_status\"\r\n\r\n201\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"file\"; filename=\"hello.txt\"\r\nContent-Type: application/octet-stream\r\n\r\n", Chunk/binary, "\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Upload\"\r\n\r\nSubmit Query\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5--">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Filename"}]}}]},
              {body, <<"hello.txt">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "success_action_status"}]}}]},
              {body, <<"201">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", "hello.txt"}]}},
                {"content-type", {"application/octet-stream", []}}]},
              {body, Chunk},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Upload"}]}}]},
              {body, <<"Submit Query">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
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

%% @todo Move somewhere more appropriate than in the test suite

multipart_parsing_benchmark_test() ->
  run_multipart_parsing_benchmark(1).

run_multipart_parsing_benchmark(0) -> ok;
run_multipart_parsing_benchmark(N) ->
     multipart_parsing_benchmark(),
     run_multipart_parsing_benchmark(N-1).

multipart_parsing_benchmark() ->
    ContentType = "multipart/form-data; boundary=----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5",
    Chunk = binary:copy(<<"This Is_%Some=Quite0Long4String2Used9For7BenchmarKing.5">>, 102400),
    BinContent = <<"------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Filename\"\r\n\r\nhello.txt\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"success_action_status\"\r\n\r\n201\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"file\"; filename=\"hello.txt\"\r\nContent-Type: application/octet-stream\r\n\r\n", Chunk/binary, "\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Upload\"\r\n\r\nSubmit Query\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5--">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Filename"}]}}]},
              {body, <<"hello.txt">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "success_action_status"}]}}]},
              {body, <<"201">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", "hello.txt"}]}},
                {"content-type", {"application/octet-stream", []}}]},
              {body, Chunk},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Upload"}]}}]},
              {body, <<"Submit Query">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
                        exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(plain, ServerFun, ClientFun),
    ok.
-endif.
