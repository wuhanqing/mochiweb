-module(mochiweb_util_tests).

-import(mochiweb_util, [make_io/1, parse_qs/1, record_to_proplist/2, record_to_proplist/3, shell_quote/1, 
	cmd_port/2, cmd/1, cmd_string/1, cmd_status/1, parse_header/1, guess_mime/1, path_split/1, urlsplit/1,
	urlsplit_path/1, pick_accepted_encodings/3, urlunsplit/1, join/2, quote_plus/1, urlunsplit_path/1, parse_qvalues/1,
	unquote/1, urlencode/1, partition/2, safe_relative_path/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_io_test() ->
    ?assertEqual(
       <<"atom">>,
       iolist_to_binary(make_io(atom))),
    ?assertEqual(
       <<"20">>,
       iolist_to_binary(make_io(20))),
    ?assertEqual(
       <<"list">>,
       iolist_to_binary(make_io("list"))),
    ?assertEqual(
       <<"binary">>,
       iolist_to_binary(make_io(<<"binary">>))),
    ok.

-record(test_record, {field1=f1, field2=f2}).
record_to_proplist_test() ->
    ?assertEqual(
       [{'__record', test_record},
        {field1, f1},
        {field2, f2}],
       record_to_proplist(#test_record{}, record_info(fields, test_record))),
    ?assertEqual(
       [{'typekey', test_record},
        {field1, f1},
        {field2, f2}],
       record_to_proplist(#test_record{},
                          record_info(fields, test_record),
                          typekey)),
    ok.

shell_quote_test() ->
    ?assertEqual(
       "\"foo \\$bar\\\"\\`' baz\"",
       shell_quote("foo $bar\"`' baz")),
    ok.

cmd_port_test_spool(Port, Acc) ->
    receive
        {Port, eof} ->
            Acc;
        {Port, {data, {eol, Data}}} ->
            cmd_port_test_spool(Port, ["\n", Data | Acc]);
        {Port, Unknown} ->
            throw({unknown, Unknown})
    after 1000 ->
            throw(timeout)
    end.

cmd_port_test() ->
    Port = cmd_port(["echo", "$bling$ `word`!"],
                    [eof, stream, {line, 4096}]),
    Res = try lists:append(lists:reverse(cmd_port_test_spool(Port, [])))
          after catch port_close(Port)
          end,
    self() ! {Port, wtf},
    try cmd_port_test_spool(Port, [])
    catch throw:{unknown, wtf} -> ok
    end,
    try cmd_port_test_spool(Port, [])
    catch throw:timeout -> ok
    end,
    ?assertEqual(
       "$bling$ `word`!\n",
       Res).

cmd_test() ->
    ?assertEqual(
       "$bling$ `word`!\n",
       cmd(["echo", "$bling$ `word`!"])),
    ok.

cmd_string_test() ->
    ?assertEqual(
       "\"echo\" \"\\$bling\\$ \\`word\\`!\"",
       cmd_string(["echo", "$bling$ `word`!"])),
    ok.

cmd_status_test() ->
    ?assertEqual(
       {0, <<"$bling$ `word`!\n">>},
       cmd_status(["echo", "$bling$ `word`!"])),
    ok.


parse_header_test() ->
    ?assertEqual(
       {"multipart/form-data", [{"boundary", "AaB03x"}]},
       parse_header("multipart/form-data; boundary=AaB03x")),
    %% This tests (currently) intentionally broken behavior
    ?assertEqual(
       {"multipart/form-data",
        [{"b", ""},
         {"cgi", "is"},
         {"broken", "true\"e"}]},
       parse_header("multipart/form-data;b=;cgi=\"i\\s;broken=true\"e;=z;z")),
    ok.

guess_mime_test() ->
    ?assertEqual("text/plain", guess_mime("")),
    ?assertEqual("text/plain", guess_mime(".text")),
    ?assertEqual("application/zip", guess_mime(".zip")),
    ?assertEqual("application/zip", guess_mime("x.zip")),
    ?assertEqual("text/html", guess_mime("x.html")),
    ?assertEqual("application/xhtml+xml", guess_mime("x.xhtml")),
    ?assertEqual("text/x-cross-domain-policy", guess_mime("crossdomain.xml")),
    ?assertEqual("text/x-cross-domain-policy", guess_mime("www/crossdomain.xml")),
    ok.

path_split_test() ->
    {"", "foo/bar"} = path_split("/foo/bar"),
    {"foo", "bar"} = path_split("foo/bar"),
    {"bar", ""} = path_split("bar"),
    ok.

urlsplit_test() ->
    {"", "", "/foo", "", "bar?baz"} = urlsplit("/foo#bar?baz"),
    {"http", "host:port", "/foo", "", "bar?baz"} =
        urlsplit("http://host:port/foo#bar?baz"),
    {"http", "host", "", "", ""} = urlsplit("http://host"),
    {"", "", "/wiki/Category:Fruit", "", ""} =
        urlsplit("/wiki/Category:Fruit"),
    ok.

urlsplit_path_test() ->
    {"/foo/bar", "", ""} = urlsplit_path("/foo/bar"),
    {"/foo", "baz", ""} = urlsplit_path("/foo?baz"),
    {"/foo", "", "bar?baz"} = urlsplit_path("/foo#bar?baz"),
    {"/foo", "", "bar?baz#wibble"} = urlsplit_path("/foo#bar?baz#wibble"),
    {"/foo", "bar", "baz"} = urlsplit_path("/foo?bar#baz"),
    {"/foo", "bar?baz", "baz"} = urlsplit_path("/foo?bar?baz#baz"),
    ok.

urlunsplit_test() ->
    "/foo#bar?baz" = urlunsplit({"", "", "/foo", "", "bar?baz"}),
    "http://host:port/foo#bar?baz" =
        urlunsplit({"http", "host:port", "/foo", "", "bar?baz"}),
    ok.

urlunsplit_path_test() ->
    "/foo/bar" = urlunsplit_path({"/foo/bar", "", ""}),
    "/foo?baz" = urlunsplit_path({"/foo", "baz", ""}),
    "/foo#bar?baz" = urlunsplit_path({"/foo", "", "bar?baz"}),
    "/foo#bar?baz#wibble" = urlunsplit_path({"/foo", "", "bar?baz#wibble"}),
    "/foo?bar#baz" = urlunsplit_path({"/foo", "bar", "baz"}),
    "/foo?bar?baz#baz" = urlunsplit_path({"/foo", "bar?baz", "baz"}),
    ok.

join_test() ->
    ?assertEqual("foo,bar,baz",
                  join(["foo", "bar", "baz"], $,)),
    ?assertEqual("foo,bar,baz",
                  join(["foo", "bar", "baz"], ",")),
    ?assertEqual("foo bar",
                  join([["foo", " bar"]], ",")),
    ?assertEqual("foo bar,baz",
                  join([["foo", " bar"], "baz"], ",")),
    ?assertEqual("foo",
                  join(["foo"], ",")),
    ?assertEqual("foobarbaz",
                  join(["foo", "bar", "baz"], "")),
    ?assertEqual("foo" ++ [<<>>] ++ "bar" ++ [<<>>] ++ "baz",
                 join(["foo", "bar", "baz"], <<>>)),
    ?assertEqual("foobar" ++ [<<"baz">>],
                 join(["foo", "bar", <<"baz">>], "")),
    ?assertEqual("",
                 join([], "any")),
    ok.

quote_plus_test() ->
    "foo" = quote_plus(foo),
    "1" = quote_plus(1),
    "1.1" = quote_plus(1.1),
    "foo" = quote_plus("foo"),
    "foo+bar" = quote_plus("foo bar"),
    "foo%0A" = quote_plus("foo\n"),
    "foo%0A" = quote_plus("foo\n"),
    "foo%3B%26%3D" = quote_plus("foo;&="),
    "foo%3B%26%3D" = quote_plus(<<"foo;&=">>),
    ok.

unquote_test() ->
    ?assertEqual("foo bar",
                 unquote("foo+bar")),
    ?assertEqual("foo bar",
                 unquote("foo%20bar")),
    ?assertEqual("foo\r\n",
                 unquote("foo%0D%0A")),
    ?assertEqual("foo\r\n",
                 unquote(<<"foo%0D%0A">>)),
    ok.

urlencode_test() ->
    "foo=bar&baz=wibble+%0D%0A&z=1" = urlencode([{foo, "bar"},
                                                 {"baz", "wibble \r\n"},
                                                 {z, 1}]),
    ok.

parse_qs_test() ->
    ?assertEqual(
       [{"foo", "bar"}, {"baz", "wibble \r\n"}, {"z", "1"}],
       parse_qs("foo=bar&baz=wibble+%0D%0a&z=1")),
    ?assertEqual(
       [{"", "bar"}, {"baz", "wibble \r\n"}, {"z", ""}],
       parse_qs("=bar&baz=wibble+%0D%0a&z=")),
    ?assertEqual(
       [{"foo", "bar"}, {"baz", "wibble \r\n"}, {"z", "1"}],
       parse_qs(<<"foo=bar&baz=wibble+%0D%0a&z=1">>)),
    ?assertEqual(
       [],
       parse_qs("")),
    ?assertEqual(
       [{"foo", ""}, {"bar", ""}, {"baz", ""}],
       parse_qs("foo;bar&baz")),
    ok.

partition_test() ->
    {"foo", "", ""} = partition("foo", "/"),
    {"foo", "/", "bar"} = partition("foo/bar", "/"),
    {"foo", "/", ""} = partition("foo/", "/"),
    {"", "/", "bar"} = partition("/bar", "/"),
    {"f", "oo/ba", "r"} = partition("foo/bar", "oo/ba"),
    ok.

safe_relative_path_test() ->
    "foo" = safe_relative_path("foo"),
    "foo/" = safe_relative_path("foo/"),
    "foo" = safe_relative_path("foo/bar/.."),
    "bar" = safe_relative_path("foo/../bar"),
    "bar/" = safe_relative_path("foo/../bar/"),
    "" = safe_relative_path("foo/.."),
    "" = safe_relative_path("foo/../"),
    undefined = safe_relative_path("/foo"),
    undefined = safe_relative_path("../foo"),
    undefined = safe_relative_path("foo/../.."),
    undefined = safe_relative_path("foo//"),
    undefined = safe_relative_path("foo\\bar"),
    ok.

parse_qvalues_test() ->
    [] = parse_qvalues(""),
    [{"identity", 0.0}] = parse_qvalues("identity;q=0"),
    [{"identity", 0.0}] = parse_qvalues("identity ;q=0"),
    [{"identity", 0.0}] = parse_qvalues(" identity; q =0 "),
    [{"identity", 0.0}] = parse_qvalues("identity ; q = 0"),
    [{"identity", 0.0}] = parse_qvalues("identity ; q= 0.0"),
    [{"gzip", 1.0}, {"deflate", 1.0}, {"identity", 0.0}] = parse_qvalues(
        "gzip,deflate,identity;q=0.0"
    ),
    [{"deflate", 1.0}, {"gzip", 1.0}, {"identity", 0.0}] = parse_qvalues(
        "deflate,gzip,identity;q=0.0"
    ),
    [{"gzip", 1.0}, {"deflate", 1.0}, {"gzip", 1.0}, {"identity", 0.0}] =
        parse_qvalues("gzip,deflate,gzip,identity;q=0"),
    [{"gzip", 1.0}, {"deflate", 1.0}, {"identity", 0.0}] = parse_qvalues(
        "gzip, deflate , identity; q=0.0"
    ),
    [{"gzip", 1.0}, {"deflate", 1.0}, {"identity", 0.0}] = parse_qvalues(
        "gzip; q=1, deflate;q=1.0, identity;q=0.0"
    ),
    [{"gzip", 0.5}, {"deflate", 1.0}, {"identity", 0.0}] = parse_qvalues(
        "gzip; q=0.5, deflate;q=1.0, identity;q=0"
    ),
    [{"gzip", 0.5}, {"deflate", 1.0}, {"identity", 0.0}] = parse_qvalues(
        "gzip; q=0.5, deflate , identity;q=0.0"
    ),
    [{"gzip", 0.5}, {"deflate", 0.8}, {"identity", 0.0}] = parse_qvalues(
        "gzip; q=0.5, deflate;q=0.8, identity;q=0.0"
    ),
    [{"gzip", 0.5}, {"deflate", 1.0}, {"identity", 1.0}] = parse_qvalues(
        "gzip; q=0.5,deflate,identity"
    ),
    [{"gzip", 0.5}, {"deflate", 1.0}, {"identity", 1.0}, {"identity", 1.0}] =
        parse_qvalues("gzip; q=0.5,deflate,identity, identity "),
    [{"text/html;level=1", 1.0}, {"text/plain", 0.5}] =
        parse_qvalues("text/html;level=1, text/plain;q=0.5"),
    [{"text/html;level=1", 0.3}, {"text/plain", 1.0}] =
        parse_qvalues("text/html;level=1;q=0.3, text/plain"),
    [{"text/html;level=1", 0.3}, {"text/plain", 1.0}] =
        parse_qvalues("text/html; level = 1; q = 0.3, text/plain"),
    [{"text/html;level=1", 0.3}, {"text/plain", 1.0}] =
        parse_qvalues("text/html;q=0.3;level=1, text/plain"),
    invalid_qvalue_string = parse_qvalues("gzip; q=1.1, deflate"),
    invalid_qvalue_string = parse_qvalues("gzip; q=0.5, deflate;q=2"),
    invalid_qvalue_string = parse_qvalues("gzip, deflate;q=AB"),
    invalid_qvalue_string = parse_qvalues("gzip; q=2.1, deflate"),
    invalid_qvalue_string = parse_qvalues("gzip; q=0.1234, deflate"),
    invalid_qvalue_string = parse_qvalues("text/html;level=1;q=0.3, text/html;level"),
    ok.

pick_accepted_encodings_test() ->
    ["identity"] = pick_accepted_encodings(
        [],
        ["gzip", "identity"],
        "identity"
    ),
    ["gzip", "identity"] = pick_accepted_encodings(
        [{"gzip", 1.0}],
        ["gzip", "identity"],
        "identity"
    ),
    ["identity"] = pick_accepted_encodings(
        [{"gzip", 0.0}],
        ["gzip", "identity"],
        "identity"
    ),
    ["gzip", "identity"] = pick_accepted_encodings(
        [{"gzip", 1.0}, {"deflate", 1.0}],
        ["gzip", "identity"],
        "identity"
    ),
    ["gzip", "identity"] = pick_accepted_encodings(
        [{"gzip", 0.5}, {"deflate", 1.0}],
        ["gzip", "identity"],
        "identity"
    ),
    ["identity"] = pick_accepted_encodings(
        [{"gzip", 0.0}, {"deflate", 0.0}],
        ["gzip", "identity"],
        "identity"
    ),
    ["gzip"] = pick_accepted_encodings(
        [{"gzip", 1.0}, {"deflate", 1.0}, {"identity", 0.0}],
        ["gzip", "identity"],
        "identity"
    ),
    ["gzip", "deflate", "identity"] = pick_accepted_encodings(
        [{"gzip", 1.0}, {"deflate", 1.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip", "deflate"] = pick_accepted_encodings(
        [{"gzip", 1.0}, {"deflate", 1.0}, {"identity", 0.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["deflate", "gzip", "identity"] = pick_accepted_encodings(
        [{"gzip", 0.2}, {"deflate", 1.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["deflate", "deflate", "gzip", "identity"] = pick_accepted_encodings(
        [{"gzip", 0.2}, {"deflate", 1.0}, {"deflate", 1.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["deflate", "gzip", "gzip", "identity"] = pick_accepted_encodings(
        [{"gzip", 0.2}, {"deflate", 1.0}, {"gzip", 1.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip", "deflate", "gzip", "identity"] = pick_accepted_encodings(
        [{"gzip", 0.2}, {"deflate", 0.9}, {"gzip", 1.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    [] = pick_accepted_encodings(
        [{"*", 0.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip", "deflate", "identity"] = pick_accepted_encodings(
        [{"*", 1.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip", "deflate", "identity"] = pick_accepted_encodings(
        [{"*", 0.6}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip"] = pick_accepted_encodings(
        [{"gzip", 1.0}, {"*", 0.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip", "deflate"] = pick_accepted_encodings(
        [{"gzip", 1.0}, {"deflate", 0.6}, {"*", 0.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["deflate", "gzip"] = pick_accepted_encodings(
        [{"gzip", 0.5}, {"deflate", 1.0}, {"*", 0.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip", "identity"] = pick_accepted_encodings(
        [{"deflate", 0.0}, {"*", 1.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ["gzip", "identity"] = pick_accepted_encodings(
        [{"*", 1.0}, {"deflate", 0.0}],
        ["gzip", "deflate", "identity"],
        "identity"
    ),
    ok.

-endif.
