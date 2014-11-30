-module(mochiweb_cookies_tests).

-import(mochiweb_cookies, [quote/1, parse_cookie/1, cookie/2, cookie/3]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

quote_test() ->
    %% ?assertError eunit macro is not compatible with coverage module
    try quote(":wq")
    catch error:{cookie_quoting_required, ":wq"} -> ok
    end,
    ?assertEqual(
       "foo",
       quote(foo)),
    ok.

parse_cookie_test() ->
    %% RFC example
    C1 = "$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\";
    Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
    Shipping=\"FedEx\"; $Path=\"/acme\"",
    ?assertEqual(
       [{"Customer","WILE_E_COYOTE"},
        {"Part_Number","Rocket_Launcher_0001"},
        {"Shipping","FedEx"}],
       parse_cookie(C1)),
    %% Potential edge cases
    ?assertEqual(
       [{"foo", "x"}],
       parse_cookie("foo=\"\\x\"")),
    ?assertEqual(
       [],
       parse_cookie("=")),
    ?assertEqual(
       [{"foo", ""}, {"bar", ""}],
       parse_cookie("  foo ; bar  ")),
    ?assertEqual(
       [{"foo", ""}, {"bar", ""}],
       parse_cookie("foo=;bar=")),
    ?assertEqual(
       [{"foo", "\";"}, {"bar", ""}],
       parse_cookie("foo = \"\\\";\";bar ")),
    ?assertEqual(
       [{"foo", "\";bar"}],
       parse_cookie("foo=\"\\\";bar")),
    ?assertEqual(
       [],
       parse_cookie([])),
    ?assertEqual(
       [{"foo", "bar"}, {"baz", "wibble"}],
       parse_cookie("foo=bar , baz=wibble ")),
    ok.

domain_test() ->
    ?assertEqual(
       {"Set-Cookie",
        "Customer=WILE_E_COYOTE; "
        "Version=1; "
        "Domain=acme.com; "
        "HttpOnly"},
       cookie("Customer", "WILE_E_COYOTE",
              [{http_only, true}, {domain, "acme.com"}])),
    ok.

local_time_test() ->
    {"Set-Cookie", S} = cookie("Customer", "WILE_E_COYOTE",
                               [{max_age, 111}, {secure, true}]),
    ?assertMatch(
       ["Customer=WILE_E_COYOTE",
        " Version=1",
        " Expires=" ++ _,
        " Max-Age=111",
        " Secure"],
       string:tokens(S, ";")),
    ok.

cookie_test() ->
    C1 = {"Set-Cookie",
          "Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Path=/acme"},
    C1 = cookie("Customer", "WILE_E_COYOTE", [{path, "/acme"}]),
    C1 = cookie("Customer", "WILE_E_COYOTE",
                [{path, "/acme"}, {badoption, "negatory"}]),
    C1 = cookie('Customer', 'WILE_E_COYOTE', [{path, '/acme'}]),
    C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>, [{path, <<"/acme">>}]),

    {"Set-Cookie","=NoKey; Version=1"} = cookie("", "NoKey", []),
    {"Set-Cookie","=NoKey; Version=1"} = cookie("", "NoKey"),
    LocalTime = calendar:universal_time_to_local_time({{2007, 5, 15}, {13, 45, 33}}),
    C2 = {"Set-Cookie",
          "Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Expires=Tue, 15-May-2007 13:45:33 GMT; "
          "Max-Age=0"},
    C2 = cookie("Customer", "WILE_E_COYOTE",
                [{max_age, -111}, {local_time, LocalTime}]),
    C3 = {"Set-Cookie",
          "Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Expires=Wed, 16-May-2007 13:45:50 GMT; "
          "Max-Age=86417"},
    C3 = cookie("Customer", "WILE_E_COYOTE",
                [{max_age, 86417}, {local_time, LocalTime}]),
    ok.

-endif.
