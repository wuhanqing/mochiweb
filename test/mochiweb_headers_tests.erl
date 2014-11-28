-module(mochiweb_headers_tests).

-import(mochiweb_headers, [make/1, enter_from_list/2, to_list/1, 
	default_from_list/2, tokenize_header_value/1, get_combined_value/2,
	get_primary_value/2]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_test() ->
    Identity = make([{hdr, foo}]),
    ?assertEqual(
       Identity,
       make(Identity)).

enter_from_list_test() ->
    H = make([{hdr, foo}]),
    ?assertEqual(
       [{baz, "wibble"}, {hdr, "foo"}],
       to_list(enter_from_list([{baz, wibble}], H))),
    ?assertEqual(
       [{hdr, "bar"}],
       to_list(enter_from_list([{hdr, bar}], H))),
    ok.

default_from_list_test() ->
    H = make([{hdr, foo}]),
    ?assertEqual(
       [{baz, "wibble"}, {hdr, "foo"}],
       to_list(default_from_list([{baz, wibble}], H))),
    ?assertEqual(
       [{hdr, "foo"}],
       to_list(default_from_list([{hdr, bar}], H))),
    ok.

get_primary_value_test() ->
    H = make([{hdr, foo}, {baz, <<"wibble;taco">>}]),
    ?assertEqual(
       "foo",
       get_primary_value(hdr, H)),
    ?assertEqual(
       undefined,
       get_primary_value(bar, H)),
    ?assertEqual(
       "wibble",
       get_primary_value(<<"baz">>, H)),
    ok.

get_combined_value_test() ->
    H = make([{hdr, foo}, {baz, <<"wibble,taco">>}, {content_length, "123, 123"},
              {test, " 123,  123,     123  , 123,123 "},
              {test2, "456,  123,     123  , 123"},
              {test3, "123"}, {test4, " 123, "}]),
    ?assertEqual(
       "foo",
       get_combined_value(hdr, H)),
    ?assertEqual(
       undefined,
       get_combined_value(bar, H)),
    ?assertEqual(
       undefined,
       get_combined_value(<<"baz">>, H)),
    ?assertEqual(
       "123",
       get_combined_value(<<"content_length">>, H)),
    ?assertEqual(
       "123",
       get_combined_value(<<"test">>, H)),
    ?assertEqual(
       undefined,
       get_combined_value(<<"test2">>, H)),
    ?assertEqual(
       "123",
       get_combined_value(<<"test3">>, H)),
    ?assertEqual(
       "123",
       get_combined_value(<<"test4">>, H)),
    ok.

set_cookie_test() ->
    H = make([{"set-cookie", foo}, {"set-cookie", bar}, {"set-cookie", baz}]),
    ?assertEqual(
       [{"set-cookie", "foo"}, {"set-cookie", "bar"}, {"set-cookie", "baz"}],
       to_list(H)),
    ok.

headers_test() ->
    H = ?MODULE:make([{hdr, foo}, {"Hdr", "bar"}, {'Hdr', 2}]),
    [{hdr, "foo, bar, 2"}] = ?MODULE:to_list(H),
    H1 = ?MODULE:insert(taco, grande, H),
    [{hdr, "foo, bar, 2"}, {taco, "grande"}] = ?MODULE:to_list(H1),
    H2 = ?MODULE:make([{"Set-Cookie", "foo"}]),
    [{"Set-Cookie", "foo"}] = ?MODULE:to_list(H2),
    H3 = ?MODULE:insert("Set-Cookie", "bar", H2),
    [{"Set-Cookie", "foo"}, {"Set-Cookie", "bar"}] = ?MODULE:to_list(H3),
    "foo, bar" = ?MODULE:get_value("set-cookie", H3),
    {value, {"Set-Cookie", "foo, bar"}} = ?MODULE:lookup("set-cookie", H3),
    undefined = ?MODULE:get_value("shibby", H3),
    none = ?MODULE:lookup("shibby", H3),
    H4 = ?MODULE:insert("content-type",
                        "application/x-www-form-urlencoded; charset=utf8",
                        H3),
    "application/x-www-form-urlencoded" = ?MODULE:get_primary_value(
                                             "content-type", H4),
    H4 = ?MODULE:delete_any("nonexistent-header", H4),
    H3 = ?MODULE:delete_any("content-type", H4),
    HB = <<"Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n">>,
    H_HB = ?MODULE:from_binary(HB),
    H_HB = ?MODULE:from_binary(binary_to_list(HB)),
    "47" = ?MODULE:get_value("Content-Length", H_HB),
    "text/plain" = ?MODULE:get_value("Content-Type", H_HB),
    L_H_HB = ?MODULE:to_list(H_HB),
    2 = length(L_H_HB),
    true = lists:member({'Content-Length', "47"}, L_H_HB),
    true = lists:member({'Content-Type', "text/plain"}, L_H_HB),
    HL = [ <<"Content-Length: 47\r\n">>, <<"Content-Type: text/plain\r\n">> ],
    HL2 = [ "Content-Length: 47\r\n", <<"Content-Type: text/plain\r\n">> ],
    HL3 = [ <<"Content-Length: 47\r\n">>, "Content-Type: text/plain\r\n" ],
    H_HL = ?MODULE:from_binary(HL),
    H_HL = ?MODULE:from_binary(HL2),
    H_HL = ?MODULE:from_binary(HL3),
    "47" = ?MODULE:get_value("Content-Length", H_HL),
    "text/plain" = ?MODULE:get_value("Content-Type", H_HL),
    L_H_HL = ?MODULE:to_list(H_HL),
    2 = length(L_H_HL),
    true = lists:member({'Content-Length', "47"}, L_H_HL),
    true = lists:member({'Content-Type', "text/plain"}, L_H_HL),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<>>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"\r\n">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"\r\n\r\n">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary("")),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<>>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"">>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"\r\n">>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"\r\n\r\n">>])),
    ok.

tokenize_header_value_test() ->
    ?assertEqual(["a quote in a \"quote\"."],
                 tokenize_header_value("\"a quote in a \\\"quote\\\".\"")),
    ?assertEqual(["abc"], tokenize_header_value("abc")),
    ?assertEqual(["abc", "def"], tokenize_header_value("abc def")),
    ?assertEqual(["abc", "def"], tokenize_header_value("abc , def")),
    ?assertEqual(["abc", "def"], tokenize_header_value(",abc ,, def,,")),
    ?assertEqual(["abc def"], tokenize_header_value("\"abc def\"      ")),
    ?assertEqual(["abc, def"], tokenize_header_value("\"abc, def\"")),
    ?assertEqual(["\\a\\$"], tokenize_header_value("\"\\a\\$\"")),
    ?assertEqual(["abc def", "foo, bar", "12345", ""],
                 tokenize_header_value("\"abc def\" \"foo, bar\" , 12345, \"\"")),
    ?assertEqual(undefined,
                 tokenize_header_value(undefined)),
    ?assertEqual(undefined,
                 tokenize_header_value("umatched quote\"")),
    ?assertEqual(undefined,
                 tokenize_header_value("\"unmatched quote")).

-endif.

