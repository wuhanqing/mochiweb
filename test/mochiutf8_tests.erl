-module(mochiutf8_tests).

-import(mochiutf8, [binary_skip_bytes/2, invalid_utf8_indexes/1, codepoint_to_bytes/1, 
	valid_utf8_bytes/1, bytes_foldl/3, bytes_to_codepoints/1, codepoint_foldl/3, len/1, codepoints_to_bytes/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

binary_skip_bytes_test() ->
    ?assertEqual(<<"foo">>,
                 binary_skip_bytes(<<"foo">>, [])),
    ?assertEqual(<<"foobar">>,
                 binary_skip_bytes(<<"foo bar">>, [3])),
    ?assertEqual(<<"foo">>,
                 binary_skip_bytes(<<"foo bar">>, [3, 4, 5, 6])),
    ?assertEqual(<<"oo bar">>,
                 binary_skip_bytes(<<"foo bar">>, [0])),
    ok.

invalid_utf8_indexes_test() ->
    ?assertEqual(
       [],
       invalid_utf8_indexes(<<"unicode snowman for you: ", 226, 152, 131>>)),
    ?assertEqual(
       [0],
       invalid_utf8_indexes(<<128>>)),
    ?assertEqual(
       [57,59,60,64,66,67],
       invalid_utf8_indexes(<<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (",
                              167, 65, 170, 186, 73, 83, 80, 166, 87, 186, 217, 41, 41>>)),
    ok.

codepoint_to_bytes_test() ->
    %% U+0000 - U+007F - 7 bits
    %% U+0080 - U+07FF - 11 bits
    %% U+0800 - U+FFFF - 16 bits (excluding UTC-16 surrogate code points)
    %% U+10000 - U+10FFFF - 21 bits
    ?assertEqual(
       <<"a">>,
       codepoint_to_bytes($a)),
    ?assertEqual(
       <<16#c2, 16#80>>,
       codepoint_to_bytes(16#80)),
    ?assertEqual(
       <<16#df, 16#bf>>,
       codepoint_to_bytes(16#07ff)),
    ?assertEqual(
       <<16#ef, 16#bf, 16#bf>>,
       codepoint_to_bytes(16#ffff)),
    ?assertEqual(
       <<16#f4, 16#8f, 16#bf, 16#bf>>,
       codepoint_to_bytes(16#10ffff)),
    ok.

bytes_foldl_test() ->
    ?assertEqual(
       <<"abc">>,
       bytes_foldl(fun (B, Acc) -> <<Acc/binary, B/binary>> end, <<>>, <<"abc">>)),
    ?assertEqual(
       <<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>,
       bytes_foldl(fun (B, Acc) -> <<Acc/binary, B/binary>> end, <<>>,
                   <<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>)),
    ok.

bytes_to_codepoints_test() ->
    ?assertEqual(
       "abc" ++ [16#2603, 16#4e2d, 16#85, 16#10ffff],
       bytes_to_codepoints(<<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>)),
    ok.

codepoint_foldl_test() ->
    ?assertEqual(
       "cba",
       codepoint_foldl(fun (C, Acc) -> [C | Acc] end, [], <<"abc">>)),
    ?assertEqual(
       [16#10ffff, 16#85, 16#4e2d, 16#2603 | "cba"],
       codepoint_foldl(fun (C, Acc) -> [C | Acc] end, [],
                       <<"abc", 226, 152, 131, 228, 184, 173, 194, 133, 244,143,191,191>>)),
    ok.

len_test() ->
    ?assertEqual(
       29,
       len(<<"unicode snowman for you: ", 226, 152, 131, 228, 184, 173, 194, 133, 244, 143, 191, 191>>)),
    ok.

codepoints_to_bytes_test() ->
    ?assertEqual(
       iolist_to_binary(lists:map(fun mochiutf8:codepoint_to_bytes/1, lists:seq(1, 1000))),
       codepoints_to_bytes(lists:seq(1, 1000))),
    ok.

valid_utf8_bytes_test() ->
    ?assertEqual(
       <<"invalid U+11ffff: ">>,
       valid_utf8_bytes(<<"invalid U+11ffff: ", 244, 159, 191, 191>>)),
    ?assertEqual(
       <<"U+10ffff: ", 244, 143, 191, 191>>,
       valid_utf8_bytes(<<"U+10ffff: ", 244, 143, 191, 191>>)),
    ?assertEqual(
       <<"overlong 2-byte encoding (a): ">>,
       valid_utf8_bytes(<<"overlong 2-byte encoding (a): ", 2#11000001, 2#10100001>>)),
    ?assertEqual(
       <<"overlong 2-byte encoding (!): ">>,
       valid_utf8_bytes(<<"overlong 2-byte encoding (!): ", 2#11000000, 2#10100001>>)),
    ?assertEqual(
       <<"mu: ", 194, 181>>,
       valid_utf8_bytes(<<"mu: ", 194, 181>>)),
    ?assertEqual(
       <<"bad coding bytes: ">>,
       valid_utf8_bytes(<<"bad coding bytes: ", 2#10011111, 2#10111111, 2#11111111>>)),
    ?assertEqual(
       <<"low surrogate (unpaired): ">>,
       valid_utf8_bytes(<<"low surrogate (unpaired): ", 237, 176, 128>>)),
    ?assertEqual(
       <<"high surrogate (unpaired): ">>,
       valid_utf8_bytes(<<"high surrogate (unpaired): ", 237, 191, 191>>)),
    ?assertEqual(
       <<"unicode snowman for you: ", 226, 152, 131>>,
       valid_utf8_bytes(<<"unicode snowman for you: ", 226, 152, 131>>)),
    ?assertEqual(
       <<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (AISPW))">>,
       valid_utf8_bytes(<<"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; (",
                          167, 65, 170, 186, 73, 83, 80, 166, 87, 186, 217, 41, 41>>)),
    ok.

-endif.
