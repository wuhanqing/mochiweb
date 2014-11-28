
-module(mochijson2_tests).

-import(mochijson2, [decode/1, decode/2, decoder/1, encode/1, encoder/1,
	json_string_is_safe/1, json_bin_is_safe/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% This is a macro to placate syntax highlighters..
-define(Q, $\").
-define(ADV_COL(S, N), S#decoder{offset=N+S#decoder.offset,
                                 column=N+S#decoder.column}).
-define(INC_COL(S), S#decoder{offset=1+S#decoder.offset,
                              column=1+S#decoder.column}).
-define(INC_LINE(S), S#decoder{offset=1+S#decoder.offset,
                               column=1,
                               line=1+S#decoder.line}).
-define(INC_CHAR(S, C),
        case C of
            $\n ->
                S#decoder{column=1,
                          line=1+S#decoder.line,
                          offset=1+S#decoder.offset};
            _ ->
                S#decoder{column=1+S#decoder.column,
                          offset=1+S#decoder.offset}
        end).
-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% testing constructs borrowed from the Yaws JSON implementation.

%% Create an object from a list of Key/Value pairs.

obj_new() ->
    {struct, []}.

is_obj({struct, Props}) ->
    F = fun ({K, _}) when is_binary(K) -> true end,
    lists:all(F, Props).

obj_from_list(Props) ->
    Obj = {struct, Props},
    ?assert(is_obj(Obj)),
    Obj.

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({struct, Props1}, {struct, Props2}) ->
    equiv_object(Props1, Props2);
equiv(L1, L2) when is_list(L1), is_list(L2) ->
    equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(B1, B2) when is_binary(B1), is_binary(B2) -> B1 == B2;
equiv(A, A) when A =:= true orelse A =:= false orelse A =:= null -> true.

%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) ->
                             equiv(K1, K2) and equiv(V1, V2)
                     end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) ->
    true;
equiv_list([V1 | L1], [V2 | L2]) ->
    equiv(V1, V2) andalso equiv_list(L1, L2).

decode_test() ->
    [1199344435545.0, 1] = decode(<<"[1199344435545.0,1]">>),
    <<16#F0,16#9D,16#9C,16#95>> = decode([34,"\\ud835","\\udf15",34]).

e2j_vec_test() ->
    test_one(e2j_test_vec(utf8), 1).

test_one([], _N) ->
    %% io:format("~p tests passed~n", [N-1]),
    ok;
test_one([{E, J} | Rest], N) ->
    %% io:format("[~p] ~p ~p~n", [N, E, J]),
    true = equiv(E, decode(J)),
    true = equiv(E, decode(encode(E))),
    test_one(Rest, 1+N).

e2j_test_vec(utf8) ->
    [
     {1, "1"},
     {3.1416, "3.14160"}, %% text representation may truncate, trail zeroes
     {-1, "-1"},
     {-3.1416, "-3.14160"},
     {12.0e10, "1.20000e+11"},
     {1.234E+10, "1.23400e+10"},
     {-1.234E-10, "-1.23400e-10"},
     {10.0, "1.0e+01"},
     {123.456, "1.23456E+2"},
     {10.0, "1e1"},
     {<<"foo">>, "\"foo\""},
     {<<"foo", 5, "bar">>, "\"foo\\u0005bar\""},
     {<<"">>, "\"\""},
     {<<"\n\n\n">>, "\"\\n\\n\\n\""},
     {<<"\" \b\f\r\n\t\"">>, "\"\\\" \\b\\f\\r\\n\\t\\\"\""},
     {obj_new(), "{}"},
     {obj_from_list([{<<"foo">>, <<"bar">>}]), "{\"foo\":\"bar\"}"},
     {obj_from_list([{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]),
      "{\"foo\":\"bar\",\"baz\":123}"},
     {[], "[]"},
     {[[]], "[[]]"},
     {[1, <<"foo">>], "[1,\"foo\"]"},

     %% json array in a json object
     {obj_from_list([{<<"foo">>, [123]}]),
      "{\"foo\":[123]}"},

     %% json object in a json object
     {obj_from_list([{<<"foo">>, obj_from_list([{<<"bar">>, true}])}]),
      "{\"foo\":{\"bar\":true}}"},

     %% fold evaluation order
     {obj_from_list([{<<"foo">>, []},
                     {<<"bar">>, obj_from_list([{<<"baz">>, true}])},
                     {<<"alice">>, <<"bob">>}]),
      "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}"},

     %% json object in a json array
     {[-123, <<"foo">>, obj_from_list([{<<"bar">>, []}]), null],
      "[-123,\"foo\",{\"bar\":[]},null]"}
    ].

%% test utf8 encoding
encoder_utf8_test() ->
    %% safe conversion case (default)
    [34,"\\u0001","\\u0442","\\u0435","\\u0441","\\u0442",34] =
        encode(<<1,"\321\202\320\265\321\201\321\202">>),

    %% raw utf8 output (optional)
    Enc = mochijson2:encoder([{utf8, true}]),
    [34,"\\u0001",[209,130],[208,181],[209,129],[209,130],34] =
        Enc(<<1,"\321\202\320\265\321\201\321\202">>).

input_validation_test() ->
    Good = [
        {16#00A3, <<?Q, 16#C2, 16#A3, ?Q>>}, %% pound
        {16#20AC, <<?Q, 16#E2, 16#82, 16#AC, ?Q>>}, %% euro
        {16#10196, <<?Q, 16#F0, 16#90, 16#86, 16#96, ?Q>>} %% denarius
    ],
    lists:foreach(fun({CodePoint, UTF8}) ->
        Expect = list_to_binary(xmerl_ucs:to_utf8(CodePoint)),
        Expect = decode(UTF8)
    end, Good),

    Bad = [
        %% 2nd, 3rd, or 4th byte of a multi-byte sequence w/o leading byte
        <<?Q, 16#80, ?Q>>,
        %% missing continuations, last byte in each should be 80-BF
        <<?Q, 16#C2, 16#7F, ?Q>>,
        <<?Q, 16#E0, 16#80,16#7F, ?Q>>,
        <<?Q, 16#F0, 16#80, 16#80, 16#7F, ?Q>>,
        %% we don't support code points > 10FFFF per RFC 3629
        <<?Q, 16#F5, 16#80, 16#80, 16#80, ?Q>>,
        %% escape characters trigger a different code path
        <<?Q, $\\, $\n, 16#80, ?Q>>
    ],
    lists:foreach(
      fun(X) ->
              ok = try decode(X) catch invalid_utf8 -> ok end,
              %% could be {ucs,{bad_utf8_character_code}} or
              %%          {json_encode,{bad_char,_}}
              {'EXIT', _} = (catch encode(X))
      end, Bad).

inline_json_test() ->
    ?assertEqual(<<"\"iodata iodata\"">>,
                 iolist_to_binary(
                   encode({json, [<<"\"iodata">>, " iodata\""]}))),
    ?assertEqual({struct, [{<<"key">>, <<"iodata iodata">>}]},
                 decode(
                   encode({struct,
                           [{key, {json, [<<"\"iodata">>, " iodata\""]}}]}))),
    ok.

big_unicode_test() ->
    UTF8Seq = list_to_binary(xmerl_ucs:to_utf8(16#0001d120)),
    ?assertEqual(
       <<"\"\\ud834\\udd20\"">>,
       iolist_to_binary(encode(UTF8Seq))),
    ?assertEqual(
       UTF8Seq,
       decode(iolist_to_binary(encode(UTF8Seq)))),
    ok.

custom_decoder_test() ->
    ?assertEqual(
       {struct, [{<<"key">>, <<"value">>}]},
       (decoder([]))("{\"key\": \"value\"}")),
    F = fun ({struct, [{<<"key">>, <<"value">>}]}) -> win end,
    ?assertEqual(
       win,
       (decoder([{object_hook, F}]))("{\"key\": \"value\"}")),
    ok.

atom_test() ->
    %% JSON native atoms
    [begin
         ?assertEqual(A, decode(atom_to_list(A))),
         ?assertEqual(iolist_to_binary(atom_to_list(A)),
                      iolist_to_binary(encode(A)))
     end || A <- [true, false, null]],
    %% Atom to string
    ?assertEqual(
       <<"\"foo\"">>,
       iolist_to_binary(encode(foo))),
    ?assertEqual(
       <<"\"\\ud834\\udd20\"">>,
       iolist_to_binary(encode(list_to_atom(xmerl_ucs:to_utf8(16#0001d120))))),
    ok.

key_encode_test() ->
    %% Some forms are accepted as keys that would not be strings in other
    %% cases
    ?assertEqual(
       <<"{\"foo\":1}">>,
       iolist_to_binary(encode({struct, [{foo, 1}]}))),
    ?assertEqual(
       <<"{\"foo\":1}">>,
       iolist_to_binary(encode({struct, [{<<"foo">>, 1}]}))),
    ?assertEqual(
       <<"{\"foo\":1}">>,
       iolist_to_binary(encode({struct, [{"foo", 1}]}))),
	?assertEqual(
       <<"{\"foo\":1}">>,
       iolist_to_binary(encode([{foo, 1}]))),
    ?assertEqual(
       <<"{\"foo\":1}">>,
       iolist_to_binary(encode([{<<"foo">>, 1}]))),
    ?assertEqual(
       <<"{\"foo\":1}">>,
       iolist_to_binary(encode([{"foo", 1}]))),
    ?assertEqual(
       <<"{\"\\ud834\\udd20\":1}">>,
       iolist_to_binary(
         encode({struct, [{[16#0001d120], 1}]}))),
    ?assertEqual(
       <<"{\"1\":1}">>,
       iolist_to_binary(encode({struct, [{1, 1}]}))),
    ok.

unsafe_chars_test() ->
    Chars = "\"\\\b\f\n\r\t",
    [begin
         ?assertEqual(false, json_string_is_safe([C])),
         ?assertEqual(false, json_bin_is_safe(<<C>>)),
         ?assertEqual(<<C>>, decode(encode(<<C>>)))
     end || C <- Chars],
    ?assertEqual(
       false,
       json_string_is_safe([16#0001d120])),
    ?assertEqual(
       false,
       json_bin_is_safe(list_to_binary(xmerl_ucs:to_utf8(16#0001d120)))),
    ?assertEqual(
       [16#0001d120],
       xmerl_ucs:from_utf8(
         binary_to_list(
           decode(encode(list_to_atom(xmerl_ucs:to_utf8(16#0001d120))))))),
    ?assertEqual(
       false,
       json_string_is_safe([16#110000])),
    ?assertEqual(
       false,
       json_bin_is_safe(list_to_binary(xmerl_ucs:to_utf8([16#110000])))),
    %% solidus can be escaped but isn't unsafe by default
    ?assertEqual(
       <<"/">>,
       decode(<<"\"\\/\"">>)),
    ok.

int_test() ->
    ?assertEqual(0, decode("0")),
    ?assertEqual(1, decode("1")),
    ?assertEqual(11, decode("11")),
    ok.

large_int_test() ->
    ?assertEqual(<<"-2147483649214748364921474836492147483649">>,
        iolist_to_binary(encode(-2147483649214748364921474836492147483649))),
    ?assertEqual(<<"2147483649214748364921474836492147483649">>,
        iolist_to_binary(encode(2147483649214748364921474836492147483649))),
    ok.

float_test() ->
    ?assertEqual(<<"-2147483649.0">>, iolist_to_binary(encode(-2147483649.0))),
    ?assertEqual(<<"2147483648.0">>, iolist_to_binary(encode(2147483648.0))),
    ok.

handler_test() ->
    ?assertEqual(
       {'EXIT',{json_encode,{bad_term,{x,y}}}},
       catch encode({x,y})),
    F = fun ({x,y}) -> [] end,
    ?assertEqual(
       <<"[]">>,
       iolist_to_binary((encoder([{handler, F}]))({x, y}))),
    ok.

encode_empty_test_() ->
    [{A, ?_assertEqual(<<"{}">>, iolist_to_binary(encode(B)))}
     || {A, B} <- [{"eep18 {}", {}},
                   {"eep18 {[]}", {[]}},
                   {"{struct, []}", {struct, []}}]].

encode_test_() ->
    P = [{<<"k">>, <<"v">>}],
    JSON = iolist_to_binary(encode({struct, P})),
    [{atom_to_list(F),
      ?_assertEqual(JSON, iolist_to_binary(encode(decode(JSON, [{format, F}]))))}
     || F <- [struct, eep18, proplist]].

format_test_() ->
    P = [{<<"k">>, <<"v">>}],
    JSON = iolist_to_binary(encode({struct, P})),
    [{atom_to_list(F),
      ?_assertEqual(A, decode(JSON, [{format, F}]))}
     || {F, A} <- [{struct, {struct, P}},
                   {eep18, {P}},
                   {proplist, P}]].

-endif.
