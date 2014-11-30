-module(mochijson_tests).

-import(mochijson, [encoder/1, encode/1, decode/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% testing constructs borrowed from the Yaws JSON implementation.

%% Create an object from a list of Key/Value pairs.

obj_new() ->
    {struct, []}.

is_obj({struct, Props}) ->
    F = fun ({K, _}) when is_list(K) ->
                true;
            (_) ->
                false
        end,    
    lists:all(F, Props).

obj_from_list(Props) ->
    Obj = {struct, Props},
    case is_obj(Obj) of
        true -> Obj;
        false -> exit(json_bad_object)
    end.

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({struct, Props1}, {struct, Props2}) ->
    equiv_object(Props1, Props2);
equiv({array, L1}, {array, L2}) ->
    equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(S1, S2) when is_list(S1), is_list(S2)     -> S1 == S2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true.

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

e2j_vec_test() ->
    test_one(e2j_test_vec(utf8), 1).

issue33_test() ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=33
    Js = {struct, [{"key", [194, 163]}]},
    Encoder = encoder([{input_encoding, utf8}]),
    "{\"key\":\"\\u00a3\"}" = lists:flatten(Encoder(Js)).

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
    {3.1416, "3.14160"}, % text representation may truncate, trail zeroes
    {-1, "-1"},
    {-3.1416, "-3.14160"},
    {12.0e10, "1.20000e+11"},
    {1.234E+10, "1.23400e+10"},
    {-1.234E-10, "-1.23400e-10"},
    {10.0, "1.0e+01"},
    {123.456, "1.23456E+2"},
    {10.0, "1e1"},
    {"foo", "\"foo\""},
    {"foo" ++ [5] ++ "bar", "\"foo\\u0005bar\""},
    {"", "\"\""},
    {"\"", "\"\\\"\""},
    {"\n\n\n", "\"\\n\\n\\n\""},
    {"\\", "\"\\\\\""},
    {"\" \b\f\r\n\t\"", "\"\\\" \\b\\f\\r\\n\\t\\\"\""},
    {obj_new(), "{}"},
    {obj_from_list([{"foo", "bar"}]), "{\"foo\":\"bar\"}"},
    {obj_from_list([{"foo", "bar"}, {"baz", 123}]),
     "{\"foo\":\"bar\",\"baz\":123}"},
    {{array, []}, "[]"},
    {{array, [{array, []}]}, "[[]]"},
    {{array, [1, "foo"]}, "[1,\"foo\"]"},

    % json array in a json object
    {obj_from_list([{"foo", {array, [123]}}]),
     "{\"foo\":[123]}"},

    % json object in a json object
    {obj_from_list([{"foo", obj_from_list([{"bar", true}])}]),
     "{\"foo\":{\"bar\":true}}"},

    % fold evaluation order
    {obj_from_list([{"foo", {array, []}},
                     {"bar", obj_from_list([{"baz", true}])},
                     {"alice", "bob"}]),
     "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}"},

    % json object in a json array
    {{array, [-123, "foo", obj_from_list([{"bar", {array, []}}]), null]},
     "[-123,\"foo\",{\"bar\":[]},null]"}
    ].

-endif.
