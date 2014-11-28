-module(mochilists_tests).

-import(mochilists, [set_defaults/2, get_value/2, set_default/2, get_value/3]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

set_defaults_test() ->
    ?assertEqual(
       [{k, v}],
       set_defaults([{k, v}], [])),
    ?assertEqual(
       [{k, v}],
       set_defaults([{k, vee}], [{k, v}])),
    ?assertEqual(
       lists:sort([{kay, vee}, {k, v}]),
       lists:sort(set_defaults([{k, vee}, {kay, vee}], [{k, v}]))),
    ok.

set_default_test() ->
    ?assertEqual(
       [{k, v}],
       set_default({k, v}, [])),
    ?assertEqual(
       [{k, v}],
       set_default({k, vee}, [{k, v}])),
    ok.

get_value_test() ->
    ?assertEqual(
       undefined,
       get_value(foo, [])),
    ?assertEqual(
       undefined,
       get_value(foo, [{bar, baz}])),
    ?assertEqual(
       bar,
       get_value(foo, [{foo, bar}])),
    ?assertEqual(
       default,
       get_value(foo, [], default)),
    ?assertEqual(
       default,
       get_value(foo, [{bar, baz}], default)),
    ?assertEqual(
       bar,
       get_value(foo, [{foo, bar}], default)),
    ok.

-endif.

