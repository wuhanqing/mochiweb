-module(mochiglobal_tests).

-import(mochiglobal, [delete/1, get/2]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_put_delete_test() ->
    K = '$$test$$mochiglobal',
    delete(K),
    ?assertEqual(
       bar,
       get(K, bar)),
    try
        ?MODULE:put(K, baz),
        ?assertEqual(
           baz,
           get(K, bar)),
        ?MODULE:put(K, wibble),
        ?assertEqual(
           wibble,
           ?MODULE:get(K))
    after
        delete(K)
    end,
    ?assertEqual(
       bar,
       get(K, bar)),
    ?assertEqual(
       undefined,
       ?MODULE:get(K)),
    ok.

-endif.
