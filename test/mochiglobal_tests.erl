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
        mochiglobal:put(K, baz),
        ?assertEqual(
           baz,
           get(K, bar)),
        mochiglobal:put(K, wibble),
        ?assertEqual(
           wibble,
           mochiglobal:get(K))
    after
        delete(K)
    end,
    ?assertEqual(
       bar,
       get(K, bar)),
    ?assertEqual(
       undefined,
       mochiglobal:get(K)),
    ok.

-endif.
