-module(mochiweb_mime_tests).

-import(mochiweb_mime, [from_extension/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

exhaustive_from_extension_test() ->
    T = mochiweb_cover:clause_lookup_table(mochiweb_mime, from_extension),
    [?assertEqual(V, from_extension(K)) || {K, V} <- T].

from_extension_test() ->
    ?assertEqual("text/html",
                 from_extension(".html")),
    ?assertEqual(undefined,
                 from_extension("")),
    ?assertEqual(undefined,
                 from_extension(".wtf")),
    ok.

-endif.
