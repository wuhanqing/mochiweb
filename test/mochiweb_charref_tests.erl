-module(mochiweb_charref_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

exhaustive_entity_test() ->
    T = mochiweb_cover:clause_lookup_table(?MODULE, entity),
    [?assertEqual(V, entity(K)) || {K, V} <- T].

charref_test() ->
    1234 = charref("#1234"),
    255 = charref("#xfF"),
    255 = charref(<<"#XFf">>),
    38 = charref("amp"),
    38 = charref(<<"amp">>),
    undefined = charref("not_an_entity"),
    undefined = charref("#not_an_entity"),
    undefined = charref("#xnot_an_entity"),
    ok.

-endif.
