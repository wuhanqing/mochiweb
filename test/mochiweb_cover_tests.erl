-module(mochiweb_cover_tests).

-import(mochiweb_cover, [clause_lookup_table/2]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

foo_table(a) -> b;
foo_table("a") -> <<"b">>;
foo_table(123) -> {4, 3, 2};
foo_table([list]) -> [];
foo_table([list1, list2]) -> [list1, list2, list3];
foo_table(ignored) -> some, code, ignored;
foo_table(Var) -> Var.

foo_table_test() ->
    T = clause_lookup_table(?MODULE, foo_table),
    [?assertEqual(V, foo_table(K)) || {K, V} <- T].

clause_lookup_table_test() ->
    ?assertEqual(b, foo_table(a)),
    ?assertEqual(ignored, foo_table(ignored)),
    ?assertEqual('Var', foo_table('Var')),
    ?assertEqual(
       [{a, b},
        {"a", <<"b">>},
        {123, {4, 3, 2}},
        {[list], []},
        {[list1, list2], [list1, list2, list3]}],
       clause_lookup_table(?MODULE, foo_table)).

-endif.
