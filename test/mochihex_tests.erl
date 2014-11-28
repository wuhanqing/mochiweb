-module(mochihex_tests).

-import(mochihex, [to_hex/1, to_int/1, to_bin/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_hex_test() ->
    "ff000ff1" = to_hex([255, 0, 15, 241]),
    "ff000ff1" = to_hex(16#ff000ff1),
    "0" = to_hex(16#0),
    ok.

to_bin_test() ->
    <<255, 0, 15, 241>> = to_bin("ff000ff1"),
    <<255, 0, 10, 161>> = to_bin("Ff000aA1"),
    ok.

to_int_test() ->
    16#ff000ff1 = to_int("ff000ff1"),
    16#ff000aa1 = to_int("FF000Aa1"),
    16#0 = to_int("0"),
    ok.

-endif.
