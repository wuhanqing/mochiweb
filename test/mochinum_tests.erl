-module(mochinum_tests).

-import(mochinum, [int_ceil/1, int_pow/1, int_pow/2, digits/1, frexp/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

int_ceil_test() ->
    ?assertEqual(1, int_ceil(0.0001)),
    ?assertEqual(0, int_ceil(0.0)),
    ?assertEqual(1, int_ceil(0.99)),
    ?assertEqual(1, int_ceil(1.0)),
    ?assertEqual(-1, int_ceil(-1.5)),
    ?assertEqual(-2, int_ceil(-2.0)),
    ok.

int_pow_test() ->
    ?assertEqual(1, int_pow(1, 1)),
    ?assertEqual(1, int_pow(1, 0)),
    ?assertEqual(1, int_pow(10, 0)),
    ?assertEqual(10, int_pow(10, 1)),
    ?assertEqual(100, int_pow(10, 2)),
    ?assertEqual(1000, int_pow(10, 3)),
    ok.

digits_test() ->
    ?assertEqual("0",
                 digits(0)),
    ?assertEqual("0.0",
                 digits(0.0)),
    ?assertEqual("1.0",
                 digits(1.0)),
    ?assertEqual("-1.0",
                 digits(-1.0)),
    ?assertEqual("0.1",
                 digits(0.1)),
    ?assertEqual("0.01",
                 digits(0.01)),
    ?assertEqual("0.001",
                 digits(0.001)),
    ?assertEqual("1.0e+6",
                 digits(1000000.0)),
    ?assertEqual("0.5",
                 digits(0.5)),
    ?assertEqual("4503599627370496.0",
                 digits(4503599627370496.0)),
    %% small denormalized number
    %% 4.94065645841246544177e-324 =:= 5.0e-324
    <<SmallDenorm/float>> = <<0,0,0,0,0,0,0,1>>,
    ?assertEqual("5.0e-324",
                 digits(SmallDenorm)),
    ?assertEqual(SmallDenorm,
                 list_to_float(digits(SmallDenorm))),
    %% large denormalized number
    %% 2.22507385850720088902e-308
    <<BigDenorm/float>> = <<0,15,255,255,255,255,255,255>>,
    ?assertEqual("2.225073858507201e-308",
                 digits(BigDenorm)),
    ?assertEqual(BigDenorm,
                 list_to_float(digits(BigDenorm))),
    %% small normalized number
    %% 2.22507385850720138309e-308
    <<SmallNorm/float>> = <<0,16,0,0,0,0,0,0>>,
    ?assertEqual("2.2250738585072014e-308",
                 digits(SmallNorm)),
    ?assertEqual(SmallNorm,
                 list_to_float(digits(SmallNorm))),
    %% large normalized number
    %% 1.79769313486231570815e+308
    <<LargeNorm/float>> = <<127,239,255,255,255,255,255,255>>,
    ?assertEqual("1.7976931348623157e+308",
                 digits(LargeNorm)),
    ?assertEqual(LargeNorm,
                 list_to_float(digits(LargeNorm))),
    %% issue #10 - mochinum:frexp(math:pow(2, -1074)).
    ?assertEqual("5.0e-324",
                 digits(math:pow(2, -1074))),
    ok.

frexp_test() ->
    %% zero
    ?assertEqual({0.0, 0}, frexp(0.0)),
    %% one
    ?assertEqual({0.5, 1}, frexp(1.0)),
    %% negative one
    ?assertEqual({-0.5, 1}, frexp(-1.0)),
    %% small denormalized number
    %% 4.94065645841246544177e-324
    <<SmallDenorm/float>> = <<0,0,0,0,0,0,0,1>>,
    ?assertEqual({0.5, -1073}, frexp(SmallDenorm)),
    %% large denormalized number
    %% 2.22507385850720088902e-308
    <<BigDenorm/float>> = <<0,15,255,255,255,255,255,255>>,
    ?assertEqual(
       {0.99999999999999978, -1022},
       frexp(BigDenorm)),
    %% small normalized number
    %% 2.22507385850720138309e-308
    <<SmallNorm/float>> = <<0,16,0,0,0,0,0,0>>,
    ?assertEqual({0.5, -1021}, frexp(SmallNorm)),
    %% large normalized number
    %% 1.79769313486231570815e+308
    <<LargeNorm/float>> = <<127,239,255,255,255,255,255,255>>,
    ?assertEqual(
        {0.99999999999999989, 1024},
        frexp(LargeNorm)),
    %% issue #10 - mochinum:frexp(math:pow(2, -1074)).
    ?assertEqual(
       {0.5, -1073},
       frexp(math:pow(2, -1074))),
    ok.

-endif.
