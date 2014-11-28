-module(mochilogfile2_tests).

-import(mochilogfile2, [name/1, open/1, close/1, write/2]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

name_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "open_close_test.log"),
    H = open(FileName),
    ?assertEqual(
       FileName,
       name(H)),
    close(H),
    file:delete(FileName),
    file:del_dir(D),
    ok.

open_close_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "open_close_test.log"),
    OpenClose = fun () ->
                        H = open(FileName),
                        ?assertEqual(
                           true,
                           filelib:is_file(FileName)),
                        ok = close(H),
                        ?assertEqual(
                           {ok, <<>>},
                           file:read_file(FileName)),
                        ok
                end,
    OpenClose(),
    OpenClose(),
    file:delete(FileName),
    file:del_dir(D),
    ok.

write_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "write_test.log"),
    F = fun () ->
                H = open(FileName),
                write(H, "test line"),
                close(H),
                ok
        end,
    F(),
    ?assertEqual(
       {ok, <<"test line\n">>},
       file:read_file(FileName)),
    F(),
    ?assertEqual(
       {ok, <<"test line\ntest line\n">>},
       file:read_file(FileName)),
    file:delete(FileName),
    file:del_dir(D),
    ok.

fix_log_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "write_test.log"),
    file:write_file(FileName, <<"first line good\nsecond line bad">>),
    F = fun () ->
                H = open(FileName),
                write(H, "test line"),
                close(H),
                ok
        end,
    F(),
    ?assertEqual(
       {ok, <<"first line good\ntest line\n">>},
       file:read_file(FileName)),
    file:write_file(FileName, <<"first line bad">>),
    F(),
    ?assertEqual(
       {ok, <<"test line\n">>},
       file:read_file(FileName)),
    F(),
    ?assertEqual(
       {ok, <<"test line\ntest line\n">>},
       file:read_file(FileName)),
    ok.

-endif.
