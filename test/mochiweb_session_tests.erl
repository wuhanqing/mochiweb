-module(mochiweb_session_tests).

-import(mochiweb_session, [check_session_cookie/4, generate_session_data/4]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

generate_check_session_cookie_test_() ->
    {setup,
     fun setup_server_key/0,
     fun generate_check_session_cookie/1}.

setup_server_key() ->
    crypto:start(),
    ["adfasdfasfs",30000].

generate_check_session_cookie([ServerKey, TS]) ->
    Id = fun (A) -> A end,
    TSFuture = TS + 1000,
    TSPast = TS - 1,
    [?_assertEqual(
        {true, [TSFuture, "alice"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice", Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice and"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice and", Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice and"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice and", Id, ServerKey),
          TS, Id,ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice and bob"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice and bob",
                                Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice jlkjfkjsdfg sdkfjgldsjgl"]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice jlkjfkjsdfg sdkfjgldsjgl",
                                Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true, [TSFuture, "alice .'¡'ç+-$%/(&\""]},
        check_session_cookie(
          generate_session_data(TSFuture, "alice .'¡'ç+-$%/(&\""
                                ,Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertEqual(
        {true,[TSFuture,"alice456689875"]},
        check_session_cookie(
          generate_session_data(TSFuture, ["alice","456689875"],
                                Id, ServerKey),
          TS, Id, ServerKey)),
     ?_assertError(
        function_clause,
        check_session_cookie(
          generate_session_data(TSFuture, {tuple,one},
                                Id, ServerKey),
          TS, Id,ServerKey)),
     ?_assertEqual(
        {false, [TSPast, "bob"]},
        check_session_cookie(
          generate_session_data(TSPast, "bob", Id,ServerKey),
          TS, Id, ServerKey))
    ].

-endif.
