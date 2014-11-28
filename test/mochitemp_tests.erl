-module(mochitemp_tests).

-import(mochitemp, [gettempdir/2, gettempdir_identity/1, normalize_dir/1, gettempdir/2, 
	rngchars/1, mkdtemp/0, mkdtemp_n/2, rngchar/1, rmtempdir/1, gettempdir/0,
	make_dir/1, mkdtemp/3]).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pushenv(L) ->
    [{K, os:getenv(K)} || K <- L].
popenv(L) ->
    F = fun ({K, false}) ->
                %% Erlang doesn't have an unsetenv, wtf.
                os:putenv(K, "");
            ({K, V}) ->
                os:putenv(K, V)
        end,
    lists:foreach(F, L).

gettempdir_fallback_test() ->
    ?assertEqual(
       "/",
       gettempdir([{fun mochitemp:gettempdir_identity/1, ["/--not-here--/"]},
                   {fun mochitemp:gettempdir_identity/1, ["/"]}],
                  fun mochitemp:normalize_dir/1)),
    ?assertEqual(
       "/",
       %% simulate a true os:getenv unset env
       gettempdir([{fun mochitemp:gettempdir_identity/1, [false]},
                   {fun mochitemp:gettempdir_identity/1, ["/"]}],
                  fun mochitemp:normalize_dir/1)),
    ok.

gettempdir_identity_test() ->
    ?assertEqual(
       "/",
       gettempdir([{fun mochitemp:gettempdir_identity/1, ["/"]}], fun mochitemp:normalize_dir/1)),
    ok.

gettempdir_cwd_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertEqual(
       normalize_dir(Cwd),
       gettempdir([{fun mochitemp:gettempdir_cwd/1, [cwd]}], fun mochitemp:normalize_dir/1)),
    ok.

rngchars_test() ->
    crypto:start(),
    ?assertEqual(
       "",
       rngchars(0)),
    ?assertEqual(
       10,
       length(rngchars(10))),
    ok.

rngchar_test() ->
    ?assertEqual(
       $a,
       rngchar(0)),
    ?assertEqual(
       $A,
       rngchar(26)),
    ?assertEqual(
       $_,
       rngchar(62)),
    ok.

mkdtemp_n_failonce_test() ->
    crypto:start(),
    D = mkdtemp(),
    Path = filename:join([D, "testdir"]),
    %% Toggle the existence of a dir so that it fails
    %% the first time and succeeds the second.
    F = fun () ->
                case filelib:is_dir(Path) of
                    true ->
                        file:del_dir(Path);
                    false ->
                        file:make_dir(Path)
                end,
                Path
        end,
    try
        %% Fails the first time
        ?assertThrow(
           {error, eexist},
           mkdtemp_n(F, 1)),
        %% Reset state
        file:del_dir(Path),
        %% Succeeds the second time
        ?assertEqual(
           Path,
           mkdtemp_n(F, 2))
    after rmtempdir(D)
    end,
    ok.

mkdtemp_n_fail_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertThrow(
       {error, eexist},
       mkdtemp_n(fun () -> Cwd end, 1)),
    ?assertThrow(
       {error, eexist},
       mkdtemp_n(fun () -> Cwd end, 2)),
    ok.

make_dir_fail_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertThrow(
      {error, eexist},
      make_dir(Cwd)),
    ok.

mkdtemp_test() ->
    crypto:start(),
    D = mkdtemp(),
    ?assertEqual(
       true,
       filelib:is_dir(D)),
    ?assertEqual(
       ok,
       file:del_dir(D)),
    ok.

rmtempdir_test() ->
    crypto:start(),
    D1 = mkdtemp(),
    ?assertEqual(
       true,
       filelib:is_dir(D1)),
    ?assertEqual(
       ok,
       rmtempdir(D1)),
    D2 = mkdtemp(),
    ?assertEqual(
       true,
       filelib:is_dir(D2)),
    ok = file:write_file(filename:join([D2, "foo"]), <<"bytes">>),
    D3 = mkdtemp("suffix", "prefix", D2),
    ?assertEqual(
       true,
       filelib:is_dir(D3)),
    ok = file:write_file(filename:join([D3, "foo"]), <<"bytes">>),
    ?assertEqual(
       ok,
       rmtempdir(D2)),
    ?assertEqual(
       {error, enoent},
       file:consult(D3)),
    ?assertEqual(
       {error, enoent},
       file:consult(D2)),
    ok.

gettempdir_env_test() ->
    Env = pushenv(["TMPDIR", "TEMP", "TMP"]),
    FalseEnv = [{"TMPDIR", false}, {"TEMP", false}, {"TMP", false}],
    try
        popenv(FalseEnv),
        popenv([{"TMPDIR", "/"}]),
        ?assertEqual(
           "/",
           os:getenv("TMPDIR")),
        ?assertEqual(
           "/",
           gettempdir()),
        {ok, Cwd} = file:get_cwd(),
        popenv(FalseEnv),
        popenv([{"TMP", Cwd}]),
        ?assertEqual(
           normalize_dir(Cwd),
           gettempdir())
    after popenv(Env)
    end,
    ok.

-endif.
