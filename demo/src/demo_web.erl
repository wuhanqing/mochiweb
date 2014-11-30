%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for demo.

-module(demo_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/0, stop/0, loop/2]).

%% External API

start() ->
    DocRoot = demo_deps:local_path(["priv", "www"]),
	mochiweb_http:start(?MODULE, 8000, {?MODULE, loop, [DocRoot]}).

stop() ->
    mochiweb_http:stop(?MODULE, 8000).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
	io:format("PATH: ~p~n", [Path]),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
