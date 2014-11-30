
-module(demo).
-export([start/0]).

%% @spec start() -> ok
%% @doc Start the demo server.
start() ->
    ok = ensure_started(crypto),
    ok = ensure_started(esockd),
    demo_web:start().

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
