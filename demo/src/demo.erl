%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc demo.

-module(demo).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the demo server.
start() ->
    demo_deps:ensure(),
    ensure_started(crypto),
    ensure_started(esockd),
    application:start(demo).


%% @spec stop() -> ok
%% @doc Stop the demo server.
stop() ->
    application:stop(demo).
