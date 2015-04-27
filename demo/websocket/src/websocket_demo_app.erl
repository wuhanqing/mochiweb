
-module(websocket_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = websocket_demo_sup:start_link(),
    Broadcaster = spawn_link(websocket_demo, broadcast_server, [dict:new()]),
    mochiweb:start_http(8080, [{max_clients, 1024}, {acceptors, 2}],
                        {websocket_demo, loop, [Broadcaster]}),
    {ok, Sup}.

stop(_State) ->
    mochiweb:stop_http(8080),
    ok.

