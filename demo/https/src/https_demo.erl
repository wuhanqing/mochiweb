
-module(https_demo).

-export([start/0, stop/0]).

-export([loop/1]).

start() ->
    ok = application:start(crypto),
    ok = application:start(esockd),
    mochiweb:start_http(8080, [{max_clients, 1024}, {acceptors, 2}], 
                        {?MODULE, loop, []}).

stop() ->
    mochiweb:stop_http(8080).

loop(Req) ->
    Req:ok("text/plain", "ok... it works!").


