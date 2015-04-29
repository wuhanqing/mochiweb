
-module(https_demo).

-export([start/0, stop/0]).

-export([loop/1]).

start() ->
    [ok = application:start(App) || App <- [asn1, crypto, public_key, ssl, esockd]],
    Options = [{max_clients, 1024},
               {acceptors, 2},
               {ssl, [{certfile, "etc/server_cert.pem"},
                      {keyfile,  "etc/server_key.pem"}]}],
    mochiweb:start_http(8443, Options, {?MODULE, loop, []}).

stop() ->
    mochiweb:stop_http(8080).

loop(Req) ->
    Req:ok({"text/plain", "ok... it works!"}).


