-module(hello).

-export([start/1, loop/1]).

-define(LOOP, {?MODULE, loop}).

start(Port) ->
    mochiweb_http:start([{name, ?MODULE}, {port, Port}, {loop, ?LOOP}]).

loop(Req) ->
    io:format("~s ~s~n", [Req:method(), Req:get(path)]),
    Req:ok("text/plain", [], "Hello...").
