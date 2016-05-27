
-module(websocket_demo).

-export([start/1, loop/2, stop/1]).

-export([ws_loop/3, broadcast_server/1]).

%%
%% Mochiweb websocket example
%%
%% [1]: At first you have to start HTTP server which will listen for HTTP
%%      requests and eventually upgrade connection to websocket
%% [2]: Attempt to upgrade connection to websocket.
%%      Function mochiweb_websocket:upgrade_connection/2:
%%      * first argument is mochiweb_request
%%      * second is M:F which will handle further websocket messages.
%%      Function return two funs:
%%      * ReentryWs/1 - use it to enter to messages handling loop
%%        (in this example ws_loop/3)
%%      * ReplyChannel/1 - use to send messages to client. May be passed to
%%        other processes
%% [3]: Example of sending message to client
%% [4]: State that will be passed to message handling loop
%% [5]: Pass control to messages handling loop. From this moment each message
%%      received from client can be handled...
%% [6]: ...here as Payload. State is variable intended for holding your custom
%%      state. ReplyChannel is the same function as in [3].
%%      Notice! Payload is list of messages received from client. Websocket
%%      framing mechanism concatenates messages which are sent one after another
%%      in short time.
%% [7]: Print payload received from client and send it back
%% [8]: Message handling function must return new state value

start([ssl]) ->
    [ok = application:start(App) || App <- [asn1, crypto, public_key, ssl, gen_logger, esockd]],
    Options = [{max_clients, 1024},
               {acceptors, 2},
               {ssl, [{certfile, "etc/server_cert.pem"},
                      {keyfile,  "etc/server_key.pem"}]}],
    Broadcaster = spawn_link(?MODULE, broadcast_server, [dict:new()]),
    mochiweb:start_http(8443, Options, {?MODULE, loop, [Broadcaster]});

start(_) ->
    ok = application:start(crypto), ok = esockd:start(),
    Broadcaster = spawn_link(?MODULE, broadcast_server, [dict:new()]),
    mochiweb:start_http(8080, [{max_clients, 1024}, {acceptors, 2}],
                        {?MODULE, loop, [Broadcaster]}).

stop(ssl) ->
    mochiweb:stop_http(8443);

stop(_) ->
    mochiweb:stop_http(8080).

loop(Req, Broadcaster) ->
    "/" ++ Path = Req:get(path),
	io:format("PATH: ~p~n", [Path]),
    H = mochiweb_request:get_header_value("Upgrade", Req),
    loop(Req,
         Broadcaster,
         H =/= undefined andalso string:to_lower(H) =:= "websocket").

loop(Req, _Broadcaster, false) ->
    mochiweb_request:serve_file("priv/index.html", "./", Req);
loop(Req, Broadcaster, true) ->
    {ReentryWs, ReplyChannel} = mochiweb_websocket:upgrade_connection(
                                  Req, fun ?MODULE:ws_loop/3),
    %% [3]
    Broadcaster ! {register, self(), ReplyChannel},
    %% [4]
    %% [5]
    ReentryWs(Broadcaster).

ws_loop(Payload, Broadcaster, _ReplyChannel) ->
    %% [6]

    %% [7]
    io:format("Received data: ~p~n", [Payload]),
    Received = list_to_binary(Payload),
    Broadcaster ! {broadcast, self(), Received},

    %% [8]
    Broadcaster.

%% This server keeps track of connected pids
broadcast_server(Pids) ->
    Pids1 = receive
                {register, Pid, Channel} ->
                    broadcast_register(Pid, Channel, Pids);
                {broadcast, Pid, Message} ->
                    broadcast_sendall(Pid, Message, Pids);
                {'DOWN', MRef, process, Pid, _Reason} ->
                    broadcast_down(Pid, MRef, Pids);
                Msg ->
                    io:format("Unknown message: ~p~n", [Msg]),
                    Pids
            end,
    erlang:hibernate(?MODULE, broadcast_server, [Pids1]).

broadcast_register(Pid, Channel, Pids) ->
    MRef = erlang:monitor(process, Pid),
    broadcast_sendall(
      Pid, "connected", dict:store(Pid, {Channel, MRef}, Pids)).

broadcast_down(Pid, MRef, Pids) ->
    Pids1 = case dict:find(Pid, Pids) of
                {ok, {_, MRef}} ->
                    dict:erase(Pid, Pids);
                _ ->
                    Pids
            end,
    broadcast_sendall(Pid, "disconnected", Pids1).

broadcast_sendall(Pid, Msg, Pids) ->
    M = iolist_to_binary([pid_to_list(Pid), ": ", Msg]),
    dict:fold(
      fun (K, {Reply, MRef}, Acc) ->
              try
                  begin
                      Reply({text, M}),
                      dict:store(K, {Reply, MRef}, Acc)
                  end
              catch
                  _:_ ->
                      Acc
              end
      end,
      dict:new(),
      Pids).

%docroot() ->
%    {file, Here} = code:is_loaded(?MODULE),
%    Dir = filename:dirname(filename:dirname(Here)),
%    filename:join([Dir, "priv", "www"]).


