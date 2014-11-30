%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Start and stop the MochiWeb server.

-module(mochiweb).

-include("mochiweb.hrl").

-author('bob@mochimedia.com').

-export([start_http/3, stop_http/2).
-export([new_request/1, new_response/1]).
-export([ensure_started/1]).

-define(HTTP_SOCKOPTS, [
    binary,
    {reuseaddr, true},
    {packet, 0},
    %{backlog, Backlog},
    {recbuf, ?RECBUF_SIZE},
    {exit_on_close, false},
    {active, false},
    {nodelay, true}
]).

-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).

%% @spec start(Options) -> ServerRet
%%     Options = [option()]
%%     Option = {name, atom()} | {ip, string() | tuple()} | {backlog, integer()}
%%              | {nodelay, boolean()} | {acceptor_pool_size, integer()}
%%              | {ssl, boolean()} | {profile_fun, undefined | (Props) -> ok}
%%              | {link, false}
%% @doc Start a mochiweb server.
%%      profile_fun is used to profile accept timing.
%%      After each accept, if defined, profile_fun is called with a proplist of a subset of the mochiweb_socket_server state and timing information.
%%      The proplist is as follows: [{name, Name}, {port, Port}, {active_sockets, ActiveSockets}, {timing, Timing}].
%% @end

start_http(Name, Port, Loop) when is_atom(Name), is_integer(Port) ->
    Callback = {mochiweb_http, start_link, [Loop]},
    sockd:listen(Name, Port, ?HTTP_SOCKOPTS, Callback).

stop_http(Name, Port) when is_atom(Name), is_integer(Port) ->
    esockd:close(Name,  Port).

%% See the erlang:decode_packet/3 docs for the full type
-spec uri(HttpUri :: term()) -> string().
uri({abs_path, Uri}) ->
    Uri;
%% TODO:
%% This makes it hard to implement certain kinds of proxies with mochiweb,
%% perhaps a field could be added to the mochiweb_request record to preserve
%% this information in raw_path.
uri({absoluteURI, _Protocol, _Host, _Port, Uri}) ->
    Uri;
%% From http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
uri('*') ->
    "*";
%% Erlang decode_packet will return this for requests like `CONNECT host:port`
uri({scheme, Hostname, Port}) ->
    Hostname ++ ":" ++ Port;
uri(HttpString) when is_list(HttpString) ->
    HttpString.

%% @spec new_request({Socket, Request, Headers}) -> MochiWebRequest
%% @doc Return a mochiweb_request data structure.
new_request({Socket, {Method, HttpUri, Version}, Headers}) ->
    mochiweb_request:new(Socket,
                         Method,
                         uri(HttpUri),
                         Version,
                         mochiweb_headers:make(Headers)).

%% @spec new_response({Request, integer(), Headers}) -> MochiWebResponse
%% @doc Return a mochiweb_response data structure.
new_response({Request, Code, Headers}) ->
    mochiweb_response:new(Request,
                          Code,
                          mochiweb_headers:make(Headers)).

%% @spec ensure_started(App::atom()) -> ok
%% @doc Start the given App if it has not been started already.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
