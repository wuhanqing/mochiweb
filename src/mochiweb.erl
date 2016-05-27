%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Start and stop the MochiWeb server.

-module(mochiweb).

-include("mochiweb.hrl").

-author('bob@mochimedia.com').

-export([start_http/2, start_http/3, start_http/4, stop_http/1, stop_http/2]).

-export([new_request/1, new_response/1]).

-define(SOCKET_OPTS, [
    binary,
    {reuseaddr, true},
    {packet, raw},
    {backlog, 1024}, %%128?
    {recbuf, ?RECBUF_SIZE},
    {exit_on_close, false},
    {nodelay, false}
]).

%% @doc Start HTTP Listener
-spec(start_http(esockd:listen_on(), esockd:mfargs()) -> {ok, pid()}).
start_http(ListenOn, MFArgs) ->
    start_http(ListenOn, [], MFArgs).

-spec(start_http(esockd:listen_on(), [esockd:option()], esockd:mfargs()) -> {ok, pid()}).
start_http(ListenOn, Options, MFArgs) ->
    start_http(http, ListenOn, Options, MFArgs).

-spec(start_http(atom(), esockd:listen_on(), [esockd:option()], esockd:mfargs()) -> {ok, pid()}).
start_http(Proto, ListenOn, Options, MFArgs) when is_atom(Proto) ->
    SockOpts = merge_opts(?SOCKET_OPTS,
                          proplists:get_value(sockopts, Options, [])),
    esockd:open(Proto, ListenOn, merge_opts(Options, [{sockopts, SockOpts}]),
                {mochiweb_http, start_link, [MFArgs]}).

%% @doc Stop HTTP Listener
-spec(stop_http(esockd:listen_on()) -> ok).
stop_http(ListenOn) -> stop_http(http, ListenOn).

-spec(stop_http(atom(), esockd:listen_on()) -> ok).
stop_http(Proto, ListenOn) -> esockd:close(Proto, ListenOn).

%% @private
merge_opts(Defaults, Options) ->
    lists:foldl(
        fun({Opt, Val}, Acc) ->
                case lists:keymember(Opt, 1, Acc) of
                    true ->
                        lists:keyreplace(Opt, 1, Acc, {Opt, Val});
                    false ->
                        [{Opt, Val}|Acc]
                end;
            (Opt, Acc) ->
                case lists:member(Opt, Acc) of
                    true -> Acc;
                    false -> [Opt | Acc]
                end
        end, Defaults, Options).

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

%% @spec new_request({Conn, Request, Headers}) -> MochiWebRequest
%% @doc Return a mochiweb_request data structure.
new_request({Conn, {Method, HttpUri, Version}, Headers}) ->
    mochiweb_request:new(Conn,
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

