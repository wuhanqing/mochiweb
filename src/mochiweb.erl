%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Start and stop the MochiWeb server.

-module(mochiweb).

-include("mochiweb.hrl").

-author('bob@mochimedia.com').

-export([start_http/2, start_http/3, 
         stop_http/1]).

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

start_http(Port, Loop) ->
    Callback = {mochiweb_http, start_link, [Loop]},
    esockd:open(http, Port, [{sockopts, ?SOCKET_OPTS}], Callback).

start_http(Port, Options, Loop) ->
    Callback = {mochiweb_http, start_link, [Loop]},
    SockOpts = merge_opts(?SOCKET_OPTS,
                          proplists:get_value(sockopts, Options, [])),
    esockd:open(http, Port, merge_opts(Options, [{sockopts, SockOpts}]), Callback).

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

stop_http(Port) when is_integer(Port) ->
    esockd:close(http,  Port).

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
new_request({Transport, Socket, {Method, HttpUri, Version}, Headers}) ->
    mochiweb_request:new(Transport,
                         Socket,
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

