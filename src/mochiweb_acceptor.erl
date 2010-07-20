%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.

%% @doc MochiWeb acceptor.

-module(mochiweb_acceptor).
-author('bob@mochimedia.com').

-include("internal.hrl").

-export([start_link/3, init/3]).
-export([worker/3]).

start_link(Server, Listen, Loop) ->
    proc_lib:spawn_link(?MODULE, init, [Server, Listen, Loop]).

init(Server, Listen, Loop) ->
    case catch mochiweb_socket:accept(Listen) of
        {ok, Socket} ->
            spawn_call_loop(Loop, Socket);
        {error, closed} ->
            ok;
        {error, timeout} ->
            ok;
        {error, esslaccept} ->
            ok;
        Other ->
            error_logger:error_report(
              [{application, mochiweb},
               "Accept failed error",
               lists:flatten(io_lib:format("~p", [Other]))]),
            exit({error, accept_failed})
    end,
    init(Server, Listen, Loop).

spawn_call_loop(Loop, Socket) ->
    Pid = proc_lib:spawn(?MODULE, worker, [self(), Loop, Socket]),
    case mochiweb_socket:controlling_process(Socket, Pid) of
        ok ->
            Pid ! self();
        Other ->
            error_logger:error_report(
              [{application, mochiweb},
               "Accept failure while setting controlling process",
               lists:flatten(io_lib:format("~p", [Other]))]),
            exit({error, accept_worker_failed})
    end.

worker(Parent, Loop, Socket) ->
    receive
        Parent ->
            ok
    after 60000 ->
            exit(normal)
    end,
    call_loop(Loop, Socket).

call_loop({M, F}, Socket) ->
    M:F(Socket);
call_loop({M, F, A}, Socket) ->
    erlang:apply(M, F, [Socket | A]);
call_loop(Loop, Socket) ->
    Loop(Socket).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
