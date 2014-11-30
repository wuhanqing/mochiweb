%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.

%% @doc Write newline delimited log files, ensuring that if a truncated
%%      entry is found on log open then it is fixed before writing. Uses
%%      delayed writes and raw files for performance.
-module(mochilogfile2).
-author('bob@mochimedia.com').

-export([open/1, write/2, close/1, name/1]).

%% @spec open(Name) -> Handle
%% @doc Open the log file Name, creating or appending as necessary. All data
%%      at the end of the file will be truncated until a newline is found, to
%%      ensure that all records are complete.
open(Name) ->
    {ok, FD} = file:open(Name, [raw, read, write, delayed_write, binary]),
    fix_log(FD),
    {?MODULE, Name, FD}.

%% @spec name(Handle) -> string()
%% @doc Return the path of the log file.
name({?MODULE, Name, _FD}) ->
    Name.

%% @spec write(Handle, IoData) -> ok
%% @doc Write IoData to the log file referenced by Handle.
write({?MODULE, _Name, FD}, IoData) ->
    ok = file:write(FD, [IoData, $\n]),
    ok.

%% @spec close(Handle) -> ok
%% @doc Close the log file referenced by Handle.
close({?MODULE, _Name, FD}) ->
    ok = file:sync(FD),
    ok = file:close(FD),
    ok.

fix_log(FD) ->
    {ok, Location} = file:position(FD, eof),
    Seek = find_last_newline(FD, Location),
    {ok, Seek} = file:position(FD, Seek),
    ok = file:truncate(FD),
    ok.

%% Seek backwards to the last valid log entry
find_last_newline(_FD, N) when N =< 1 ->
    0;
find_last_newline(FD, Location) ->
    case file:pread(FD, Location - 1, 1) of
	{ok, <<$\n>>} ->
            Location;
	{ok, _} ->
	    find_last_newline(FD, Location - 1)
    end.

