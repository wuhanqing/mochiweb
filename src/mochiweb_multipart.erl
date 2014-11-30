%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Utilities for parsing multipart/form-data.

-module(mochiweb_multipart).
-author('bob@mochimedia.com').

-export([parse_form/1, parse_form/2]).
-export([parse_multipart_request/2]).
-export([parts_to_body/3, parts_to_multipart_body/4]).
-export([default_file_handler/2]).

-define(CHUNKSIZE, 4096).

-record(mp, {state, boundary, length, buffer, callback, req}).

%% TODO: DOCUMENT THIS MODULE.
%% @type key() = atom() | string() | binary().
%% @type value() = atom() | iolist() | integer().
%% @type header() = {key(), value()}.
%% @type bodypart() = {Start::integer(), End::integer(), Body::iolist()}.
%% @type formfile() = {Name::string(), ContentType::string(), Content::binary()}.
%% @type request().
%% @type file_handler() = (Filename::string(), ContentType::string()) -> file_handler_callback().
%% @type file_handler_callback() = (binary() | eof) -> file_handler_callback() | term().

%% @spec parts_to_body([bodypart()], ContentType::string(),
%%                     Size::integer()) -> {[header()], iolist()}
%% @doc Return {[header()], iolist()} representing the body for the given
%%      parts, may be a single part or multipart.
parts_to_body([{Start, End, Body}], ContentType, Size) ->
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    mochiweb_util:make_io(Start), "-", mochiweb_util:make_io(End),
                    "/", mochiweb_util:make_io(Size)]}],
    {HeaderList, Body};
parts_to_body(BodyList, ContentType, Size) when is_list(BodyList) ->
    parts_to_multipart_body(BodyList, ContentType, Size,
                            mochihex:to_hex(crypto:rand_bytes(8))).

%% @spec parts_to_multipart_body([bodypart()], ContentType::string(),
%%                               Size::integer(), Boundary::string()) ->
%%           {[header()], iolist()}
%% @doc Return {[header()], iolist()} representing the body for the given
%%      parts, always a multipart response.
parts_to_multipart_body(BodyList, ContentType, Size, Boundary) ->
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = multipart_body(BodyList, ContentType, Boundary, Size),

    {HeaderList, MultiPartBody}.

%% @spec multipart_body([bodypart()], ContentType::string(),
%%                      Boundary::string(), Size::integer()) -> iolist()
%% @doc Return the representation of a multipart body for the given [bodypart()].
multipart_body([], _ContentType, Boundary, _Size) ->
    ["--", Boundary, "--\r\n"];
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", ContentType, "\r\n",
     "Content-Range: ",
         "bytes ", mochiweb_util:make_io(Start), "-", mochiweb_util:make_io(End),
             "/", mochiweb_util:make_io(Size), "\r\n\r\n",
     Body, "\r\n"
     | multipart_body(BodyList, ContentType, Boundary, Size)].

%% @spec parse_form(request()) -> [{string(), string() | formfile()}]
%% @doc Parse a multipart form from the given request using the in-memory
%%      default_file_handler/2.
parse_form(Req) ->
    parse_form(Req, fun default_file_handler/2).

%% @spec parse_form(request(), F::file_handler()) -> [{string(), string() | term()}]
%% @doc Parse a multipart form from the given request using the given file_handler().
parse_form(Req, FileHandler) ->
    Callback = fun (Next) -> parse_form_outer(Next, FileHandler, []) end,
    {_, _, Res} = parse_multipart_request(Req, Callback),
    Res.

parse_form_outer(eof, _, Acc) ->
    lists:reverse(Acc);
parse_form_outer({headers, H}, FileHandler, State) ->
    {"form-data", H1} = proplists:get_value("content-disposition", H),
    Name = proplists:get_value("name", H1),
    Filename = proplists:get_value("filename", H1),
    case Filename of
        undefined ->
            fun (Next) ->
                    parse_form_value(Next, {Name, []}, FileHandler, State)
            end;
        _ ->
            ContentType = proplists:get_value("content-type", H),
            Handler = FileHandler(Filename, ContentType),
            fun (Next) ->
                    parse_form_file(Next, {Name, Handler}, FileHandler, State)
            end
    end.

parse_form_value(body_end, {Name, Acc}, FileHandler, State) ->
    Value = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
    State1 = [{Name, Value} | State],
    fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
parse_form_value({body, Data}, {Name, Acc}, FileHandler, State) ->
    Acc1 = [Data | Acc],
    fun (Next) -> parse_form_value(Next, {Name, Acc1}, FileHandler, State) end.

parse_form_file(body_end, {Name, Handler}, FileHandler, State) ->
    Value = Handler(eof),
    State1 = [{Name, Value} | State],
    fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
parse_form_file({body, Data}, {Name, Handler}, FileHandler, State) ->
    H1 = Handler(Data),
    fun (Next) -> parse_form_file(Next, {Name, H1}, FileHandler, State) end.

default_file_handler(Filename, ContentType) ->
    default_file_handler_1(Filename, ContentType, []).

default_file_handler_1(Filename, ContentType, Acc) ->
    fun(eof) ->
            Value = iolist_to_binary(lists:reverse(Acc)),
            {Filename, ContentType, Value};
       (Next) ->
            default_file_handler_1(Filename, ContentType, [Next | Acc])
    end.

parse_multipart_request(Req, Callback) ->
    %% TODO: Support chunked?
    Length = list_to_integer(Req:get_combined_header_value("content-length")),
    Boundary = iolist_to_binary(
                 get_boundary(Req:get_header_value("content-type"))),
    Prefix = <<"\r\n--", Boundary/binary>>,
    BS = byte_size(Boundary),
    Chunk = read_chunk(Req, Length),
    Length1 = Length - byte_size(Chunk),
    <<"--", Boundary:BS/binary, "\r\n", Rest/binary>> = Chunk,
    feed_mp(headers, flash_multipart_hack(#mp{boundary=Prefix,
                                              length=Length1,
                                              buffer=Rest,
                                              callback=Callback,
                                              req=Req})).

parse_headers(<<>>) ->
    [];
parse_headers(Binary) ->
    parse_headers(Binary, []).

parse_headers(Binary, Acc) ->
    case find_in_binary(<<"\r\n">>, Binary) of
        {exact, N} ->
            <<Line:N/binary, "\r\n", Rest/binary>> = Binary,
            parse_headers(Rest, [split_header(Line) | Acc]);
        not_found ->
            lists:reverse([split_header(Binary) | Acc])
    end.

split_header(Line) ->
    {Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end,
                                           binary_to_list(Line)),
    {string:to_lower(string:strip(Name)),
     mochiweb_util:parse_header(Value)}.

read_chunk(Req, Length) when Length > 0 ->
    case Length of
        Length when Length < ?CHUNKSIZE ->
            Req:recv(Length);
        _ ->
            Req:recv(?CHUNKSIZE)
    end.

read_more(State=#mp{length=Length, buffer=Buffer, req=Req}) ->
    Data = read_chunk(Req, Length),
    Buffer1 = <<Buffer/binary, Data/binary>>,
    flash_multipart_hack(State#mp{length=Length - byte_size(Data),
                                  buffer=Buffer1}).

flash_multipart_hack(State=#mp{length=0, buffer=Buffer, boundary=Prefix}) ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=22
    %% Flash doesn't terminate multipart with \r\n properly so we fix it up here
    PrefixSize = size(Prefix),
    case size(Buffer) - (2 + PrefixSize) of
        Seek when Seek >= 0 ->
            case Buffer of
                <<_:Seek/binary, Prefix:PrefixSize/binary, "--">> ->
                    Buffer1 = <<Buffer/binary, "\r\n">>,
                    State#mp{buffer=Buffer1};
                _ ->
                    State
            end;
        _ ->
            State
    end;
flash_multipart_hack(State) ->
    State.

feed_mp(headers, State=#mp{buffer=Buffer, callback=Callback}) ->
    {State1, P} = case find_in_binary(<<"\r\n\r\n">>, Buffer) of
                      {exact, N} ->
                          {State, N};
                      _ ->
                          S1 = read_more(State),
                          %% Assume headers must be less than ?CHUNKSIZE
                          {exact, N} = find_in_binary(<<"\r\n\r\n">>,
                                                      S1#mp.buffer),
                          {S1, N}
                  end,
    <<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
    NextCallback = Callback({headers, parse_headers(Headers)}),
    feed_mp(body, State1#mp{buffer=Rest,
                            callback=NextCallback});
feed_mp(body, State=#mp{boundary=Prefix, buffer=Buffer, callback=Callback}) ->
    Boundary = find_boundary(Prefix, Buffer),
    case Boundary of
        {end_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            C1 = Callback({body, Data}),
            C2 = C1(body_end),
            {State#mp.length, Rest, C2(eof)};
        {next_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            C1 = Callback({body, Data}),
            feed_mp(headers, State#mp{callback=C1(body_end),
                                      buffer=Rest});
        {maybe, Start} ->
            <<Data:Start/binary, Rest/binary>> = Buffer,
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}),
                                             buffer=Rest}));
        not_found ->
            {Data, Rest} = {Buffer, <<>>},
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}),
                                             buffer=Rest}))
    end.

get_boundary(ContentType) ->
    {"multipart/form-data", Opts} = mochiweb_util:parse_header(ContentType),
    case proplists:get_value("boundary", Opts) of
        S when is_list(S) ->
            S
    end.

%% @spec find_in_binary(Pattern::binary(), Data::binary()) ->
%%            {exact, N} | {partial, N, K} | not_found
%% @doc Searches for the given pattern in the given binary.
find_in_binary(P, Data) when size(P) > 0 ->
    PS = size(P),
    DS = size(Data),
    case DS - PS of
        Last when Last < 0 ->
            partial_find(P, Data, 0, DS);
        Last ->
            case binary:match(Data, P) of
                {Pos, _} -> {exact, Pos};
                nomatch -> partial_find(P, Data, Last+1, PS-1)
            end
    end.

partial_find(_B, _D, _N, 0) ->
    not_found;
partial_find(B, D, N, K) ->
    <<B1:K/binary, _/binary>> = B,
    case D of
        <<_Skip:N/binary, B1:K/binary>> ->
            {partial, N, K};
        _ ->
            partial_find(B, D, 1 + N, K - 1)
    end.

find_boundary(Prefix, Data) ->
    case find_in_binary(Prefix, Data) of
        {exact, Skip} ->
            PrefixSkip = Skip + size(Prefix),
            case Data of
                <<_:PrefixSkip/binary, "\r\n", _/binary>> ->
                    {next_boundary, Skip, size(Prefix) + 2};
                <<_:PrefixSkip/binary, "--\r\n", _/binary>> ->
                    {end_boundary, Skip, size(Prefix) + 4};
                _ when size(Data) < PrefixSkip + 4 ->
                    %% Underflow
                    {maybe, Skip};
                _ ->
                    %% False positive
                    not_found
            end;
        {partial, Skip, Length} when (Skip + Length) =:= size(Data) ->
            %% Underflow
            {maybe, Skip};
        _ ->
            not_found
    end.

