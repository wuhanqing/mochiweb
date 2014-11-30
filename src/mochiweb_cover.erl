%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.

%% @doc Workarounds for various cover deficiencies.
-module(mochiweb_cover).
-export([get_beam/1, get_abstract_code/1,
         get_clauses/2, clause_lookup_table/1]).
-export([clause_lookup_table/2]).

%% Internal

get_beam(Module) ->
    {Module, Beam, _Path} = code:get_object_code(Module),
    Beam.

get_abstract_code(Beam) ->
    {ok, {_Module,
          [{abstract_code,
            {raw_abstract_v1, L}}]}} = beam_lib:chunks(Beam, [abstract_code]),
    L.

get_clauses(Function, Code) ->
    [L] = [Clauses || {function, _, FName, _, Clauses}
                          <- Code, FName =:= Function],
    L.

clause_lookup_table(Module, Function) ->
    clause_lookup_table(
      get_clauses(Function,
                  get_abstract_code(get_beam(Module)))).

clause_lookup_table(Clauses) ->
    lists:foldr(fun clause_fold/2, [], Clauses).

clause_fold({clause, _,
             [InTerm],
             _Guards=[],
             [OutTerm]},
            Acc) ->
    try [{erl_parse:normalise(InTerm), erl_parse:normalise(OutTerm)} | Acc]
    catch error:_ -> Acc
    end;
clause_fold(_, Acc) ->
    Acc.

