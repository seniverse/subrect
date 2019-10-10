%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(subrect).

-export([parse/1, annotate/1, validate/2, normalize/2, to_parts/2, extract/3]).

-type constraint_expr() :: string() | binary().
%% type of constraint expression. You may use a string or a binary as constraint expression.

-type slice() :: non_neg_integer()
               | {non_neg_integer(), pos_integer(), non_neg_integer()}
               | {non_neg_integer(), pos_integer()}.

-type constraint() :: binary()
                    | [slice()]
                    | {constraint(), [constraint()]}.

-type elem() :: pos_integer()
              | [{binary(), field()}].

-type field() :: {elem(), [pos_integer()]}.

-type schema() :: #{binary() => field()}.

%% @doc parse the `Constraint' expression to internal representation
-spec parse(constraint_expr()) -> Result
  when Result :: {ok, [constraint()]} | Error,
       Error :: {error, {Line, module(), Reason}},
       Line :: pos_integer(),
       Reason :: term().
parse(Constraint) ->
    case subrect_lexer:string(unicode:characters_to_list(Constraint)) of
        {ok, Tokens, _} ->
            subrect_parser:parse(Tokens);
        {error, Error, _} ->
            {error, Error}
    end.


-type annotated_elem() :: pos_integer()
                        | annotated_schema().

-type annotated_field() :: {{non_neg_integer(), pos_integer()}, {annotated_elem(), [pos_integer()]}}.

-type annotated_schema() :: #{binary() => annotated_field()}.

-spec annotate(schema()) -> annotated_schema().

annotate(Schema) ->
    maps:map(fun(_, V) -> annotate_field(0, V) end, Schema).

annotate_field(Offset, {Elem, Shape}) ->
    {Elem1, N} = annotate_elem(Elem),
    {Shape1, Size} =
        lists:mapfoldr(
          fun (E, Acc) ->
                  {Acc, E*Acc}
          end,
          N,
          Shape),
    {{Offset, Size}, {Elem1, Shape1}}.

annotate_elem(N) when is_integer(N) ->
    {N, N};
annotate_elem(List) ->
    {List1, N} =
        lists:mapfoldl(
          fun({K,V}, Acc) ->
                  {{Offset, Size}, _} = Field = annotate_field(Acc, V),
                  {{K,Field}, Offset+Size}
          end,
          0,
          List),
    {maps:from_list(List1), N}.


-spec validate([constraint()], schema()) -> boolean().
validate([], _Schema) ->
    true;
validate([H|T], Schema) ->
    validate_constraint(H, Schema) and validate(T, Schema).

validate_constraint(Constraint, {Elem, []}) ->
    validate_constraint(Constraint, Elem);
validate_constraint(Name, Schema) when is_binary(Name), is_map(Schema) ->
    maps:is_key(Name, Schema);
validate_constraint({Name, Constraints}, Schema) when is_binary(Name), is_map(Schema) ->
    case maps:find(Name, Schema) of
        error ->
            false;
        {ok, Field} ->
            validate(Constraints, Field)
    end;
validate_constraint(Name, Fields) when is_binary(Name), is_list(Fields) ->
    lists:keymember(Name, 1, Fields);
validate_constraint({Name, Constraints}, Fields) when is_binary(Name), is_list(Fields) ->
    case lists:keyfind(Name, 1, Fields) of
        false ->
            false;
        {Name, Field} ->
            validate(Constraints, Field)
    end;
validate_constraint(Slices, {_, [H|_]}) when is_list(Slices) ->
    validate_slices(Slices, H);
validate_constraint({Slices, Constraints}, {Elem, [H|T]}) when is_list(Slices) ->
    validate_slices(Slices, H) and validate(Constraints, {Elem, T});
validate_constraint(_, _) ->
    false.

validate_slices([], _) ->
    true;
validate_slices([H|T], Size) ->
    validate_slice(H, Size) and validate_slices(T, Size).

validate_slice(I, N) when is_integer(I), I >= 0, I < N ->
    true;
validate_slice({Start, Stride, Stop}, N) when Start >= 0, Start < N, Stop >= Start ->
    Max = Start + (Stop - Start) div Stride * Stride,
    Max < N;
validate_slice({Start, _}, N) when Start >= 0, Start < N ->
    true;
validate_slice(_, _) ->
    false.

-type normalized_slice() :: {non_neg_integer(), pos_integer(), non_neg_integer()}.
-type normalized_constraint() :: binary()
                               | [normalized_slice()]
                               | {normalized_constraint(), [normalized_constraint()]}.
-spec normalize([constraint()], schema()) -> [normalized_constraint()].
normalize(Constraints, Schema) ->
    [normalize_constraint(C, Schema) || C <- Constraints].

normalize_constraint(Constraint, {Elem, []}) ->
    normalize_constraint(Constraint, Elem);
normalize_constraint({Name, Constraints}, Schema) when is_binary(Name), is_map(Schema) ->
    {Name, normalize(Constraints, maps:get(Name, Schema))};
normalize_constraint({Name, Constraints}, Fields) when is_binary(Name), is_list(Fields) ->
    {Name, Field} = lists:keyfind(Name, 1, Fields),
    {Name, normalize(Constraints, Field)};
normalize_constraint(Slices, {_, [H|_]}) when is_list(Slices) ->
    [ normalize_slice(S, H) || S <- Slices ];
normalize_constraint({Slices, Constraints}, {Elem, [H|T]}) when is_list(Slices) ->
    {normalize_constraint(Slices, {Elem, [H|T]}), normalize(Constraints, {Elem, T})};
normalize_constraint(Constraint, _) ->
    Constraint.

normalize_slice(I, _) when is_integer(I) ->
    {I, 1, I};
normalize_slice({Start, Stride}, N)->
    {Start, Stride, N-1};
normalize_slice(Slice, _) ->
    Slice.

-type part() :: {binary(), {non_neg_integer(), pos_integer()}}.

-type parts() :: part()
               | [part()].

-spec to_parts([normalized_constraint()], annotated_schema()) -> parts().

to_parts(Constraints, Schema) when is_list(Constraints), is_map(Schema) ->
    [to_parts_var(C, Schema)
     || C <- Constraints ].

to_parts_var(Name, Schema) when is_binary(Name), is_map(Schema) ->
    {Part, _}= maps:get(Name, Schema),
    {Name, Part};
to_parts_var({Name, Constraints}, Schema) when is_binary(Name), is_map(Schema) ->
    {_, Field}= maps:get(Name, Schema),
    [to_parts(C, Name, 0, Field) || C <- Constraints ].

to_parts(Constraint, Var, Offset, {Elem, []}) ->
    to_parts(Constraint, Var, Offset, Elem);
to_parts(Name, Var, Offset, Fields) when is_binary(Name), is_map(Fields) ->
    {{Offset1, Size}, _} = maps:get(Name, Fields),
    {Var, {Offset + Offset1, Size}};
to_parts(Slices, Var, Offset, {_, [H|_]}) when is_list(Slices) ->
    [to_parts_slice(S, Var, Offset, H) || S <- Slices ];
to_parts({Name, Constraints}, Var, Offset, Fields) when is_binary(Name), is_map(Fields) ->
    {{Offset1, _}, Field} = maps:get(Name, Fields),
    [to_parts(C, Var, Offset+Offset1, Field) || C <- Constraints];
to_parts({Slices, Constraints}, Var, Offset, {Elem, [H|T]}) when is_list(Slices) ->
    [ to_parts(C, Var, Offset + I * H, {Elem, T})
      || {Start, Stride, Stop} <- Slices,
         I <- lists:seq(Start, Stop, Stride),
         C <- Constraints ].

to_parts_slice({Start, 1, Stop}, Var, Offset, Size) ->
    {Var, {Offset + Start * Size, (Stop + 1 - Start) * Size}};
to_parts_slice({Start, Stride, Stop}, Var, Offset, Size) ->
    [{Var, {Offset + I * Size, Size}}
     || I <- lists:seq(Start, Stop, Stride) ].


-spec extract([normalized_constraint()], #{binary() => binary()}, annotated_schema()) -> iodata().

extract(Constraints, Data, Schema) when is_list(Constraints), is_map(Schema) ->
    [extract_var(C, Data, Schema)
     || C <- Constraints ].

extract_var(Name, Data, Schema) when is_binary(Name), is_map(Schema) ->
    maps:get(Name, Data);
extract_var({Name, Constraints}, Data, Schema) when is_binary(Name), is_map(Schema) ->
    Part = maps:get(Name, Data),
    {_, Field}= maps:get(Name, Schema),
    [extract_constraint(C, Part, Field) || C <- Constraints ].

extract_constraint(Constraint, Data, {Elem, []}) ->
    extract_constraint(Constraint, Data, Elem);
extract_constraint(Name, Data, Fields) when is_binary(Name), is_map(Fields) ->
    {Part, _} = maps:get(Name, Fields),
    binary:part(Data, Part);
extract_constraint(Slices, Data, {_, [H|_]}) when is_list(Slices) ->
    [ extract_slice(S, Data, H) || S <- Slices ];
extract_constraint({Name, Constraints}, Data, Fields) when is_binary(Name), is_map(Fields) ->
    {Part, Field} = maps:get(Name, Fields),
    Data1 = binary:part(Data, Part),
    [extract_constraint(C, Data1, Field) || C <- Constraints];
extract_constraint({Slices, Constraints}, Data, {Elem, [H|T]}) when is_list(Slices) ->
    [extract_constraints(
       Start, Stride, Stop,
       H,
       Data,
       Constraints,
       {Elem, T})
      || {Start, Stride, Stop} <- Slices].

extract_constraints(Start, Stride, Stop, Size, Data, Constraints, Field) when Start =< Stop ->
    Data1 = binary:part(Data, Start * Size, Size),
    [[extract_constraint(C, Data1, Field)
      || C <- Constraints ]
     |extract_constraints(Start+Stride, Stride, Stop, Size, Data, Constraints, Field)];
extract_constraints(_, _, _, _, _, _, _) ->
    [].


extract_slice({Start, 1, Stop}, Data, Size) ->
    binary:part(Data, Start * Size, (Stop + 1 - Start) * Size);
extract_slice({Start, Stride, Stop}, Data, Size) when Start =< Stop ->
    [binary:part(Data, Start * Size, Size)|extract_slice({Start+Stride, Stride, Stop}, Data, Size)];
extract_slice({Start, Stride, Stop}, Data, Size) ->
    [].
