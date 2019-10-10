-module(subrect_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_lex_error, test_parse_error, test_parse, test_annotate, test_validate, test_normalize, test_to_parts, test_extract].

test_lex_error(_Config) ->
    {error, {_, M, A}} = subrect:parse("/"),
    <<"illegal characters \"/\"">> = iolist_to_binary(M:format_error(A)),
    ok.

test_parse_error(_Config) ->
    {error, {_, M, A}} = subrect:parse("a{"),
    <<"syntax error before: ">> = iolist_to_binary(M:format_error(A)),
    ok.

test_parse(_Config) ->
    {ok, [[]]} = subrect:parse("[]"),
    {ok, [[0]]} = subrect:parse("[0]"),
    {ok, [[0,1]]} = subrect:parse("[0,1]"),
    {ok, [[{0,1,2}]]} = subrect:parse("[0:2]"),
    {ok, [[{0,1}]]} = subrect:parse("[0:]"),
    {ok, [[{0,1,2}]]} = subrect:parse("[:2]"),
    {ok, [[{0,1,2}]]} = subrect:parse("[0:1:2]"),
    {ok, [[{0,1,2}]]} = subrect:parse("[:1:2]"),
    {ok, [[{0,1}]]} = subrect:parse("[0:1:]"),
    {ok, [<<"a">>]} = subrect:parse("a"),
    {ok, [<<"a">>, <<"b">>]} = subrect:parse("a,b"),
    {ok, [{<<"a">>, [<<"b">>]}]} = subrect:parse("a.b"),
    {ok, [{<<"a">>, [[0]]}]} = subrect:parse("a[0]"),
    {ok, [{<<"a">>, [<<"b">>, <<"c">>]}]} = subrect:parse("a{b,c}"),
    {ok, [{<<"a">>, [{<<"b">>, [<<"c">>, <<"d">>]}]}, <<"e">>]} = subrect:parse("a.b{c,d},e"),
    ok.

test_annotate(_Config) ->
    #{<<"a">> := {{0,4}, {4, []}}} = subrect:annotate(#{<<"a">> => {4, []}}),
    #{<<"a">> := {{0,64}, {4, [16,4]}}} = subrect:annotate(#{<<"a">> => {4, [4, 4]}}),
    #{<<"a">> := {{0, 20}, {#{<<"x">> := {{0,16},{4,[4]}},<<"y">> := {{16,4}, {4, []}}},[]}}} =
        subrect:annotate(#{<<"a">> => {[{<<"x">>, {4, [4]}}, {<<"y">>, {4, []}}], []}}).

validate(Expr, Schema) ->
    {ok, Constraints} = subrect:parse(Expr),
    subrect:validate(Constraints, Schema).

test_validate(_Config) ->
    true = validate("a", #{<<"a">> => {4, []}}),
    false = validate("b", #{<<"a">> => {4, []}}),
    false = validate("a[1]", #{<<"a">> => {4, []}}),
    true = validate("a", #{<<"a">> => {4, [4]}}),
    false = validate("a", #{<<"b">> => {4, [4]}}),
    true = validate("a[1]", #{<<"a">> => {4, [4]}}),
    true = validate("a[1:]", #{<<"a">> => {4, [4]}}),
    true = validate("a[1:2:3]", #{<<"a">> => {4, [4]}}),
    false = validate("a[1]", #{<<"b">> => {4, [4]}}),
    false = validate("a[1][1]", #{<<"a">> => {4, [4]}}),
    false = validate("a[4]", #{<<"a">> => {4, [4]}}),
    true = validate("a.x", #{<<"a">> => {[{<<"x">>, {4, []}}], []}}),
    true = validate("a.x[:2]", #{<<"a">> => {[{<<"x">>, {4, [4]}}], []}}),
    false = validate("a.x[:2]", #{<<"a">> => {[{<<"y">>, {4, [4]}}], []}}),
    false = validate("a.x[:2]", #{<<"a">> => {[{<<"x">>, {4, [4]}}], [4]}}),
    true = validate("a[2].x[:2]", #{<<"a">> => {[{<<"x">>, {4, [4]}}], [4]}}).


normalize(Expr, Schema) ->
    {ok, Constraints} = subrect:parse(Expr),
    subrect:normalize(Constraints, Schema).

test_normalize(_Config) ->
    [<<"a">>] = normalize("a", #{<<"a">> => {4, []}}),
    [{<<"a">>, [[{1,1,1}]]}] = normalize("a[1]", #{<<"a">> => {4, [4]}}),
    [{<<"a">>, [[{1,1,3}]]}] = normalize("a[1:]", #{<<"a">> => {4, [4]}}),
    [{<<"a">>,[{[{2,1,2}],[{<<"x">>,[[{1,1,3}]]}]}]}] = normalize("a[2].x[1:]", #{<<"a">> => {[{<<"x">>, {4, [4]}}], [4]}}).

to_parts(Expr, Schema) ->
    {ok, Constraints} = subrect:parse(Expr),
    Constraints1 = subrect:normalize(Constraints, Schema),
    Schema1 = subrect:annotate(Schema),
    lists:flatten(subrect:to_parts(Constraints1, Schema1)).

test_to_parts(_Config) ->
    [{<<"a">>, {0, 4}}] = to_parts("a", #{<<"a">> => {4, []}}),
    [{<<"a">>, {0, 4}},
     {<<"b">>, {0, 4}}] = to_parts("a,b", #{<<"a">> => {4, []}, <<"b">> => {4, []}}),
    [{<<"a">>, {0, 12}}] = to_parts("a[:2]", #{<<"a">> => {4, [4]}}),
    [{<<"a">>, {0, 12}},{<<"a">>, {16,12}},{<<"a">>, {32,12}}] = to_parts("a[:2][:2]", #{<<"a">> => {4, [4, 4]}}),
    [{<<"a">>, {4, 4}}] = to_parts("a.y", #{<<"a">> => {[{<<"x">>, {4, []}}, {<<"y">>, {4, []}}], []}}),
    [{<<"a">>, {36, 4}}, {<<"a">>, {44, 4}}] = to_parts("a[2].x[1:2:]", #{<<"a">> => {[{<<"x">>, {4, [4]}}], [4]}}).

make_data(Schema) ->
    maps:map(
      fun(_, {{_, Size}, _}) ->
           << <<(rand:uniform(256)-1)>> || _ <- lists:seq(1, Size) >>
      end,
      Schema).

extract(Expr, Schema) ->
    {ok, Constraints} = subrect:parse(Expr),
    Constraints1 = subrect:normalize(Constraints, Schema),
    Schema1 = subrect:annotate(Schema),
    Data = make_data(Schema1),
    Parts = lists:flatten(subrect:to_parts(Constraints1, Schema1)),
    Expected = iolist_to_binary([binary:part(maps:get(Name, Data), Part) || {Name, Part} <- Parts]),
    Extracted = iolist_to_binary(subrect:extract(Constraints1, Data, Schema1)),
    Extracted = Expected.

test_extract(_Config) ->
    extract("a", #{<<"a">> => {4, []}}),
    extract("a,b", #{<<"a">> => {4, []}, <<"b">> => {4, []}}),
    extract("a[:2]", #{<<"a">> => {4, [4]}}),
    extract("a[:2][:2]", #{<<"a">> => {4, [4, 4]}}),
    extract("a.y", #{<<"a">> => {[{<<"x">>, {4, []}}, {<<"y">>, {4, []}}], []}}),
    extract("a[2].x[1:2:]", #{<<"a">> => {[{<<"x">>, {4, [4]}}], [4]}}).
