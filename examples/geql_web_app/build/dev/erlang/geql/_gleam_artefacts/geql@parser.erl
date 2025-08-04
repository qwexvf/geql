-module(geql@parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/geql/parser.gleam").
-export([parse/1]).
-export_type([parse_error/0, parser/0]).

-type parse_error() :: {lex_error, geql@lexer:lexer_error()} |
    {unexpected_token, binary(), geql@lexer:token(), geql@lexer:position()} |
    {unexpected_e_o_f, binary()}.

-type parser() :: {parser, list(geql@lexer:token_with_position()), integer()}.

-file("src/geql/parser.gleam", 239).
-spec get_token_at_helper(
    list(geql@lexer:token_with_position()),
    integer(),
    integer()
) -> {ok, geql@lexer:token_with_position()} | {error, nil}.
get_token_at_helper(Tokens, Target_index, Current_index) ->
    case Tokens of
        [] ->
            {error, nil};

        [First | _] when Current_index =:= Target_index ->
            {ok, First};

        [_ | Rest] ->
            get_token_at_helper(Rest, Target_index, Current_index + 1)
    end.

-file("src/geql/parser.gleam", 232).
-spec get_token_at(list(geql@lexer:token_with_position()), integer()) -> {ok,
        geql@lexer:token_with_position()} |
    {error, nil}.
get_token_at(Tokens, Index) ->
    get_token_at_helper(Tokens, Index, 0).

-file("src/geql/parser.gleam", 221).
-spec peek_token(parser()) -> {ok, geql@lexer:token_with_position()} |
    {error, nil}.
peek_token(Parser) ->
    get_token_at(erlang:element(2, Parser), erlang:element(3, Parser)).

-file("src/geql/parser.gleam", 225).
-spec consume_token(parser()) -> {ok,
        {geql@lexer:token_with_position(), parser()}} |
    {error, nil}.
consume_token(Parser) ->
    case get_token_at(erlang:element(2, Parser), erlang:element(3, Parser)) of
        {ok, Token} ->
            {ok,
                {Token,
                    begin
                        _record = Parser,
                        {parser,
                            erlang:element(2, _record),
                            erlang:element(3, Parser) + 1}
                    end}};

        {error, _} ->
            {error, nil}
    end.

-file("src/geql/parser.gleam", 87).
-spec parse_operation_type(parser()) -> {ok,
        {geql@ast:operation_type(), parser()}} |
    {error, parse_error()}.
parse_operation_type(Parser) ->
    case consume_token(Parser) of
        {ok, {{token_with_position, 'query', _}, Parser@1}} ->
            {ok, {'query', Parser@1}};

        {ok, {{token_with_position, mutation, _}, Parser@2}} ->
            {ok, {mutation, Parser@2}};

        {ok, {{token_with_position, subscription, _}, Parser@3}} ->
            {ok, {subscription, Parser@3}};

        {ok, {{token_with_position, Token, Position}, _}} ->
            {error,
                {unexpected_token,
                    <<"query, mutation, or subscription"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"operation type"/utf8>>}}
    end.

-file("src/geql/parser.gleam", 103).
-spec parse_optional_name(parser()) -> {ok,
        {gleam@option:option(binary()), parser()}} |
    {error, parse_error()}.
parse_optional_name(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, {name, Name}, _}} ->
            gleam@result:'try'(
                begin
                    _pipe = consume_token(Parser),
                    gleam@result:map_error(
                        _pipe,
                        fun(_) -> {unexpected_e_o_f, <<"name"/utf8>>} end
                    )
                end,
                fun(_use0) ->
                    {_, Parser@1} = _use0,
                    {ok, {{some, Name}, Parser@1}}
                end
            );

        _ ->
            {ok, {none, Parser}}
    end.

-file("src/geql/parser.gleam", 209).
-spec parse_name_from_parser(parser()) -> {ok, {binary(), parser()}} |
    {error, parse_error()}.
parse_name_from_parser(Parser) ->
    case consume_token(Parser) of
        {ok, {{token_with_position, {name, Name}, _}, Parser@1}} ->
            {ok, {Name, Parser@1}};

        {ok, {{token_with_position, Token, Position}, _}} ->
            {error, {unexpected_token, <<"name"/utf8>>, Token, Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"name"/utf8>>}}
    end.

-file("src/geql/parser.gleam", 251).
-spec expect_token(parser(), geql@lexer:token(), binary()) -> {ok,
        {geql@lexer:token_with_position(), parser()}} |
    {error, parse_error()}.
expect_token(Parser, Expected, Description) ->
    case consume_token(Parser) of
        {ok, {{token_with_position, Token, Position}, Parser@1}} ->
            case Token =:= Expected of
                true ->
                    {ok, {{token_with_position, Token, Position}, Parser@1}};

                false ->
                    {error, {unexpected_token, Description, Token, Position}}
            end;

        {error, _} ->
            {error, {unexpected_e_o_f, Description}}
    end.

-file("src/geql/parser.gleam", 195).
-spec parse_optional_selection_set(parser()) -> {gleam@option:option(geql@ast:selection_set()),
    parser()}.
parse_optional_selection_set(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, left_brace, _}} ->
            case parse_selection_set(Parser) of
                {ok, {Ss, P}} ->
                    {{some, Ss}, P};

                {error, _} ->
                    {none, Parser}
            end;

        _ ->
            {none, Parser}
    end.

-file("src/geql/parser.gleam", 118).
-spec parse_selection_set(parser()) -> {ok,
        {geql@ast:selection_set(), parser()}} |
    {error, parse_error()}.
parse_selection_set(Parser) ->
    gleam@result:'try'(
        expect_token(Parser, left_brace, <<"'{' to start selection set"/utf8>>),
        fun(_use0) ->
            {_, Parser@1} = _use0,
            gleam@result:'try'(
                parse_selections(Parser@1, []),
                fun(_use0@1) ->
                    {Selections, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        expect_token(
                            Parser@2,
                            right_brace,
                            <<"'}' to end selection set"/utf8>>
                        ),
                        fun(_use0@2) ->
                            {_, Parser@3} = _use0@2,
                            {ok, {{selection_set, Selections}, Parser@3}}
                        end
                    )
                end
            )
        end
    ).

-file("src/geql/parser.gleam", 135).
-spec parse_selections(parser(), list(geql@ast:selection())) -> {ok,
        {list(geql@ast:selection()), parser()}} |
    {error, parse_error()}.
parse_selections(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, right_brace, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, _} ->
            gleam@result:'try'(
                parse_selection(Parser),
                fun(_use0) ->
                    {Selection, Parser@1} = _use0,
                    parse_selections(Parser@1, [Selection | Acc])
                end
            );

        {error, _} ->
            {error, {unexpected_e_o_f, <<"selection or '}'"/utf8>>}}
    end.

-file("src/geql/parser.gleam", 150).
-spec parse_selection(parser()) -> {ok, {geql@ast:selection(), parser()}} |
    {error, parse_error()}.
parse_selection(Parser) ->
    _pipe = parse_field(Parser),
    gleam@result:map(
        _pipe,
        fun(Result) ->
            {{field_selection, erlang:element(1, Result)},
                erlang:element(2, Result)}
        end
    ).

-file("src/geql/parser.gleam", 155).
-spec parse_field(parser()) -> {ok, {geql@ast:field(), parser()}} |
    {error, parse_error()}.
parse_field(Parser) ->
    gleam@result:'try'(
        parse_name_from_parser(Parser),
        fun(_use0) ->
            {First_name, Parser@1} = _use0,
            case peek_token(Parser@1) of
                {ok, {token_with_position, colon, _}} ->
                    gleam@result:'try'(
                        begin
                            _pipe = consume_token(Parser@1),
                            gleam@result:map_error(
                                _pipe,
                                fun(_) ->
                                    {unexpected_e_o_f, <<"colon"/utf8>>}
                                end
                            )
                        end,
                        fun(_use0@1) ->
                            {_, Parser@2} = _use0@1,
                            gleam@result:'try'(
                                parse_name_from_parser(Parser@2),
                                fun(_use0@2) ->
                                    {Second_name, Parser@3} = _use0@2,
                                    {Selection_set, Parser@4} = parse_optional_selection_set(
                                        Parser@3
                                    ),
                                    {ok,
                                        {{field,
                                                {some, First_name},
                                                Second_name,
                                                [],
                                                [],
                                                Selection_set},
                                            Parser@4}}
                                end
                            )
                        end
                    );

                _ ->
                    {Selection_set@1, Parser@5} = parse_optional_selection_set(
                        Parser@1
                    ),
                    {ok,
                        {{field, none, First_name, [], [], Selection_set@1},
                            Parser@5}}
            end
        end
    ).

-file("src/geql/parser.gleam", 56).
-spec parse_operation_definition(parser()) -> {ok,
        {geql@ast:operation(), parser()}} |
    {error, parse_error()}.
parse_operation_definition(Parser) ->
    case peek_token(Parser) of
        {ok, {token_with_position, left_brace, _}} ->
            gleam@result:'try'(
                parse_selection_set(Parser),
                fun(_use0) ->
                    {Selection_set, Parser@1} = _use0,
                    {ok, {{shorthand_query, Selection_set}, Parser@1}}
                end
            );

        {ok, {token_with_position, 'query', _}} ->
            gleam@result:'try'(
                parse_operation_type(Parser),
                fun(_use0@1) ->
                    {Op_type, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Parser@2),
                        fun(_use0@2) ->
                            {Name, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_selection_set(Parser@3),
                                fun(_use0@3) ->
                                    {Selection_set@1, Parser@4} = _use0@3,
                                    {ok,
                                        {{operation,
                                                Op_type,
                                                Name,
                                                [],
                                                [],
                                                Selection_set@1},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, mutation, _}} ->
            gleam@result:'try'(
                parse_operation_type(Parser),
                fun(_use0@1) ->
                    {Op_type, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Parser@2),
                        fun(_use0@2) ->
                            {Name, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_selection_set(Parser@3),
                                fun(_use0@3) ->
                                    {Selection_set@1, Parser@4} = _use0@3,
                                    {ok,
                                        {{operation,
                                                Op_type,
                                                Name,
                                                [],
                                                [],
                                                Selection_set@1},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, subscription, _}} ->
            gleam@result:'try'(
                parse_operation_type(Parser),
                fun(_use0@1) ->
                    {Op_type, Parser@2} = _use0@1,
                    gleam@result:'try'(
                        parse_optional_name(Parser@2),
                        fun(_use0@2) ->
                            {Name, Parser@3} = _use0@2,
                            gleam@result:'try'(
                                parse_selection_set(Parser@3),
                                fun(_use0@3) ->
                                    {Selection_set@1, Parser@4} = _use0@3,
                                    {ok,
                                        {{operation,
                                                Op_type,
                                                Name,
                                                [],
                                                [],
                                                Selection_set@1},
                                            Parser@4}}
                                end
                            )
                        end
                    )
                end
            );

        {ok, {token_with_position, Token, Position}} ->
            {error,
                {unexpected_token,
                    <<"operation or selection set"/utf8>>,
                    Token,
                    Position}};

        {error, _} ->
            {error, {unexpected_e_o_f, <<"operation or selection set"/utf8>>}}
    end.

-file("src/geql/parser.gleam", 49).
-spec parse_definition(parser()) -> {ok, {geql@ast:definition(), parser()}} |
    {error, parse_error()}.
parse_definition(Parser) ->
    _pipe = parse_operation_definition(Parser),
    gleam@result:map(
        _pipe,
        fun(Result) ->
            {{operation_definition, erlang:element(1, Result)},
                erlang:element(2, Result)}
        end
    ).

-file("src/geql/parser.gleam", 34).
-spec parse_definitions(parser(), list(geql@ast:definition())) -> {ok,
        {list(geql@ast:definition()), parser()}} |
    {error, parse_error()}.
parse_definitions(Parser, Acc) ->
    case peek_token(Parser) of
        {ok, {token_with_position, e_o_f, _}} ->
            {ok, {lists:reverse(Acc), Parser}};

        {ok, _} ->
            gleam@result:'try'(
                parse_definition(Parser),
                fun(_use0) ->
                    {Definition, Parser@1} = _use0,
                    parse_definitions(Parser@1, [Definition | Acc])
                end
            );

        {error, _} ->
            {ok, {lists:reverse(Acc), Parser}}
    end.

-file("src/geql/parser.gleam", 29).
-spec parse_document(parser()) -> {ok, {geql@ast:document(), parser()}} |
    {error, parse_error()}.
parse_document(Parser) ->
    gleam@result:'try'(
        parse_definitions(Parser, []),
        fun(_use0) ->
            {Definitions, Parser@1} = _use0,
            {ok, {{document, Definitions}, Parser@1}}
        end
    ).

-file("src/geql/parser.gleam", 22).
-spec parse(binary()) -> {ok, geql@ast:document()} | {error, parse_error()}.
parse(Input) ->
    gleam@result:'try'(
        begin
            _pipe = geql@lexer:tokenize(Input),
            gleam@result:map_error(
                _pipe,
                fun(Field@0) -> {lex_error, Field@0} end
            )
        end,
        fun(Tokens) ->
            Parser = {parser, Tokens, 0},
            _pipe@1 = parse_document(Parser),
            gleam@result:map(
                _pipe@1,
                fun(Result) -> erlang:element(1, Result) end
            )
        end
    ).
