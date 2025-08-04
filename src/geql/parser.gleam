import geql/ast.{
  type Document, type Field, type Operation, type OperationType, type Selection,
  type SelectionSet,
}
import geql/lexer.{
  type LexerError, type Position, type Token, type TokenWithPosition,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type ParseError {
  LexError(error: LexerError)
  UnexpectedToken(expected: String, got: Token, position: Position)
  UnexpectedEOF(expected: String)
}

pub type Parser {
  Parser(tokens: List(TokenWithPosition), position: Int)
}

pub fn parse(input: String) -> Result(Document, ParseError) {
  use tokens <- result.try(lexer.tokenize(input) |> result.map_error(LexError))
  let parser = Parser(tokens: tokens, position: 0)
  parse_document(parser)
  |> result.map(fn(result) { result.0 })
}

fn parse_document(parser: Parser) -> Result(#(Document, Parser), ParseError) {
  use #(definitions, parser) <- result.try(parse_definitions(parser, []))
  Ok(#(ast.Document(definitions), parser))
}

fn parse_definitions(
  parser: Parser,
  acc: List(ast.Definition),
) -> Result(#(List(ast.Definition), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.EOF, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(_) -> {
      use #(definition, parser) <- result.try(parse_definition(parser))
      parse_definitions(parser, [definition, ..acc])
    }
    Error(_) -> Ok(#(list.reverse(acc), parser))
  }
}

fn parse_definition(
  parser: Parser,
) -> Result(#(ast.Definition, Parser), ParseError) {
  parse_operation_definition(parser)
  |> result.map(fn(result) { #(ast.OperationDefinition(result.0), result.1) })
}

fn parse_operation_definition(
  parser: Parser,
) -> Result(#(Operation, Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.LeftBrace, _)) -> {
      use #(selection_set, parser) <- result.try(parse_selection_set(parser))
      Ok(#(ast.ShorthandQuery(selection_set), parser))
    }
    Ok(lexer.TokenWithPosition(lexer.Query, _))
    | Ok(lexer.TokenWithPosition(lexer.Mutation, _))
    | Ok(lexer.TokenWithPosition(lexer.Subscription, _)) -> {
      use #(op_type, parser) <- result.try(parse_operation_type(parser))
      use #(name, parser) <- result.try(parse_optional_name(parser))
      use #(selection_set, parser) <- result.try(parse_selection_set(parser))
      Ok(#(
        ast.Operation(
          operation_type: op_type,
          name: name,
          variable_definitions: [],
          directives: [],
          selection_set: selection_set,
        ),
        parser,
      ))
    }
    Ok(lexer.TokenWithPosition(token, position)) ->
      Error(UnexpectedToken("operation or selection set", token, position))
    Error(_) -> Error(UnexpectedEOF("operation or selection set"))
  }
}

fn parse_operation_type(
  parser: Parser,
) -> Result(#(OperationType, Parser), ParseError) {
  case consume_token(parser) {
    Ok(#(lexer.TokenWithPosition(lexer.Query, _), parser)) ->
      Ok(#(ast.Query, parser))
    Ok(#(lexer.TokenWithPosition(lexer.Mutation, _), parser)) ->
      Ok(#(ast.Mutation, parser))
    Ok(#(lexer.TokenWithPosition(lexer.Subscription, _), parser)) ->
      Ok(#(ast.Subscription, parser))
    Ok(#(lexer.TokenWithPosition(token, position), _)) ->
      Error(UnexpectedToken("query, mutation, or subscription", token, position))
    Error(_) -> Error(UnexpectedEOF("operation type"))
  }
}

fn parse_optional_name(
  parser: Parser,
) -> Result(#(Option(String), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Name(name), _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("name") }),
      )
      Ok(#(Some(name), parser))
    }
    _ -> Ok(#(None, parser))
  }
}

fn parse_selection_set(
  parser: Parser,
) -> Result(#(SelectionSet, Parser), ParseError) {
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.LeftBrace,
    "'{' to start selection set",
  ))
  use #(selections, parser) <- result.try(parse_selections(parser, []))
  use #(_, parser) <- result.try(expect_token(
    parser,
    lexer.RightBrace,
    "'}' to end selection set",
  ))
  Ok(#(ast.SelectionSet(selections), parser))
}

fn parse_selections(
  parser: Parser,
  acc: List(Selection),
) -> Result(#(List(Selection), Parser), ParseError) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.RightBrace, _)) ->
      Ok(#(list.reverse(acc), parser))
    Ok(_) -> {
      use #(selection, parser) <- result.try(parse_selection(parser))
      parse_selections(parser, [selection, ..acc])
    }
    Error(_) -> Error(UnexpectedEOF("selection or '}'"))
  }
}

fn parse_selection(parser: Parser) -> Result(#(Selection, Parser), ParseError) {
  parse_field(parser)
  |> result.map(fn(result) { #(ast.FieldSelection(result.0), result.1) })
}

fn parse_field(parser: Parser) -> Result(#(Field, Parser), ParseError) {
  use #(first_name, parser) <- result.try(parse_name_from_parser(parser))

  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.Colon, _)) -> {
      use #(_, parser) <- result.try(
        consume_token(parser)
        |> result.map_error(fn(_) { UnexpectedEOF("colon") }),
      )
      use #(second_name, parser) <- result.try(parse_name_from_parser(parser))
      let #(selection_set, parser) = parse_optional_selection_set(parser)

      Ok(#(
        ast.Field(
          alias: Some(first_name),
          name: second_name,
          arguments: [],
          directives: [],
          selection_set: selection_set,
        ),
        parser,
      ))
    }
    _ -> {
      let #(selection_set, parser) = parse_optional_selection_set(parser)

      Ok(#(
        ast.Field(
          alias: None,
          name: first_name,
          arguments: [],
          directives: [],
          selection_set: selection_set,
        ),
        parser,
      ))
    }
  }
}

fn parse_optional_selection_set(
  parser: Parser,
) -> #(Option(SelectionSet), Parser) {
  case peek_token(parser) {
    Ok(lexer.TokenWithPosition(lexer.LeftBrace, _)) -> {
      case parse_selection_set(parser) {
        Ok(#(ss, p)) -> #(Some(ss), p)
        Error(_) -> #(None, parser)
      }
    }
    _ -> #(None, parser)
  }
}

fn parse_name_from_parser(
  parser: Parser,
) -> Result(#(String, Parser), ParseError) {
  case consume_token(parser) {
    Ok(#(lexer.TokenWithPosition(lexer.Name(name), _), parser)) ->
      Ok(#(name, parser))
    Ok(#(lexer.TokenWithPosition(token, position), _)) ->
      Error(UnexpectedToken("name", token, position))
    Error(_) -> Error(UnexpectedEOF("name"))
  }
}

fn peek_token(parser: Parser) -> Result(TokenWithPosition, Nil) {
  get_token_at(parser.tokens, parser.position)
}

fn consume_token(parser: Parser) -> Result(#(TokenWithPosition, Parser), Nil) {
  case get_token_at(parser.tokens, parser.position) {
    Ok(token) -> Ok(#(token, Parser(..parser, position: parser.position + 1)))
    Error(_) -> Error(Nil)
  }
}

fn get_token_at(
  tokens: List(TokenWithPosition),
  index: Int,
) -> Result(TokenWithPosition, Nil) {
  get_token_at_helper(tokens, index, 0)
}

fn get_token_at_helper(
  tokens: List(TokenWithPosition),
  target_index: Int,
  current_index: Int,
) -> Result(TokenWithPosition, Nil) {
  case tokens {
    [] -> Error(Nil)
    [first, ..] if current_index == target_index -> Ok(first)
    [_, ..rest] -> get_token_at_helper(rest, target_index, current_index + 1)
  }
}

fn expect_token(
  parser: Parser,
  expected: Token,
  description: String,
) -> Result(#(TokenWithPosition, Parser), ParseError) {
  case consume_token(parser) {
    Ok(#(lexer.TokenWithPosition(token, position), parser)) -> {
      case token == expected {
        True -> Ok(#(lexer.TokenWithPosition(token, position), parser))
        False -> Error(UnexpectedToken(description, token, position))
      }
    }
    Error(_) -> Error(UnexpectedEOF(description))
  }
}
