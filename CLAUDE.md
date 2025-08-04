# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GeQL is a GraphQL library for Gleam that provides parsing, schema definition, and query execution capabilities. The project follows a clean architecture with a pure GraphQL core library and separate database/web integration examples.

## Commands

### Core Development
```bash
gleam build              # Build the core library
gleam test               # Run all tests (3 tests currently)
gleam run                # Run main demo (pure GraphQL demonstration)
```

### Example Applications
```bash
# Pure GraphQL examples
cd examples/core_library_examples
gleam run                # Run core library examples

# Web application example (requires PostgreSQL)
cd examples/geql_web_app
gleam deps download      # Install dependencies
gleam run -m cigogne last # Run database migrations
gleam run                # Start web server on :8080
```

## Architecture

The codebase follows a layered architecture with clear separation of concerns:

### Core Library (`src/geql/`)
- **Pure GraphQL implementation** with zero database dependencies
- Only depends on `gleam_stdlib`
- Complete GraphQL parsing, schema definition, and execution
- Key modules:
  - `ast.gleam` - Abstract Syntax Tree definitions
  - `lexer.gleam` - Tokenization and lexical analysis
  - `parser.gleam` - GraphQL query parsing
  - `schema.gleam` - Schema definition system with fluent builder API
  - `schema_gen.gleam` - Auto-generate schemas from Gleam types
  - `executor.gleam` - Query execution engine with resolver system
  - `dataloader.gleam` - Batching and caching for N+1 query problem

### Examples Structure
- `examples/core_library_examples/` - Pure GraphQL functionality demonstrations
- `examples/geql_web_app/` - Complete web application using Wisp, Cake, Cigogne, and PostgreSQL

## Key Concepts

### Schema Definition
Uses a fluent builder API for type-safe schema construction:
```gleam
let user_type = 
  schema.object("User")
  |> schema.description("A user in the system")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.id_type()))
    |> schema.field_description("User ID")
    |> schema.resolver(resolver_function)
  )
```

### Resolver System
Resolvers are struct-based functions that extract data from parent objects:
```gleam
|> schema.resolver(fn(info) {
  // info.parent - parent object value
  // info.arguments - field arguments 
  // info.context - execution context
  case extract_from_parent(info.parent) {
    Ok(data) -> Ok(serialize_to_dynamic(data))
    Error(err) -> Error("Resolution failed")
  }
})
```

### Schema Generation
Automatically generate GraphQL schemas from Gleam types:
```gleam
let schema = schema_gen.create_schema_with_query(
  "TypeName",
  [
    schema_gen.string_field("field", "description", extractor_fn),
    schema_gen.int_field("count", "description", extractor_fn),
  ],
  root_resolver_fn
)
```

## Development Notes

- Tests use `gleeunit` framework
- The core library maintains zero database dependencies
- Web integrations are kept in separate example projects
- Dynamic/JSON serialization requires external functions (use `gleam_json` in practice)
- All database connectivity handled via `cake` query builder and `cigogne` migrations in web example
- GraphiQL playground available at `/graphiql` in web application example

## Project Philosophy

The library follows a "pure core, flexible integration" approach where:
- Core GraphQL functionality remains dependency-free
- Database, web server, and business logic are handled in separate integration layers
- Type safety is maintained throughout the GraphQL type system and resolvers
- Schema generation reduces boilerplate while maintaining type consistency