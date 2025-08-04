import geql/ast
import geql/executor
import geql/parser
import geql/schema
import gleam/io
import gleam/option.{None, Some}

pub fn main() -> Nil {
  io.println("=== GeQL Pure GraphQL Library ===")
  io.println("")
  io.println(
    "GeQL is a pure GraphQL library for Gleam with zero dependencies beyond gleam_stdlib",
  )
  io.println("")

  // Test parsing
  let query = "{ user { entries { id } } }"
  case parser.parse(query) {
    Ok(_document) -> {
      io.println("✅ Parser: Successfully parsed GraphQL query!")
    }
    Error(_error) -> {
      io.println("❌ Parser: Parse error occurred")
    }
  }

  io.println("")

  // Show simple schema creation
  io.println("🏗️ Schema Definition:")
  let simple_schema = create_simple_schema()
  io.println("✅ Created GraphQL schema with User type")

  io.println("")

  // Show simple query execution (will fail due to demo limitation)
  io.println("⚡ Query Execution:")
  let result = executor.execute_query(simple_schema, "{ user { id name } }")
  case result.data {
    Some(_) -> io.println("✅ Query executed successfully!")
    None -> {
      io.println(
        "ℹ️  Query execution shows where Dynamic serialization would be needed",
      )
      io.println(
        "   (Use gleam_json or external functions in real implementations)",
      )
    }
  }

  io.println("")
  io.println("📁 Complete Examples Available:")
  io.println("- ./examples/core_library_examples/ - Pure GraphQL examples")
  io.println("- ./examples/geql_web_app/ - Complete web server with database")
  io.println("")
  io.println("🧚‍♀️ GeQL: Pure GraphQL for Gleam!")
}

// Simple schema for demo
fn create_simple_schema() -> schema.Schema {
  let user_type =
    schema.object("User")
    |> schema.description("A user in the system")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.field_description("User ID")
      |> schema.resolver(fn(_info) { Error("Demo: Would return user ID here") }),
    )
    |> schema.field(
      schema.field_def("name", schema.string_type())
      |> schema.field_description("User name")
      |> schema.resolver(fn(_info) {
        Error("Demo: Would return user name here")
      }),
    )

  let query_type =
    schema.object("Query")
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get a user")
      |> schema.resolver(fn(_info) {
        Error("Demo: Would return user data here")
      }),
    )

  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
}

pub fn parse(query: String) -> Result(ast.Document, parser.ParseError) {
  parser.parse(query)
}
