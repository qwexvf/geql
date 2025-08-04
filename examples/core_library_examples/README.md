# GeQL Core Library Examples

This directory contains examples demonstrating the **pure GraphQL functionality** of GeQL without any web server or database dependencies.

## 🧚‍♀️ What These Examples Show

- **Pure GraphQL parsing** - Parse GraphQL queries into AST
- **Schema definition** - Build GraphQL schemas using the fluent API
- **Schema generation** - Auto-generate schemas from Gleam types
- **Query execution** - Execute GraphQL queries with resolvers
- **DataLoader integration** - Batch and cache data fetching to solve N+1 problem
- **Error handling** - Comprehensive GraphQL error management

## 🏃 Quick Start

```sh
cd examples/core_library_examples
gleam deps download
gleam run  # Run all core library examples
```

## 📁 Example Files

### `examples.gleam`
Demonstrates manual schema definition with the fluent builder API:

```gleam
let user_type = 
  schema.object("User")
  |> schema.description("A user in the system")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.id_type()))
    |> schema.field_description("The unique identifier")
  )
```

### `person_example.gleam` 
Shows **automatic schema generation** from Gleam types:

```gleam
pub type Person {
  Person(name: String, age: Int, needs_glasses: Bool)
}

// Auto-generate complete GraphQL schema
let schema = schema_gen.create_schema_with_query(
  "Person",
  [
    schema_gen.string_field("name", "The person's name", extract_name),
    schema_gen.int_field("age", "The person's age", extract_age), 
    schema_gen.bool_field("needsGlasses", "Whether needs glasses", extract_glasses),
  ],
  root_resolver
)
```

### `dataloader_example.gleam`
Shows **DataLoader integration** for solving the N+1 query problem:

```gleam
// Create DataLoaders for efficient batching
let user_loader = dataloader.new(batch_load_users)
let posts_loader = dataloader.new(batch_load_posts_by_author)

// Add to execution context
execution_context
|> schema.add_data_loader("users", user_loader)
|> schema.add_data_loader("posts_by_author", posts_loader)

// Use in resolvers to batch database calls
case schema.get_data_loader(info.context, "users") {
  Ok(loader) -> {
    let #(_updated_loader, result) = dataloader.load(loader, user_id)
    result // Multiple loads are automatically batched!
  }
}
```

### `example_execution.gleam`
Demonstrates **query execution** with struct-based resolvers:

```gleam
fn user_name_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case info.parent {
    Some(parent_user) -> {
      let user = decode_user_from_dynamic(parent_user)
      Ok(serialize_to_dynamic(user.name))
    }
    None -> Error("No parent user provided")
  }
}
```

## 🎯 Key Concepts Demonstrated

### 1. **Pure GraphQL Library**
These examples show how GeQL works as a **pure GraphQL implementation**:
- Zero web server dependencies
- Zero database dependencies  
- Focus only on GraphQL specification compliance

### 2. **Type-Safe Schema Building**
```gleam
// Fluent API with compile-time safety
let schema = 
  schema.schema()
  |> schema.query(query_type)
  |> schema.add_type(schema.ObjectTypeDef(user_type))
```

### 3. **Automatic Schema Generation**
```gleam
// Generate GraphQL schema from Gleam types
let schema = schema_gen.from_type("Person", field_specs)
```

### 4. **Struct-Based Resolvers**
```gleam
// Resolvers work with actual Gleam structs, not dynamic data
fn resolver(info) -> Result(Dynamic, String) {
  let user = decode_user_from_dynamic(info.parent)
  Ok(serialize_to_dynamic(user.field))
}
```

## 🔄 Expected Output

When you run `gleam run`, you'll see:

```
=== GeQL Core Library Examples ===

✅ Parser: Successfully parsed GraphQL query!

🎯 Core GraphQL Library Features:

📋 Auto-Generated Schema Example:
Generated GraphQL schema from Gleam type:
pub type Person {
  Person(name: String, age: Int, needs_glasses: Bool)
}
Executing query: { person { name age needsGlasses } }

🔧 Manual Schema Definition Example:
Created user schema with fluent API
Executing query: { user { id name email } }

🧚‍♀️ Pure GraphQL library demo complete!
```

## 💡 Understanding the "Demo Limitations"

You might see messages like:
```
Demo limitation: Dynamic creation requires external functions
```

This is **intentional** - it shows where a real implementation would use:
- `gleam_json` for JSON serialization
- External FFI functions for Dynamic conversion
- Database libraries for data persistence

The core GraphQL functionality (parsing, schema definition, execution) works perfectly!

## 🌐 Next Steps

After exploring these core examples:

1. **Web Integration** - See `../geql_web_app/` for complete web server example
2. **Your Own Project** - Use `gleam add geql` to add GeQL to your project
3. **Production Usage** - Follow patterns from the web app example

## 🧚‍♀️ Pure GraphQL Excellence

These examples demonstrate GeQL's core strength: **focused, pure GraphQL implementation** that you can integrate with any web framework, database, or architecture pattern.

GeQL does GraphQL. You do everything else. Perfect separation! ✨