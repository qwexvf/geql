import geql/ast
import geql/parser
import geql/schema
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/io
import gleam/string

pub type ExecutionResult {
  ExecutionResult(data: Option(Dynamic), errors: List(ExecutionError))
}

pub type ExecutionError {
  ValidationError(message: String, path: List(String))
  ResolverError(message: String, path: List(String))
  TypeError(message: String, path: List(String))
}

pub type QueryExecutionContext {
  QueryExecutionContext(
    schema: schema.Schema,
    root_value: Option(Dynamic),
    execution_context: schema.ExecutionContext,
    variable_values: Dict(String, Dynamic),
  )
}

pub type FieldContext {
  FieldContext(
    parent_value: Option(Dynamic),
    field_name: String,
    field_args: Dict(String, Dynamic),
    path: List(String),
  )
}

pub type DebugContext {
  DebugContext(
    enabled: Bool,
    indent_level: Int,
    step_counter: Int,
  )
}

pub fn new_debug_context(enabled: Bool) -> DebugContext {
  DebugContext(enabled: enabled, indent_level: 0, step_counter: 1)
}

fn debug_log(debug: DebugContext, message: String) -> DebugContext {
  case debug.enabled {
    True -> {
      let indent = string.repeat("  ", debug.indent_level)
      io.println(indent <> "🔍 " <> message)
      debug
    }
    False -> debug
  }
}

fn debug_step(debug: DebugContext, message: String) -> DebugContext {
  case debug.enabled {
    True -> {
      let indent = string.repeat("  ", debug.indent_level)
      let step_msg = int_to_string(debug.step_counter) <> ". " <> message
      io.println(indent <> "📋 " <> step_msg)
      DebugContext(..debug, step_counter: debug.step_counter + 1)
    }
    False -> debug
  }
}


fn debug_result(debug: DebugContext, message: String) -> DebugContext {
  case debug.enabled {
    True -> {
      let indent = string.repeat("  ", debug.indent_level)
      io.println(indent <> "✅ " <> message)
      debug
    }
    False -> debug
  }
}

fn debug_error(debug: DebugContext, message: String) -> DebugContext {
  case debug.enabled {
    True -> {
      let indent = string.repeat("  ", debug.indent_level)
      io.println(indent <> "❌ " <> message)
      debug
    }
    False -> debug
  }
}

fn int_to_string(i: Int) -> String {
  case i {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    _ -> "N"
  }
}

pub fn execute(
  schema_def: schema.Schema,
  document: ast.Document,
  root_value: Option(Dynamic),
  execution_context: schema.ExecutionContext,
  variable_values: Dict(String, Dynamic),
) -> ExecutionResult {
  let context =
    QueryExecutionContext(
      schema: schema_def,
      root_value: root_value,
      execution_context: execution_context,
      variable_values: variable_values,
    )

  case document.definitions {
    [ast.OperationDefinition(operation), ..] ->
      execute_operation(context, operation)
    [] ->
      ExecutionResult(data: None, errors: [
        ValidationError("Document must contain at least one operation", []),
      ])
    _ ->
      ExecutionResult(data: None, errors: [
        ValidationError("Document contains non-operation definitions", []),
      ])
  }
}

fn execute_operation(
  context: QueryExecutionContext,
  operation: ast.Operation,
) -> ExecutionResult {
  let root_type = case operation {
    ast.Operation(operation_type: ast.Query, ..) -> context.schema.query
    ast.Operation(operation_type: ast.Mutation, ..) -> context.schema.mutation
    ast.Operation(operation_type: ast.Subscription, ..) ->
      context.schema.subscription
    ast.ShorthandQuery(..) -> context.schema.query
  }

  case root_type {
    Some(object_type) -> {
      let selection_set = case operation {
        ast.Operation(selection_set: ss, ..) -> ss
        ast.ShorthandQuery(selection_set: ss) -> ss
      }

      let field_context =
        FieldContext(
          parent_value: context.root_value,
          field_name: "root",
          field_args: dict.new(),
          path: [],
        )

      execute_selection_set(context, selection_set, object_type, field_context)
    }
    None ->
      ExecutionResult(data: None, errors: [
        ValidationError(
          "Schema does not define a root type for this operation",
          [],
        ),
      ])
  }
}

fn execute_selection_set(
  context: QueryExecutionContext,
  selection_set: ast.SelectionSet,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  let results =
    list.map(selection_set.selections, fn(selection) {
      execute_selection(context, selection, object_type, field_context)
    })

  let errors =
    list.fold(results, [], fn(acc, result) { list.append(acc, result.errors) })

  let data_results =
    list.fold(results, [], fn(acc, result) {
      case result.data {
        Some(data) -> [data, ..acc]
        None -> acc
      }
    })

  case data_results, errors {
    [], [] -> ExecutionResult(data: Some(create_empty_object()), errors: [])
    [], _ -> ExecutionResult(data: None, errors: errors)
    _, _ -> {
      let merged_data = merge_selection_results(data_results)
      ExecutionResult(data: Some(merged_data), errors: errors)
    }
  }
}

fn execute_selection(
  context: QueryExecutionContext,
  selection: ast.Selection,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  case selection {
    ast.FieldSelection(field) ->
      execute_field(context, field, object_type, field_context)
    ast.FragmentSpread(_) ->
      ExecutionResult(data: None, errors: [
        ValidationError(
          "Fragment spreads not yet supported",
          field_context.path,
        ),
      ])
    ast.InlineFragment(_) ->
      ExecutionResult(data: None, errors: [
        ValidationError(
          "Inline fragments not yet supported",
          field_context.path,
        ),
      ])
  }
}

fn execute_field(
  context: QueryExecutionContext,
  field: ast.Field,
  object_type: schema.ObjectType,
  field_context: FieldContext,
) -> ExecutionResult {
  let field_name = field.name
  let field_path = list.append(field_context.path, [field_name])

  case dict.get(object_type.fields, field_name) {
    Ok(field_def) -> {
      // Get field arguments (simplified - no argument processing yet)
      let field_args = dict.new()

      let _new_field_context =
        FieldContext(
          parent_value: field_context.parent_value,
          field_name: field_name,
          field_args: field_args,
          path: field_path,
        )

      // Resolve the field value
      case field_def.resolver {
        Some(resolver) -> {
          let resolver_info =
            schema.ResolverInfo(
              parent: field_context.parent_value,
              arguments: field_args,
              context: context.execution_context,
              info: create_resolver_info(),
            )

          case resolver(resolver_info) {
            Ok(resolved_value) -> {
              // If field has a selection set, execute it
              case field.selection_set {
                Some(sub_selection_set) -> {
                  // Get the return type of this field
                  case
                    get_field_type_definition(
                      context.schema,
                      field_def.field_type,
                    )
                  {
                    Ok(schema.ObjectTypeDef(sub_object_type)) -> {
                      let sub_field_context =
                        FieldContext(
                          parent_value: Some(resolved_value),
                          field_name: field_name,
                          field_args: field_args,
                          path: field_path,
                        )
                      execute_selection_set(
                        context,
                        sub_selection_set,
                        sub_object_type,
                        sub_field_context,
                      )
                    }
                    Ok(_) ->
                      ExecutionResult(data: None, errors: [
                        TypeError(
                          "Cannot execute selection set on non-object type",
                          field_path,
                        ),
                      ])
                    Error(msg) ->
                      ExecutionResult(data: None, errors: [
                        TypeError(msg, field_path),
                      ])
                  }
                }
                None -> {
                  // Leaf field - return the resolved value
                  ExecutionResult(
                    data: Some(create_field_data(field_name, resolved_value)),
                    errors: [],
                  )
                }
              }
            }
            Error(error_msg) ->
              ExecutionResult(data: None, errors: [
                ResolverError(error_msg, field_path),
              ])
          }
        }
        None -> {
          // No resolver - try to get value from parent
          case field_context.parent_value {
            Some(parent) -> {
              let field_value = get_field_from_parent(parent, field_name)
              ExecutionResult(
                data: Some(create_field_data(field_name, field_value)),
                errors: [],
              )
            }
            None ->
              ExecutionResult(data: None, errors: [
                ResolverError("No resolver and no parent value", field_path),
              ])
          }
        }
      }
    }
    Error(_) ->
      ExecutionResult(data: None, errors: [
        ValidationError(
          "Field '"
            <> field_name
            <> "' not found on type '"
            <> object_type.name
            <> "'",
          field_path,
        ),
      ])
  }
}

fn get_field_type_definition(
  schema_def: schema.Schema,
  field_type: schema.FieldType,
) -> Result(schema.TypeDefinition, String) {
  case field_type {
    schema.Named(type_name) ->
      dict.get(schema_def.types, type_name)
      |> result.map_error(fn(_) {
        "Type '" <> type_name <> "' not found in schema"
      })
    schema.NonNull(inner_type) ->
      get_field_type_definition(schema_def, inner_type)
    schema.List(inner_type) -> get_field_type_definition(schema_def, inner_type)
  }
}

fn create_field_data(_field_name: String, _value: Dynamic) -> Dynamic {
  // Simplified - return a string representation
  create_string_dynamic("resolved_value")
}

fn get_field_from_parent(_parent: Dynamic, _field_name: String) -> Dynamic {
  // Simplified - in real implementation would use dynamic decoder
  create_string_dynamic("parent_field_value")
}

fn merge_selection_results(results: List(Dynamic)) -> Dynamic {
  // Simplified merger - in real implementation would properly merge JSON objects
  case results {
    [first, ..] -> first
    [] -> create_empty_object()
  }
}

// Helper functions to create Dynamic values
fn create_empty_object() -> Dynamic {
  // Simplified empty object representation
  create_string_dynamic("{}")
}

// Demo placeholder for Dynamic values
// In a real GraphQL implementation, you'd use proper JSON serialization
fn create_string_dynamic(_value: String) -> Dynamic {
  // Since we can't create Dynamic values without external functions in pure Gleam,
  // this demonstrates where proper Dynamic creation would happen.
  // 
  // In practice, you'd use:
  // - gleam_json for JSON serialization
  // - External functions for Dynamic conversion
  // - Proper type conversion libraries
  //
  // For this demo, we'll just create a compile error that shows the limitation
  // This way users understand they need proper Dynamic conversion in real use

  // For demo purposes, we indicate this limitation clearly
  // In practice, users should use gleam_json or other libraries
  panic as "Dynamic serialization needed: use gleam_json or similar library"
}

fn create_resolver_info() -> Dynamic {
  create_string_dynamic("resolver_info")
}

// Debug-enabled execution with tracing
pub fn execute_query_debug(schema: schema.Schema, query: String) -> ExecutionResult {
  let debug = new_debug_context(True)
  let debug = debug_step(debug, "🚀 Starting GraphQL Query Execution")
  let debug = debug_log(debug, "Query: " <> query)
  
  let debug = debug_step(debug, "📝 Parsing Query")
  case parser.parse(query) {
    Ok(document) -> {
      let debug = debug_result(debug, "Parse successful")
      let debug = debug_log(debug, "Document definitions: " <> int_to_string(list.length(document.definitions)))
      
      let debug = debug_step(debug, "🎯 Executing Query")
      
      // Use existing execute_query but with debug output
      let debug = debug_log(debug, "Calling standard executor...")
      let result = execute_query(schema, query)
      
      case result.data {
        Some(_) -> {
          let _ = debug_result(debug, "✅ Query executed successfully!")
          Nil
        }
        None -> {
          let _ = debug_error(debug, "❌ Query execution failed")
          let _ = debug_log(debug, "Errors: " <> int_to_string(list.length(result.errors)))
          Nil
        }
      }
      
      result
    }
    Error(error) -> {
      let _ = debug_error(debug, "Parse failed: " <> parse_error_to_string(error))
      ExecutionResult(data: None, errors: [
        ValidationError("Parse error: " <> parse_error_to_string(error), []),
      ])
    }
  }
}

fn parse_error_to_string(error: parser.ParseError) -> String {
  case error {
    parser.LexError(_) -> "Lexer error"
    parser.UnexpectedToken(expected, _got, _position) -> "Expected " <> expected
    parser.UnexpectedEOF(expected) -> "Unexpected EOF, expected " <> expected
  }
}

// Convenience function for simple queries
pub fn execute_query(
  schema_def: schema.Schema,
  query: String,
) -> ExecutionResult {
  case parser.parse(query) {
    Ok(document) -> {
      // Create a basic execution context with no DataLoaders
      let execution_context = schema.execution_context(create_string_dynamic("context"))
      
      execute(
        schema_def,
        document,
        None,
        execution_context,
        dict.new(),
      )
    }
    Error(_parse_error) ->
      ExecutionResult(data: None, errors: [
        ValidationError("Failed to parse query", []),
      ])
  }
}
