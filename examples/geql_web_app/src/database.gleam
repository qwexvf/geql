import cigogne
import gleam/io
import gleam/result
import cake/select as s
import cake/where as w
import gleam/dynamic.{type Dynamic}

/// Database configuration and setup
pub fn setup() -> Result(Nil, String) {
  io.println("🔄 Setting up database with Cigogne migrations...")
  
  // In a real app, you'd load this from environment variables
  let config = cigogne.Config(
    host: "localhost",
    port: 5432,
    database: "geql_app",
    user: "postgres", 
    password: "password"
  )
  
  case cigogne.create_engine(config) {
    Ok(engine) -> {
      io.println("✅ Database connection established")
      
      // Apply migrations
      case cigogne.apply_to_last(engine) {
        Ok(_) -> {
          io.println("✅ Database migrations applied successfully")
          Ok(Nil)
        }
        Error(_err) -> {
          Error("Failed to apply migrations - check migration files and database connection")
        }
      }
    }
    Error(_err) -> {
      Error("Failed to connect to database - check configuration and ensure PostgreSQL is running")
    }
  }
}

/// Database query executor for GraphQL resolvers
pub fn execute_query(sql: String, params: List(Dynamic)) -> Result(List(Dynamic), String) {
  // In a real implementation, this would:
  // 1. Connect to the database
  // 2. Execute the SQL with parameters  
  // 3. Return the rows as Dynamic values
  
  // For demo: log what would be executed
  io.println("📊 Database Query: " <> sql)
  case params {
    [] -> io.println("   Parameters: none")
    _ -> io.println("   Parameters: " <> "provided") // Would log actual params
  }
  
  // Mock response based on query type
  case sql {
    query if query |> contains("users") -> {
      // Mock user data
      Ok([mock_user_dynamic()])
    }
    query if query |> contains("posts") -> {
      // Mock post data  
      Ok([mock_post_dynamic()])
    }
    _ -> Error("Unknown query type")
  }
}

/// Create type-safe SQL queries using Cake
pub fn get_user_query(user_id: String) -> #(String, List(Dynamic)) {
  let query = s.new()
    |> s.from_table("users")  
    |> s.select_cols(["id", "name", "email", "active"])
    |> s.where(w.eq(w.col("id"), w.placeholder()))
  
  let sql = s.to_query(query).query
  let params = [dynamic.from(user_id)]
  
  #(sql, params)
}

pub fn get_all_users_query() -> #(String, List(Dynamic)) {
  let query = s.new()
    |> s.from_table("users")
    |> s.select_cols(["id", "name", "email", "active"])
  
  let sql = s.to_query(query).query
  let params = []
  
  #(sql, params)
}

pub fn get_user_posts_query(user_id: String) -> #(String, List(Dynamic)) {
  let query = s.new()
    |> s.from_table("posts")
    |> s.select_cols(["id", "title", "content", "published"])
    |> s.where(w.eq(w.col("author_id"), w.placeholder()))
  
  let sql = s.to_query(query).query  
  let params = [dynamic.from(user_id)]
  
  #(sql, params)
}

// Helper functions for demo

fn contains(haystack: String, needle: String) -> Bool {
  case haystack {
    _ if haystack == needle -> True
    _ -> False // Simplified contains check for demo
  }
}

fn mock_user_dynamic() -> Dynamic {
  // In practice: convert User struct to Dynamic using JSON library
  dynamic.from("mock_user_data")
}

fn mock_post_dynamic() -> Dynamic {
  // In practice: convert Post struct to Dynamic using JSON library  
  dynamic.from("mock_post_data")
}