# GeQL Web Application Example

This is a complete example web application demonstrating how to use the **GeQL GraphQL library** with:

- 🌐 **Wisp 2.0** - Modern web framework for Gleam
- 🗄️ **Cake** - Type-safe SQL query builder  
- 🔄 **Cigogne** - Database migrations
- 📊 **PostgreSQL** - Production database
- 🎯 **GraphiQL** - Interactive query playground

## 🏗️ Architecture

This example shows the **proper separation of concerns**:

```
┌─────────────────────────────────────┐
│        Web Application             │
│  • HTTP server (Wisp)              │
│  • Database (Cake + Cigogne)       │  
│  • Business logic & resolvers      │
└─────────────────────────────────────┘
               │ uses
               ▼
┌─────────────────────────────────────┐
│         GeQL Library               │
│  • Pure GraphQL functionality      │
│  • Schema definition               │
│  • Query parsing & execution       │
└─────────────────────────────────────┘
```

The **GeQL library remains pure** - it only handles GraphQL concerns. The web application handles HTTP, database, and business logic.

## 🚀 Quick Start

### Prerequisites

- **Gleam** installed
- **PostgreSQL** running locally
- **Database** named `geql_app` created

### Setup

1. **Navigate to example directory**:
   ```sh
   cd examples/geql_web_app
   ```

2. **Install dependencies**:
   ```sh
   gleam deps download
   ```

3. **Configure database** (update `src/database.gleam` if needed):
   ```gleam
   let config = cigogne.Config(
     host: "localhost",
     port: 5432,
     database: "geql_app", 
     user: "postgres",
     password: "password"
   )
   ```

4. **Run database migrations**:
   ```sh
   gleam run -m cigogne last
   ```

5. **Start the server**:
   ```sh
   gleam run
   ```

6. **Open your browser**:
   ```
   http://localhost:8080          # Welcome page
   http://localhost:8080/graphiql # GraphiQL playground
   ```

## 🎯 Example Queries

### Get a specific user
```graphql
{
  user(id: "1") {
    id
    name
    email
    active
  }
}
```

### Get user with their posts
```graphql
{
  user(id: "1") {
    name
    email
    posts {
      title
      content
      published
    }
  }
}
```

### Get all users
```graphql
{
  users {
    id
    name
    email
    active
  }
}
```

## 📁 Project Structure

```
examples/geql_web_app/
├── gleam.toml                  # Dependencies including GeQL
├── src/
│   ├── geql_web_app.gleam     # Main web server
│   ├── database.gleam         # Database connection & Cake queries
│   └── schema_builder.gleam   # GraphQL schema with resolvers
├── migrations/
│   ├── 0001_create_users_table.sql
│   └── 0002_create_posts_table.sql
└── README.md
```

## 🔧 Key Features

### 1. Type-Safe Database Queries

Uses **Cake** for compile-time SQL safety:

```gleam
pub fn get_user_query(user_id: String) -> #(String, List(Dynamic)) {
  let query = s.new()
    |> s.from_table("users")  
    |> s.select_cols(["id", "name", "email", "active"])
    |> s.where(w.eq(w.col("id"), w.placeholder()))
  
  let sql = s.to_query(query).query
  let params = [dynamic.from(user_id)]
  
  #(sql, params)
}
```

### 2. Database-Backed GraphQL Resolvers

Resolvers that execute real database queries:

```gleam
fn get_user_resolver(info: schema.ResolverInfo) -> Result(Dynamic, String) {
  case schema.get_argument(info.arguments, "id") {
    Ok(user_id_dynamic) -> {
      let user_id = extract_string_from_dynamic(user_id_dynamic)
      let #(sql, params) = database.get_user_query(user_id)
      
      case database.execute_query(sql, params) {
        Ok([user_data, ..]) -> Ok(user_data)
        Ok([]) -> Error("User not found")
        Error(err) -> Error("Database error: " <> err)
      }
    }
    Error(_) -> Error("Missing required argument: id")
  }
}
```

### 3. Migration Management

**Cigogne** handles database versioning:

```sql
-- migrations/0001_create_users_table.sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    active BOOLEAN DEFAULT TRUE
);
```

### 4. GraphiQL Integration

Interactive query playground at `/graphiql` for development and testing.

## 🗄️ Database Schema

### Users Table
```sql
users {
  id: SERIAL PRIMARY KEY
  name: VARCHAR(255) NOT NULL
  email: VARCHAR(255) UNIQUE NOT NULL  
  active: BOOLEAN DEFAULT TRUE
  created_at: TIMESTAMP
  updated_at: TIMESTAMP
}
```

### Posts Table
```sql
posts {
  id: SERIAL PRIMARY KEY
  title: VARCHAR(500) NOT NULL
  content: TEXT
  author_id: INTEGER REFERENCES users(id)
  published: BOOLEAN DEFAULT FALSE
  created_at: TIMESTAMP
  updated_at: TIMESTAMP
}
```

## 📈 Production Considerations

### 1. Environment Configuration
```gleam
// Load from environment variables
let config = cigogne.Config(
  host: get_env("DB_HOST", "localhost"),
  port: get_env_int("DB_PORT", 5432),
  database: get_env("DB_NAME", "geql_app"),
  user: get_env("DB_USER", "postgres"),
  password: get_env("DB_PASSWORD", "password")
)
```

### 2. Connection Pooling
Consider using a connection pool for production database access.

### 3. Error Handling
The example shows basic error handling - expand for production use.

### 4. Authentication & Authorization
Add authentication middleware and authorization checks in resolvers.

### 5. Caching
Consider adding Redis or in-memory caching for frequently accessed data.

## 🤝 Benefits of This Architecture

1. **🎯 Pure GraphQL Library** - GeQL focuses only on GraphQL specification
2. **🔌 Flexible Integration** - Easy to swap web frameworks or databases
3. **⚡ Type Safety** - Compile-time safety for both GraphQL and SQL
4. **📦 Clean Dependencies** - No database code polluting the GraphQL library
5. **🔄 Easy Testing** - Clear separation allows isolated unit tests

## 🚀 Next Steps

- Add authentication with JWT tokens
- Implement mutations for data modification
- Add real-time subscriptions with WebSockets
- Deploy to production with Docker
- Add comprehensive error handling and logging

---

This example demonstrates how to build a GraphQL API with Gleam while keeping the core GraphQL library pure and focused! 🧚‍♀️✨