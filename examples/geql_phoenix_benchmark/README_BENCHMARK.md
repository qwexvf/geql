# GeQL Phoenix/Absinthe Benchmark

This Phoenix application provides performance benchmarking capabilities to compare [Gleam GeQL](../../) with the established Phoenix/Absinthe GraphQL stack.

## 🎯 Purpose

- **Performance Comparison**: Measure parsing, execution, and memory usage between GeQL and Absinthe
- **Feature Parity Testing**: Ensure equivalent GraphQL functionality across both implementations  
- **Real-world Benchmarking**: HTTP server performance under load
- **Development Reference**: Show idiomatic Absinthe patterns for comparison

## 🏗️ Architecture

```
┌─────────────────────────────────────┐
│           Phoenix Web App           │
│                                     │
│  ┌─────────────┐  ┌─────────────┐   │
│  │   GraphQL   │  │ Benchmarking│   │
│  │   Schema    │  │   Module    │   │
│  │             │  │             │   │
│  │ • Users     │  │ • Parsing   │   │
│  │ • Posts     │  │ • Execution │   │
│  │ • Queries   │  │ • Memory    │   │
│  │ • Mutations │  │ • HTTP Load │   │
│  └─────────────┘  └─────────────┘   │
│                                     │
│  ┌─────────────────────────────────┐ │
│  │        Absinthe GraphQL         │ │
│  │   • Schema Definition           │ │
│  │   • Query Parsing & Execution  │ │
│  │   • Resolver Management        │ │
│  │   • Error Handling             │ │
│  └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

## 🚀 Quick Start

### Setup

```bash
# Install dependencies
mix deps.get

# Start the development server
mix phx.server
```

The application will be available at:
- **GraphQL Endpoint**: http://localhost:4000/api/graphql
- **GraphiQL Interface**: http://localhost:4000/api/graphiql
- **Web Interface**: http://localhost:4000

### Running Benchmarks

```bash
# Internal Elixir benchmarks (parsing, execution)
mix run lib/benchmark.ex

# HTTP load testing (requires hey or wrk)
hey -n 1000 -c 10 -m POST \
    -H "Content-Type: application/json" \
    -d '{"query":"{ user(id: \"1\") { id name email } }"}' \
    http://localhost:4000/api/graphql
```

## 📊 GraphQL Schema

The schema mirrors the Gleam GeQL implementation for direct comparison:

### Types

```graphql
type User {
  id: ID!
  name: String
  email: String  
  active: Boolean
  posts: [Post!]
}

type Post {
  id: ID!
  title: String
  content: String
  published: Boolean
}
```

### Queries

```graphql
type Query {
  user(id: ID!): User
  users: [User!]
  post(id: ID!): Post  
  posts: [Post!]
}
```

### Mutations

```graphql
type Mutation {
  createUser(name: String!, email: String!): User
  createPost(title: String!, content: String!, authorId: ID!): Post
}
```

## 🧪 Example Queries

### Simple User Query
```graphql
{
  user(id: "1") {
    id
    name
    email
  }
}
```

### Complex Nested Query
```graphql
{
  user(id: "1") {
    id
    name
    email
    active
    posts {
      id
      title
      content
      published
    }
  }
}
```

### Multiple Users with Posts
```graphql
{
  users {
    id
    name
    posts {
      id
      title
    }
  }
}
```

## 📈 Benchmark Categories

### 1. Parsing Performance
- Query string → AST conversion
- Schema validation  
- Syntax error handling

### 2. Execution Performance
- Resolver execution
- Field resolution
- Data serialization

### 3. Memory Usage
- Peak memory consumption
- Memory allocation patterns
- Garbage collection impact

### 4. HTTP Performance
- Requests per second
- Latency percentiles
- Concurrent connection handling

## 🔧 Comparative Analysis

### Phoenix/Absinthe Strengths
- **Mature Ecosystem**: Extensive tooling and middleware
- **Built-in Features**: Subscriptions, DataLoader, introspection
- **Developer Experience**: GraphiQL, detailed error messages
- **Production Ready**: Battle-tested in production environments

### Gleam GeQL Strengths  
- **Type Safety**: Compile-time guarantees
- **Performance**: Lower memory footprint, faster parsing
- **Simplicity**: Zero-dependency core library
- **Functional Purity**: Predictable, side-effect-free operations

### Performance Expectations

Based on language characteristics:

| Metric | Phoenix/Absinthe | Gleam GeQL | Winner |
|--------|------------------|------------|--------|  
| **Parsing Speed** | Good | Excellent | GeQL |
| **Memory Usage** | Higher (BEAM) | Lower (Native) | GeQL |
| **Cold Start** | Slower | Faster | GeQL |
| **Throughput** | Excellent | Good | Absinthe |
| **Ecosystem** | Excellent | Developing | Absinthe |

## 🏃 Running Comparative Tests

### 1. Start Both Servers

```bash
# Terminal 1: Phoenix/Absinthe
cd examples/geql_phoenix_benchmark
mix phx.server  # http://localhost:4000

# Terminal 2: Gleam GeQL  
cd examples/geql_web_app
gleam run      # http://localhost:8080
```

### 2. HTTP Load Testing

```bash
# Test Phoenix/Absinthe
hey -n 5000 -c 50 -m POST \
    -H "Content-Type: application/json" \
    -d '{"query":"{ users { id name } }"}' \
    http://localhost:4000/api/graphql

# Test Gleam GeQL (when HTTP server is implemented)
hey -n 5000 -c 50 -m POST \
    -H "Content-Type: application/json" \
    -d '{"query":"{ users { id name } }"}' \
    http://localhost:8080/graphql
```

### 3. Memory Profiling

```bash
# Phoenix memory usage
:observer.start()  # In IEx session

# Gleam memory usage  
valgrind --tool=massif gleam run  # If compiled to native
```

## 📋 Development Notes

### Mock Data
Both implementations use identical in-memory data sets:
- 5 sample users with realistic data
- 5 sample posts with author relationships
- No database dependencies for pure GraphQL performance testing

### Resolver Pattern
- **Absinthe**: Standard 3-arity resolver functions `(parent, args, resolution)`
- **GeQL**: Custom resolver info structure with Dynamic types

### Error Handling  
- **Absinthe**: Built-in error formatting and GraphQL spec compliance
- **GeQL**: Custom error types with Result-based handling

## 🤝 Contributing

When adding benchmarks:

1. **Keep Parity**: Mirror any GeQL schema changes
2. **Realistic Data**: Use representative query patterns  
3. **Fair Testing**: Equivalent work loads for both implementations
4. **Document Results**: Include performance analysis in commits

## 📚 Resources

- [Absinthe Documentation](https://hexdocs.pm/absinthe/)
- [Phoenix Framework](https://phoenixframework.org/)
- [Benchee (Elixir Benchmarking)](https://github.com/bencheeorg/benchee)
- [GraphQL Specification](https://spec.graphql.org/)

---

**Benchmark Results**: Add your performance comparison results here as you run tests!