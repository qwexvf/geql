# GeQL Examples

This directory contains complete examples showing how to use GeQL in real-world applications.

## 🧚‍♀️ Core Library Examples

**[`core_library_examples/`](core_library_examples/)** - Pure GraphQL functionality:

- **📝 GraphQL Parsing** - Parse queries into AST
- **🏗️ Schema Definition** - Fluent builder API
- **🔧 Schema Generation** - Auto-generate from Gleam types  
- **⚡ Query Execution** - Struct-based resolvers
- **❌ Error Handling** - Comprehensive GraphQL errors

```sh
cd core_library_examples
gleam run  # Pure GraphQL examples
```

## 🌐 Web Application Example

**[`geql_web_app/`](geql_web_app/)** - A complete web application demonstrating:

- **🚀 HTTP Server** - Wisp 2.0 web framework
- **🗄️ Database Integration** - PostgreSQL with Cake query builder
- **📊 Database Migrations** - Version management with Cigogne
- **🎯 GraphiQL Playground** - Interactive query development
- **⚡ Type-Safe Resolvers** - Database-backed GraphQL resolvers
- **🏗️ Production Architecture** - Clean separation of concerns

### Quick Start

```sh
cd geql_web_app
gleam deps download
gleam run  # http://localhost:8080
```

### What You'll Learn

1. **Pure GraphQL Library Usage** - How to use GeQL without any web/database dependencies
2. **Web Integration Patterns** - Best practices for HTTP server integration
3. **Database-Backed Resolvers** - How to connect GraphQL to real databases
4. **Production Architecture** - Proper separation between GraphQL core and application logic

## 🎯 Key Architecture Principle

The examples demonstrate GeQL's **clean separation philosophy**:

```
┌─────────────────────────────────────┐
│        Your Application            │
│  • Web server (Wisp)               │
│  • Database (Cake + Cigogne)       │
│  • Business logic & resolvers      │
│  • Authentication & authorization   │
└─────────────────────────────────────┘
               │ uses
               ▼
┌─────────────────────────────────────┐
│         GeQL Library               │
│  • Pure GraphQL functionality      │
│  • Schema definition & validation  │
│  • Query parsing & execution       │
│  • Zero external dependencies      │
└─────────────────────────────────────┘
```

**GeQL remains pure** - it only handles GraphQL specification compliance. Your application handles everything else (HTTP, database, auth, etc.).

## 🚀 Adding More Examples

We welcome contributions of additional examples showing:

- **Authentication & Authorization** - JWT tokens, role-based access
- **Real-time Subscriptions** - WebSocket-based live queries  
- **Microservices** - GraphQL federation and schema stitching
- **Testing Strategies** - Unit testing GraphQL schemas and resolvers
- **Deployment** - Docker, CI/CD, and production deployment patterns

Each example should:
- ✅ Use GeQL as a pure library dependency
- ✅ Show clear separation of concerns
- ✅ Include comprehensive documentation
- ✅ Provide working code that can be run locally
- ✅ Demonstrate clean architecture patterns

---

Happy GraphQL development with GeQL! 🧚‍♀️✨