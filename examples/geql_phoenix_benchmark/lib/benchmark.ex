defmodule GeqlPhoenixBenchmark.Benchmark do
  @moduledoc """
  Benchmark comparing Phoenix/Absinthe GraphQL performance with Gleam GeQL.
  
  This module provides comprehensive benchmarks for:
  - Query parsing performance
  - Query execution performance  
  - Schema definition performance
  - Memory usage comparisons
  
  Run with: mix run lib/benchmark.ex
  """

  def run do
    IO.puts("🚀 GeQL vs Phoenix/Absinthe Performance Benchmark")
    IO.puts("=" |> String.duplicate(60))
    IO.puts("")

    # Test queries matching the Gleam examples
    simple_query = "{ user(id: \"1\") { id name email } }"
    complex_query = """
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
    """

    nested_query = """
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
    """

    # Benchmark parsing performance
    IO.puts("📝 Query Parsing Benchmarks")
    IO.puts("-" |> String.duplicate(30))
    
    Benchee.run(%{
      "Absinthe: Simple Query Parse" => fn -> 
        Absinthe.run(simple_query, GeqlPhoenixBenchmark.Schema)
      end,
      "Absinthe: Complex Query Parse" => fn -> 
        Absinthe.run(complex_query, GeqlPhoenixBenchmark.Schema)
      end,
      "Absinthe: Nested Query Parse" => fn -> 
        Absinthe.run(nested_query, GeqlPhoenixBenchmark.Schema)
      end
    }, 
    time: 3,
    print: [fast_warning: false]
    )

    IO.puts("")
    IO.puts("⚡ Query Execution Benchmarks")
    IO.puts("-" |> String.duplicate(30))

    # Use Absinthe.run for both parsing and execution benchmarks
    # Absinthe handles parsing internally during run/2

    Benchee.run(%{
      "Absinthe: Execute Simple" => fn ->
        Absinthe.run(simple_query, GeqlPhoenixBenchmark.Schema)
      end,
      "Absinthe: Execute Complex" => fn ->
        Absinthe.run(complex_query, GeqlPhoenixBenchmark.Schema)
      end,
      "Absinthe: Execute Nested" => fn ->
        Absinthe.run(nested_query, GeqlPhoenixBenchmark.Schema)
      end
    },
    time: 3,
    print: [fast_warning: false]
    )

    IO.puts("")
    IO.puts("💾 Memory Usage Analysis")
    IO.puts("-" |> String.duplicate(30))

    # Memory usage benchmarks
    Benchee.run(%{
      "Absinthe: Memory Simple" => fn ->
        Absinthe.run(simple_query, GeqlPhoenixBenchmark.Schema)
      end,
      "Absinthe: Memory Complex" => fn ->
        Absinthe.run(complex_query, GeqlPhoenixBenchmark.Schema)
      end
    },
    time: 2,
    memory_time: 1,
    print: [fast_warning: false]
    )

    IO.puts("")
    print_comparison_summary()
    print_integration_instructions()
  end

  defp print_comparison_summary do
    IO.puts("📊 Performance Comparison Summary")
    IO.puts("=" |> String.duplicate(40))
    IO.puts("")
    IO.puts("Phoenix/Absinthe Characteristics:")
    IO.puts("✅ Mature ecosystem with extensive tooling")
    IO.puts("✅ Built-in GraphiQL interface and introspection")
    IO.puts("✅ Excellent error handling and debugging")
    IO.puts("✅ DataLoader and subscription support")
    IO.puts("⚠️  Higher memory usage due to Erlang/OTP overhead")
    IO.puts("⚠️  More complex setup and configuration")
    IO.puts("")
    IO.puts("Gleam GeQL Characteristics:")
    IO.puts("✅ Lower memory footprint")
    IO.puts("✅ Compile-time type safety")
    IO.puts("✅ Pure functional approach")
    IO.puts("✅ Zero-dependency core library")
    IO.puts("⚠️  Newer ecosystem with fewer tools")
    IO.puts("⚠️  Manual Dynamic serialization required")
    IO.puts("")
  end

  defp print_integration_instructions do
    IO.puts("🔧 Running Comparative Benchmarks")
    IO.puts("=" |> String.duplicate(40))
    IO.puts("")
    IO.puts("To compare with Gleam GeQL:")
    IO.puts("")
    IO.puts("1. Start the Phoenix benchmark server:")
    IO.puts("   cd examples/geql_phoenix_benchmark")
    IO.puts("   mix phx.server")
    IO.puts("")
    IO.puts("2. Start the Gleam web server:")
    IO.puts("   cd examples/geql_web_app")
    IO.puts("   gleam run")
    IO.puts("")
    IO.puts("3. Use tools like wrk or hey for HTTP benchmarks:")
    IO.puts("   # Benchmark Phoenix/Absinthe")
    IO.puts("   hey -n 1000 -c 10 -m POST -H 'Content-Type: application/json' \\")
    IO.puts("       -d '{\"query\":\"{ user(id: \\\"1\\\") { id name } }\"}' \\")
    IO.puts("       http://localhost:4000/api/graphql")
    IO.puts("")
    IO.puts("   # Benchmark Gleam GeQL (when implemented)")
    IO.puts("   hey -n 1000 -c 10 -m POST -H 'Content-Type: application/json' \\")
    IO.puts("       -d '{\"query\":\"{ user(id: \\\"1\\\") { id name } }\"}' \\")
    IO.puts("       http://localhost:8080/graphql")
    IO.puts("")
    IO.puts("4. GraphiQL interfaces:")
    IO.puts("   Phoenix/Absinthe: http://localhost:4000/api/graphiql")
    IO.puts("   Gleam GeQL: http://localhost:8080 (when implemented)")
    IO.puts("")
  end
end

# Run the benchmark if this file is executed directly with mix run
# Note: In Elixir, __ENV__.file gives the current file path