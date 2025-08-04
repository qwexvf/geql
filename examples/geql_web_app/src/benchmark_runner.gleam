// Benchmark runner for GeQL web app
// Provides a simple entry point for running benchmarks

import benchmark
import gleam/io
import gleam/string

pub fn main() {
  io.println("🚀 Starting GeQL Benchmarks...")
  io.println("")

  // Run the benchmark demo (simplified version due to timing limitations)
  benchmark.demo()

  io.println("")
  io.println("📋 Benchmark Summary")
  io.println("=" |> string.repeat(25))
  io.println("")
  io.println("GeQL is ready for HTTP benchmarking!")
  io.println("Start the server with: gleam run")
  io.println("Then use hey/wrk for load testing")
  io.println("")
  io.println("For comparison with Phoenix/Absinthe:")
  io.println("cd ../geql_phoenix_benchmark && mix phx.server")
}
