import gleam/io
import wisp
import mist
import gleam/erlang/process
import gleam/bytes_builder
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/result
import gleam/string
import geql
import geql/executor
import database
import schema_builder

/// Main entry point for the GraphQL web application
pub fn main() {
  io.println("🚀 Starting GeQL Web Application...")
  
  // Initialize database
  case database.setup() {
    Ok(_) -> io.println("✅ Database initialized successfully")
    Error(err) -> {
      io.println("❌ Database setup failed: " <> err)
      panic as "Failed to setup database"
    }
  }
  
  // Create GraphQL schema
  let schema = schema_builder.create_schema()
  io.println("✅ GraphQL schema created")
  
  // Start web server
  let assert Ok(_) = 
    wisp.mist_handler(handle_request(_, schema), "secret_key")
    |> mist.new
    |> mist.port(8080)
    |> mist.start_http
  
  io.println("🌐 GraphQL server running on http://localhost:8080")
  io.println("📍 GraphQL endpoint: POST /graphql")
  io.println("📍 GraphiQL playground: GET /graphiql")
  
  process.sleep_forever()
}

/// HTTP request handler
fn handle_request(req: Request(BitArray), schema: geql.Schema) -> Response(BitArray) {
  case wisp.path_segments(req) {
    ["graphql"] -> handle_graphql(req, schema)
    ["graphiql"] -> handle_graphiql(req)
    [] -> handle_root(req)
    _ -> wisp.not_found()
  }
}

/// Handle GraphQL queries
fn handle_graphql(req: Request(BitArray), schema: geql.Schema) -> Response(BitArray) {
  case req.method {
    http.Post -> {
      case wisp.get_query(req) {
        Ok(query_string) -> {
          // Execute GraphQL query
          let result = executor.execute_query(schema, query_string)
          
          // Convert result to JSON response
          let json_response = case result.data {
            Some(_data) -> json.object([
              #("data", json.object([
                #("message", json.string("Query executed successfully"))
              ])),
              #("errors", json.null())
            ])
            None -> json.object([
              #("data", json.null()),
              #("errors", json.array([], json.string))
            ])
          }
          
          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(
            json.to_string_builder(json_response)
            |> bytes_builder.to_bit_array
          )
        }
        Error(_) -> wisp.bad_request()
      }
    }
    _ -> wisp.method_not_allowed([http.Post])
  }
}

/// Handle GraphiQL playground
fn handle_graphiql(_req: Request(BitArray)) -> Response(BitArray) {
  let html = "
<!DOCTYPE html>
<html>
<head>
  <title>GraphiQL - GeQL Explorer</title>
  <style>
    body { margin: 0; }
    #graphiql { height: 100vh; }
  </style>
  <script src=\"https://unpkg.com/react@16/umd/react.development.js\"></script>
  <script src=\"https://unpkg.com/react-dom@16/umd/react-dom.development.js\"></script>
  <script src=\"https://unpkg.com/graphiql/graphiql.min.js\"></script>
  <link rel=\"stylesheet\" href=\"https://unpkg.com/graphiql/graphiql.min.css\" />
</head>
<body>
  <div id=\"graphiql\">Loading...</div>
  <script>
    const fetcher = (graphQLParams) => {
      return fetch('/graphql', {
        method: 'POST',
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(graphQLParams),
      })
      .then(response => response.json())
      .catch(() => response.text());
    };

    ReactDOM.render(
      React.createElement(GraphiQL, { fetcher: fetcher }),
      document.getElementById('graphiql'),
    );
  </script>
</body>
</html>
  "
  
  response.new(200)
  |> response.set_header("content-type", "text/html")
  |> response.set_body(bytes_builder.from_string(html) |> bytes_builder.to_bit_array)
}

/// Handle root endpoint
fn handle_root(_req: Request(BitArray)) -> Response(BitArray) {
  let html = "
<!DOCTYPE html>
<html>
<head>
  <title>GeQL Web App</title>
</head>
<body>
  <h1>🧚‍♀️ GeQL GraphQL Web Application</h1>
  <p>Welcome to the GeQL example web application!</p>
  <ul>
    <li><a href=\"/graphql\">GraphQL Endpoint</a> (POST queries here)</li>
    <li><a href=\"/graphiql\">GraphiQL Playground</a> (Interactive query explorer)</li>
  </ul>
  <h2>Example Query</h2>
  <pre>
{
  user(id: \"1\") {
    id
    name
    email
    active
  }
}
  </pre>
</body>
</html>
  "
  
  response.new(200)
  |> response.set_header("content-type", "text/html")
  |> response.set_body(bytes_builder.from_string(html) |> bytes_builder.to_bit_array)
}