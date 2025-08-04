defmodule GeqlPhoenixBenchmarkWeb.Router do
  use GeqlPhoenixBenchmarkWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {GeqlPhoenixBenchmarkWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :graphql do
    plug :accepts, ["json"]
  end

  scope "/", GeqlPhoenixBenchmarkWeb do
    pipe_through :browser

    get "/", PageController, :home
  end

  # GraphQL API
  scope "/api" do
    pipe_through :graphql

    forward "/graphql", Absinthe.Plug,
      schema: GeqlPhoenixBenchmark.Schema

    # GraphiQL interface for development
    if Mix.env() == :dev do
      forward "/graphiql", Absinthe.Plug.GraphiQL,
        schema: GeqlPhoenixBenchmark.Schema,
        interface: :simple
    end
  end
end
