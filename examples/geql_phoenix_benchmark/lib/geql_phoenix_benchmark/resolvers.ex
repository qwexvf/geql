defmodule GeqlPhoenixBenchmark.Resolvers do
  @moduledoc """
  GraphQL resolvers for the benchmark application.
  Uses in-memory data for performance testing - no database calls.
  """

  # Mock data - same structure as Gleam GeQL example
  @users [
    %{id: "1", name: "Alice Johnson", email: "alice@example.com", active: true},
    %{id: "2", name: "Bob Smith", email: "bob@example.com", active: true},
    %{id: "3", name: "Carol Brown", email: "carol@example.com", active: false},
    %{id: "4", name: "David Wilson", email: "david@example.com", active: true},
    %{id: "5", name: "Eve Davis", email: "eve@example.com", active: true}
  ]

  @posts [
    %{id: "1", title: "Getting Started with GraphQL", content: "GraphQL is a powerful query language...", author_id: "1", published: true},
    %{id: "2", title: "Phoenix and Absinthe Tutorial", content: "Learn how to build GraphQL APIs...", author_id: "1", published: true},
    %{id: "3", title: "Functional Programming Patterns", content: "Exploring functional programming...", author_id: "2", published: true},
    %{id: "4", title: "Database Optimization Tips", content: "How to optimize your database queries...", author_id: "3", published: false},
    %{id: "5", title: "Gleam vs Elixir", content: "Comparing two functional languages...", author_id: "2", published: true}
  ]

  # Query resolvers
  def get_user(_parent, %{id: id}, _resolution) do
    case Enum.find(@users, &(&1.id == id)) do
      nil -> {:error, "User not found"}
      user -> {:ok, user}
    end
  end

  def get_all_users(_parent, _args, _resolution) do
    {:ok, @users}
  end

  def get_post(_parent, %{id: id}, _resolution) do
    case Enum.find(@posts, &(&1.id == id)) do
      nil -> {:error, "Post not found"}
      post -> {:ok, post}
    end
  end

  def get_all_posts(_parent, _args, _resolution) do
    {:ok, @posts}
  end

  # Field resolvers for nested data
  def get_user_posts(%{id: user_id}, _args, _resolution) do
    posts = Enum.filter(@posts, &(&1.author_id == user_id))
    {:ok, posts}
  end

  # Mutation resolvers
  def create_user(_parent, args, _resolution) do
    new_id = :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
    user = %{
      id: new_id,
      name: args.name,
      email: args.email,
      active: true
    }
    {:ok, user}
  end

  def create_post(_parent, args, _resolution) do
    new_id = :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
    post = %{
      id: new_id,
      title: args.title,
      content: args.content,
      author_id: args.author_id,
      published: false
    }
    {:ok, post}
  end
end