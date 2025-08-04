defmodule GeqlPhoenixBenchmark.Schema do
  use Absinthe.Schema

  # Data types matching the Gleam GeQL implementation
  object :user do
    field :id, non_null(:id), description: "The user's unique identifier"
    field :name, :string, description: "The user's display name"
    field :email, :string, description: "The user's email address"
    field :active, :boolean, description: "Whether the user account is active"
    field :posts, list_of(:post), description: "Posts authored by this user" do
      resolve &GeqlPhoenixBenchmark.Resolvers.get_user_posts/3
    end
  end

  object :post do
    field :id, non_null(:id), description: "The post's unique identifier"
    field :title, :string, description: "The post's title"
    field :content, :string, description: "The post's content"
    field :published, :boolean, description: "Whether the post is published"
  end

  query do
    @desc "Get a specific user by ID"
    field :user, :user do
      arg :id, non_null(:id), description: "The user's ID"
      resolve &GeqlPhoenixBenchmark.Resolvers.get_user/3
    end

    @desc "Get all users"
    field :users, list_of(:user) do
      resolve &GeqlPhoenixBenchmark.Resolvers.get_all_users/3
    end

    @desc "Get a specific post by ID"
    field :post, :post do
      arg :id, non_null(:id)
      resolve &GeqlPhoenixBenchmark.Resolvers.get_post/3
    end

    @desc "Get all posts"
    field :posts, list_of(:post) do
      resolve &GeqlPhoenixBenchmark.Resolvers.get_all_posts/3
    end
  end

  # Optional: Add mutations for complete CRUD
  mutation do
    @desc "Create a new user"
    field :create_user, :user do
      arg :name, non_null(:string)
      arg :email, non_null(:string)
      resolve &GeqlPhoenixBenchmark.Resolvers.create_user/3
    end

    @desc "Create a new post"
    field :create_post, :post do
      arg :title, non_null(:string)
      arg :content, non_null(:string)
      arg :author_id, non_null(:id)
      resolve &GeqlPhoenixBenchmark.Resolvers.create_post/3
    end
  end
end