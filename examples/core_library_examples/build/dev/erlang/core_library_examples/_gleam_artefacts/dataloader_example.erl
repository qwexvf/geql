-module(dataloader_example).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/dataloader_example.gleam").
-export([create_dataloader_schema/0, create_execution_context_with_dataloaders/0, run_dataloader_example/0]).
-export_type([user/0, post/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type user() :: {user, binary(), binary(), binary()}.

-type post() :: {post, binary(), binary(), binary(), binary()}.

-file("src/dataloader_example.gleam", 251).
?DOC(" Demo placeholder for Dynamic values - shows where proper serialization would happen\n").
-spec create_demo_dynamic(any()) -> gleam@dynamic:dynamic_().
create_demo_dynamic(_) ->
    erlang:error(#{gleam_error => panic,
            message => <<"Demo limitation: DataLoader requires Dynamic serialization (use gleam_json in real implementations)"/utf8>>,
            file => <<?FILEPATH/utf8>>,
            module => <<"dataloader_example"/utf8>>,
            function => <<"create_demo_dynamic"/utf8>>,
            line => 259}).

-file("src/dataloader_example.gleam", 262).
-spec user_to_dynamic(user()) -> gleam@dynamic:dynamic_().
user_to_dynamic(User) ->
    create_demo_dynamic(<<"user:"/utf8, (erlang:element(2, User))/binary>>).

-file("src/dataloader_example.gleam", 267).
-spec users_to_dynamic(list(user())) -> gleam@dynamic:dynamic_().
users_to_dynamic(Users_list) ->
    User_dynamics = gleam@list:map(Users_list, fun user_to_dynamic/1),
    create_demo_dynamic(User_dynamics).

-file("src/dataloader_example.gleam", 277).
-spec post_to_dynamic(post()) -> gleam@dynamic:dynamic_().
post_to_dynamic(Post) ->
    create_demo_dynamic(<<"post:"/utf8, (erlang:element(2, Post))/binary>>).

-file("src/dataloader_example.gleam", 272).
-spec posts_to_dynamic(list(post())) -> gleam@dynamic:dynamic_().
posts_to_dynamic(Posts_list) ->
    Post_dynamics = gleam@list:map(Posts_list, fun post_to_dynamic/1),
    create_demo_dynamic(Post_dynamics).

-file("src/dataloader_example.gleam", 282).
-spec dynamic_to_user(gleam@dynamic:dynamic_()) -> user().
dynamic_to_user(_) ->
    {user, <<"1"/utf8>>, <<"Mock User"/utf8>>, <<"mock@example.com"/utf8>>}.

-file("src/dataloader_example.gleam", 155).
-spec user_id_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
user_id_resolver(Info) ->
    case erlang:element(2, Info) of
        {some, Parent_user} ->
            User = dynamic_to_user(Parent_user),
            {ok, create_demo_dynamic(erlang:element(2, User))};

        none ->
            {error, <<"No parent user"/utf8>>}
    end.

-file("src/dataloader_example.gleam", 165).
-spec user_name_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
user_name_resolver(Info) ->
    case erlang:element(2, Info) of
        {some, Parent_user} ->
            User = dynamic_to_user(Parent_user),
            {ok, create_demo_dynamic(erlang:element(3, User))};

        none ->
            {error, <<"No parent user"/utf8>>}
    end.

-file("src/dataloader_example.gleam", 175).
-spec user_email_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
user_email_resolver(Info) ->
    case erlang:element(2, Info) of
        {some, Parent_user} ->
            User = dynamic_to_user(Parent_user),
            {ok, create_demo_dynamic(erlang:element(4, User))};

        none ->
            {error, <<"No parent user"/utf8>>}
    end.

-file("src/dataloader_example.gleam", 186).
?DOC(" This resolver uses DataLoader to efficiently batch load posts\n").
-spec user_posts_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
user_posts_resolver(Info) ->
    case erlang:element(2, Info) of
        {some, Parent_user} ->
            User = dynamic_to_user(Parent_user),
            case geql@schema:get_data_loader(
                erlang:element(4, Info),
                <<"posts_by_author"/utf8>>
            ) of
                {ok, Posts_loader} ->
                    {_, Result} = geql@dataloader:load(
                        Posts_loader,
                        create_demo_dynamic(erlang:element(2, User))
                    ),
                    Result;

                {error, Err} ->
                    {error, <<"DataLoader error: "/utf8, Err/binary>>}
            end;

        none ->
            {error, <<"No parent user"/utf8>>}
    end.

-file("src/dataloader_example.gleam", 287).
-spec dynamic_to_post(gleam@dynamic:dynamic_()) -> post().
dynamic_to_post(_) ->
    {post,
        <<"1"/utf8>>,
        <<"Mock Post"/utf8>>,
        <<"1"/utf8>>,
        <<"Mock content"/utf8>>}.

-file("src/dataloader_example.gleam", 208).
-spec post_id_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
post_id_resolver(Info) ->
    case erlang:element(2, Info) of
        {some, Parent_post} ->
            Post = dynamic_to_post(Parent_post),
            {ok, create_demo_dynamic(erlang:element(2, Post))};

        none ->
            {error, <<"No parent post"/utf8>>}
    end.

-file("src/dataloader_example.gleam", 218).
-spec post_title_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
post_title_resolver(Info) ->
    case erlang:element(2, Info) of
        {some, Parent_post} ->
            Post = dynamic_to_post(Parent_post),
            {ok, create_demo_dynamic(erlang:element(3, Post))};

        none ->
            {error, <<"No parent post"/utf8>>}
    end.

-file("src/dataloader_example.gleam", 229).
?DOC(" This resolver uses DataLoader to efficiently batch load users\n").
-spec post_author_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
post_author_resolver(Info) ->
    case erlang:element(2, Info) of
        {some, Parent_post} ->
            Post = dynamic_to_post(Parent_post),
            case geql@schema:get_data_loader(
                erlang:element(4, Info),
                <<"users"/utf8>>
            ) of
                {ok, Users_loader} ->
                    {_, Result} = geql@dataloader:load(
                        Users_loader,
                        create_demo_dynamic(erlang:element(4, Post))
                    ),
                    Result;

                {error, Err} ->
                    {error, <<"DataLoader error: "/utf8, Err/binary>>}
            end;

        none ->
            {error, <<"No parent post"/utf8>>}
    end.

-file("src/dataloader_example.gleam", 38).
?DOC(" Batch load function for users - simulates database query\n").
-spec batch_load_users(list(gleam@dynamic:dynamic_())) -> {ok,
        list({ok, gleam@dynamic:dynamic_()} | {error, binary()})} |
    {error, binary()}.
batch_load_users(User_ids) ->
    gleam_stdlib:println(
        <<<<"🔄 BATCH LOADING USERS: "/utf8,
                (erlang:integer_to_binary(erlang:length(User_ids)))/binary>>/binary,
            " users"/utf8>>
    ),
    String_ids = gleam@list:map(User_ids, fun(_) -> <<"mock_id"/utf8>> end),
    Results = gleam@list:map(
        String_ids,
        fun(Id) ->
            case gleam@list:find(
                [{user,
                        <<"1"/utf8>>,
                        <<"Alice Johnson"/utf8>>,
                        <<"alice@example.com"/utf8>>},
                    {user,
                        <<"2"/utf8>>,
                        <<"Bob Smith"/utf8>>,
                        <<"bob@example.com"/utf8>>},
                    {user,
                        <<"3"/utf8>>,
                        <<"Carol Davis"/utf8>>,
                        <<"carol@example.com"/utf8>>}],
                fun(User) -> erlang:element(2, User) =:= Id end
            ) of
                {ok, User@1} ->
                    {ok, user_to_dynamic(User@1)};

                {error, _} ->
                    {error, <<"User not found: "/utf8, Id/binary>>}
            end
        end
    ),
    {ok, Results}.

-file("src/dataloader_example.gleam", 150).
-spec get_users_resolver(geql@schema:resolver_info()) -> {ok,
        gleam@dynamic:dynamic_()} |
    {error, binary()}.
get_users_resolver(_) ->
    {ok,
        users_to_dynamic(
            [{user,
                    <<"1"/utf8>>,
                    <<"Alice Johnson"/utf8>>,
                    <<"alice@example.com"/utf8>>},
                {user,
                    <<"2"/utf8>>,
                    <<"Bob Smith"/utf8>>,
                    <<"bob@example.com"/utf8>>},
                {user,
                    <<"3"/utf8>>,
                    <<"Carol Davis"/utf8>>,
                    <<"carol@example.com"/utf8>>}]
        )}.

-file("src/dataloader_example.gleam", 72).
?DOC(" Create a schema with DataLoader-backed resolvers\n").
-spec create_dataloader_schema() -> geql@schema:schema().
create_dataloader_schema() ->
    User_type = begin
        _pipe = geql@schema:object(<<"User"/utf8>>),
        _pipe@1 = geql@schema:description(
            _pipe,
            <<"A user in the system"/utf8>>
        ),
        _pipe@4 = geql@schema:field(
            _pipe@1,
            begin
                _pipe@2 = geql@schema:field_def(
                    <<"id"/utf8>>,
                    geql@schema:non_null(geql@schema:id_type())
                ),
                _pipe@3 = geql@schema:field_description(
                    _pipe@2,
                    <<"User ID"/utf8>>
                ),
                geql@schema:resolver(_pipe@3, fun user_id_resolver/1)
            end
        ),
        _pipe@7 = geql@schema:field(
            _pipe@4,
            begin
                _pipe@5 = geql@schema:field_def(
                    <<"name"/utf8>>,
                    geql@schema:string_type()
                ),
                _pipe@6 = geql@schema:field_description(
                    _pipe@5,
                    <<"User name"/utf8>>
                ),
                geql@schema:resolver(_pipe@6, fun user_name_resolver/1)
            end
        ),
        _pipe@10 = geql@schema:field(
            _pipe@7,
            begin
                _pipe@8 = geql@schema:field_def(
                    <<"email"/utf8>>,
                    geql@schema:string_type()
                ),
                _pipe@9 = geql@schema:field_description(
                    _pipe@8,
                    <<"User email"/utf8>>
                ),
                geql@schema:resolver(_pipe@9, fun user_email_resolver/1)
            end
        ),
        geql@schema:field(
            _pipe@10,
            begin
                _pipe@11 = geql@schema:field_def(
                    <<"posts"/utf8>>,
                    geql@schema:list_type(
                        geql@schema:named_type(<<"Post"/utf8>>)
                    )
                ),
                _pipe@12 = geql@schema:field_description(
                    _pipe@11,
                    <<"Posts by this user"/utf8>>
                ),
                geql@schema:resolver(_pipe@12, fun user_posts_resolver/1)
            end
        )
    end,
    Post_type = begin
        _pipe@13 = geql@schema:object(<<"Post"/utf8>>),
        _pipe@14 = geql@schema:description(_pipe@13, <<"A blog post"/utf8>>),
        _pipe@17 = geql@schema:field(
            _pipe@14,
            begin
                _pipe@15 = geql@schema:field_def(
                    <<"id"/utf8>>,
                    geql@schema:non_null(geql@schema:id_type())
                ),
                _pipe@16 = geql@schema:field_description(
                    _pipe@15,
                    <<"Post ID"/utf8>>
                ),
                geql@schema:resolver(_pipe@16, fun post_id_resolver/1)
            end
        ),
        _pipe@20 = geql@schema:field(
            _pipe@17,
            begin
                _pipe@18 = geql@schema:field_def(
                    <<"title"/utf8>>,
                    geql@schema:string_type()
                ),
                _pipe@19 = geql@schema:field_description(
                    _pipe@18,
                    <<"Post title"/utf8>>
                ),
                geql@schema:resolver(_pipe@19, fun post_title_resolver/1)
            end
        ),
        geql@schema:field(
            _pipe@20,
            begin
                _pipe@21 = geql@schema:field_def(
                    <<"author"/utf8>>,
                    geql@schema:named_type(<<"User"/utf8>>)
                ),
                _pipe@22 = geql@schema:field_description(
                    _pipe@21,
                    <<"Post author"/utf8>>
                ),
                geql@schema:resolver(_pipe@22, fun post_author_resolver/1)
            end
        )
    end,
    Query_type = begin
        _pipe@23 = geql@schema:object(<<"Query"/utf8>>),
        geql@schema:field(
            _pipe@23,
            begin
                _pipe@24 = geql@schema:field_def(
                    <<"users"/utf8>>,
                    geql@schema:list_type(
                        geql@schema:named_type(<<"User"/utf8>>)
                    )
                ),
                _pipe@25 = geql@schema:field_description(
                    _pipe@24,
                    <<"Get all users"/utf8>>
                ),
                geql@schema:resolver(_pipe@25, fun get_users_resolver/1)
            end
        )
    end,
    _pipe@26 = geql@schema:schema(),
    _pipe@27 = geql@schema:'query'(_pipe@26, Query_type),
    _pipe@28 = geql@schema:add_type(_pipe@27, {object_type_def, User_type}),
    geql@schema:add_type(_pipe@28, {object_type_def, Post_type}).

-file("src/dataloader_example.gleam", 56).
?DOC(" Batch load function for posts by author ID - simulates database query\n").
-spec batch_load_posts_by_author(list(gleam@dynamic:dynamic_())) -> {ok,
        list({ok, gleam@dynamic:dynamic_()} | {error, binary()})} |
    {error, binary()}.
batch_load_posts_by_author(Author_ids) ->
    gleam_stdlib:println(
        <<<<"🔄 BATCH LOADING POSTS: "/utf8,
                (erlang:integer_to_binary(erlang:length(Author_ids)))/binary>>/binary,
            " authors"/utf8>>
    ),
    String_ids = gleam@list:map(
        Author_ids,
        fun(_) -> <<"mock_author_id"/utf8>> end
    ),
    Results = gleam@list:map(
        String_ids,
        fun(Author_id) ->
            Author_posts = gleam@list:filter(
                [{post,
                        <<"1"/utf8>>,
                        <<"Getting Started with GraphQL"/utf8>>,
                        <<"1"/utf8>>,
                        <<"GraphQL is a query language..."/utf8>>},
                    {post,
                        <<"2"/utf8>>,
                        <<"Advanced Gleam Patterns"/utf8>>,
                        <<"1"/utf8>>,
                        <<"Gleam offers powerful features..."/utf8>>},
                    {post,
                        <<"3"/utf8>>,
                        <<"Database Design Tips"/utf8>>,
                        <<"2"/utf8>>,
                        <<"When designing schemas..."/utf8>>},
                    {post,
                        <<"4"/utf8>>,
                        <<"Web Development Best Practices"/utf8>>,
                        <<"3"/utf8>>,
                        <<"Modern web development..."/utf8>>},
                    {post,
                        <<"5"/utf8>>,
                        <<"Functional Programming Benefits"/utf8>>,
                        <<"2"/utf8>>,
                        <<"Functional programming offers..."/utf8>>}],
                fun(Post) -> erlang:element(4, Post) =:= Author_id end
            ),
            {ok, posts_to_dynamic(Author_posts)}
        end
    ),
    {ok, Results}.

-file("src/dataloader_example.gleam", 134).
?DOC(" Create execution context with DataLoaders\n").
-spec create_execution_context_with_dataloaders() -> geql@schema:execution_context().
create_execution_context_with_dataloaders() ->
    User_context = create_demo_dynamic(<<"demo_context"/utf8>>),
    Base_context = geql@schema:execution_context(User_context),
    User_loader = geql@dataloader:new(fun batch_load_users/1),
    Posts_loader = geql@dataloader:new(fun batch_load_posts_by_author/1),
    _pipe = Base_context,
    _pipe@1 = geql@schema:add_data_loader(_pipe, <<"users"/utf8>>, User_loader),
    geql@schema:add_data_loader(
        _pipe@1,
        <<"posts_by_author"/utf8>>,
        Posts_loader
    ).

-file("src/dataloader_example.gleam", 293).
?DOC(" Run the DataLoader example\n").
-spec run_dataloader_example() -> nil.
run_dataloader_example() ->
    gleam_stdlib:println(<<"=== DataLoader Example ==="/utf8>>),
    gleam_stdlib:println(
        <<"Demonstrates batching and caching to solve the N+1 query problem"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Schema = create_dataloader_schema(),
    Execution_context = create_execution_context_with_dataloaders(),
    Query = <<"
    {
      users {
        id
        name
        posts {
          title
        }
      }
    }
  "/utf8>>,
    gleam_stdlib:println(
        <<"🔍 Executing query that would normally cause N+1 problem:"/utf8>>
    ),
    gleam_stdlib:println(Query),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"With DataLoader, this becomes efficient batched operations:"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    case geql@executor:execute_query(Schema, Query) of
        Result ->
            case erlang:element(2, Result) of
                {some, _} ->
                    gleam_stdlib:println(
                        <<"✅ Query executed successfully with DataLoader batching!"/utf8>>
                    ),
                    gleam_stdlib:println(<<""/utf8>>),
                    gleam_stdlib:println(<<"💡 Key Benefits:"/utf8>>),
                    gleam_stdlib:println(
                        <<"- Multiple individual loads are batched into single operations"/utf8>>
                    ),
                    gleam_stdlib:println(
                        <<"- Results are cached to avoid duplicate requests"/utf8>>
                    ),
                    gleam_stdlib:println(
                        <<"- Dramatic reduction in database queries"/utf8>>
                    ),
                    gleam_stdlib:println(
                        <<"- Better performance and reduced load on data sources"/utf8>>
                    );

                none ->
                    gleam_stdlib:println(<<"❌ Query failed"/utf8>>),
                    case erlang:element(3, Result) of
                        [First_error | _] ->
                            case First_error of
                                {validation_error, Msg, _} ->
                                    gleam_stdlib:println(
                                        <<"Validation: "/utf8, Msg/binary>>
                                    );

                                {resolver_error, Msg@1, _} ->
                                    gleam_stdlib:println(
                                        <<"Resolver: "/utf8, Msg@1/binary>>
                                    );

                                {type_error, Msg@2, _} ->
                                    gleam_stdlib:println(
                                        <<"Type: "/utf8, Msg@2/binary>>
                                    )
                            end;

                        [] ->
                            gleam_stdlib:println(<<"Unknown error"/utf8>>)
                    end
            end
    end.
