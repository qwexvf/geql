-module(example_execution).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/example_execution.gleam").
-export([sample_user/0, create_user_schema/0, execute_user_query/0, run_example/0]).
-export_type([user/0]).

-type user() :: {user, binary(), binary(), binary()}.

-file("src/example_execution.gleam", 13).
-spec sample_user() -> user().
sample_user() ->
    {user, <<"user123"/utf8>>, <<"John Doe"/utf8>>, <<"john@example.com"/utf8>>}.

-file("src/example_execution.gleam", 19).
-spec get_user_from_parent(gleam@dynamic:dynamic_()) -> user().
get_user_from_parent(_) ->
    sample_user().

-file("src/example_execution.gleam", 29).
-spec create_user_schema() -> geql@schema:schema().
create_user_schema() ->
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
                    <<"The unique identifier for the user"/utf8>>
                ),
                geql@schema:resolver(
                    _pipe@3,
                    fun(Info) -> case erlang:element(2, Info) of
                            {some, Parent_dynamic} ->
                                User = get_user_from_parent(Parent_dynamic),
                                {error,
                                    <<"Demo: would return user.id = "/utf8,
                                        (erlang:element(2, User))/binary>>};

                            none ->
                                {error, <<"No parent user provided"/utf8>>}
                        end end
                )
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
                    <<"The user's display name"/utf8>>
                ),
                geql@schema:resolver(
                    _pipe@6,
                    fun(Info@1) -> case erlang:element(2, Info@1) of
                            {some, Parent_dynamic@1} ->
                                User@1 = get_user_from_parent(Parent_dynamic@1),
                                {error,
                                    <<"Demo: would return user.name = "/utf8,
                                        (erlang:element(3, User@1))/binary>>};

                            none ->
                                {error, <<"No parent user provided"/utf8>>}
                        end end
                )
            end
        ),
        geql@schema:field(
            _pipe@7,
            begin
                _pipe@8 = geql@schema:field_def(
                    <<"email"/utf8>>,
                    geql@schema:string_type()
                ),
                _pipe@9 = geql@schema:field_description(
                    _pipe@8,
                    <<"The user's email address"/utf8>>
                ),
                geql@schema:resolver(
                    _pipe@9,
                    fun(Info@2) -> case erlang:element(2, Info@2) of
                            {some, Parent_dynamic@2} ->
                                User@2 = get_user_from_parent(Parent_dynamic@2),
                                {error,
                                    <<"Demo: would return user.email = "/utf8,
                                        (erlang:element(4, User@2))/binary>>};

                            none ->
                                {error, <<"No parent user provided"/utf8>>}
                        end end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@10 = geql@schema:object(<<"Query"/utf8>>),
        _pipe@11 = geql@schema:description(
            _pipe@10,
            <<"The root query type"/utf8>>
        ),
        geql@schema:field(
            _pipe@11,
            begin
                _pipe@12 = geql@schema:field_def(
                    <<"user"/utf8>>,
                    geql@schema:named_type(<<"User"/utf8>>)
                ),
                _pipe@13 = geql@schema:field_description(
                    _pipe@12,
                    <<"Get a user"/utf8>>
                ),
                geql@schema:resolver(
                    _pipe@13,
                    fun(_) ->
                        User@3 = sample_user(),
                        {error,
                            <<<<<<<<<<<<"Demo: would return User(id="/utf8,
                                                    (erlang:element(2, User@3))/binary>>/binary,
                                                ", name="/utf8>>/binary,
                                            (erlang:element(3, User@3))/binary>>/binary,
                                        ", email="/utf8>>/binary,
                                    (erlang:element(4, User@3))/binary>>/binary,
                                ")"/utf8>>}
                    end
                )
            end
        )
    end,
    _pipe@14 = geql@schema:schema(),
    _pipe@15 = geql@schema:'query'(_pipe@14, Query_type),
    _pipe@16 = geql@schema:add_type(_pipe@15, {object_type_def, User_type}),
    _pipe@17 = geql@schema:add_type(
        _pipe@16,
        {scalar_type_def, geql@schema:string_scalar()}
    ),
    geql@schema:add_type(_pipe@17, {scalar_type_def, geql@schema:id_scalar()}).

-file("src/example_execution.gleam", 112).
-spec execute_user_query() -> geql@executor:execution_result().
execute_user_query() ->
    User_schema = create_user_schema(),
    Query = <<"{ user { id email name } }"/utf8>>,
    geql@executor:execute_query(User_schema, Query).

-file("src/example_execution.gleam", 119).
-spec run_example() -> nil.
run_example() ->
    gleam_stdlib:println(
        <<"Executing GraphQL query: { user { id email name } }"/utf8>>
    ),
    Result = execute_user_query(),
    case erlang:element(2, Result) of
        {some, _} ->
            gleam_stdlib:println(<<"✅ Query executed successfully!"/utf8>>);

        none ->
            gleam_stdlib:println(<<"❌ Query execution failed"/utf8>>),
            case erlang:element(3, Result) of
                [First_error | _] ->
                    case First_error of
                        {validation_error, Msg, Path} ->
                            gleam_stdlib:println(
                                <<"Validation error: "/utf8, Msg/binary>>
                            );

                        {resolver_error, Msg@1, Path@1} ->
                            gleam_stdlib:println(
                                <<"Resolver error: "/utf8, Msg@1/binary>>
                            );

                        {type_error, Msg@2, Path@2} ->
                            gleam_stdlib:println(
                                <<"Type error: "/utf8, Msg@2/binary>>
                            )
                    end;

                [] ->
                    gleam_stdlib:println(<<"Unknown error occurred"/utf8>>)
            end
    end.
