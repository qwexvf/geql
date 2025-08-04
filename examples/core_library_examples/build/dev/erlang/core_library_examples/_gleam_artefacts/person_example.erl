-module(person_example).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/person_example.gleam").
-export([sample_person/0, create_person_schema/0, execute_person_query/0, run_person_example/0]).
-export_type([person/0]).

-type person() :: {person, binary(), integer(), boolean()}.

-file("src/person_example.gleam", 14).
-spec sample_person() -> person().
sample_person() ->
    {person, <<"Alice"/utf8>>, 30, true}.

-file("src/person_example.gleam", 19).
-spec extract_name(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, binary()}.
extract_name(Parent) ->
    {ok, <<"Alice"/utf8>>}.

-file("src/person_example.gleam", 25).
-spec extract_age(gleam@dynamic:dynamic_()) -> {ok, integer()} |
    {error, binary()}.
extract_age(Parent) ->
    {ok, 30}.

-file("src/person_example.gleam", 31).
-spec extract_needs_glasses(gleam@dynamic:dynamic_()) -> {ok, boolean()} |
    {error, binary()}.
extract_needs_glasses(Parent) ->
    {ok, true}.

-file("src/person_example.gleam", 109).
-spec int_to_string(integer()) -> binary().
int_to_string(Value) ->
    case Value of
        30 ->
            <<"30"/utf8>>;

        _ ->
            <<"unknown"/utf8>>
    end.

-file("src/person_example.gleam", 116).
-spec bool_to_string(boolean()) -> binary().
bool_to_string(Value) ->
    case Value of
        true ->
            <<"true"/utf8>>;

        false ->
            <<"false"/utf8>>
    end.

-file("src/person_example.gleam", 38).
-spec create_person_schema() -> geql@schema:schema().
create_person_schema() ->
    Field_specs = [geql@schema_gen:string_field(
            <<"name"/utf8>>,
            <<"The person's name"/utf8>>,
            fun extract_name/1
        ),
        geql@schema_gen:int_field(
            <<"age"/utf8>>,
            <<"The person's age in years"/utf8>>,
            fun extract_age/1
        ),
        geql@schema_gen:bool_field(
            <<"needsGlasses"/utf8>>,
            <<"Whether the person needs glasses"/utf8>>,
            fun extract_needs_glasses/1
        )],
    geql@schema_gen:create_schema_with_query(
        <<"Person"/utf8>>,
        Field_specs,
        fun(_) ->
            Person = sample_person(),
            {error,
                <<<<<<<<<<<<"Demo: would return Person(name="/utf8,
                                        (erlang:element(2, Person))/binary>>/binary,
                                    ", age="/utf8>>/binary,
                                (int_to_string(erlang:element(3, Person)))/binary>>/binary,
                            ", needs_glasses="/utf8>>/binary,
                        (bool_to_string(erlang:element(4, Person)))/binary>>/binary,
                    ")"/utf8>>}
        end
    ).

-file("src/person_example.gleam", 66).
-spec execute_person_query() -> geql@executor:execution_result().
execute_person_query() ->
    Person_schema = create_person_schema(),
    Query = <<"{ person { name age needsGlasses } }"/utf8>>,
    geql@executor:execute_query(Person_schema, Query).

-file("src/person_example.gleam", 73).
-spec run_person_example() -> nil.
run_person_example() ->
    gleam_stdlib:println(<<"=== Auto-Generated Schema Example ==="/utf8>>),
    gleam_stdlib:println(<<"Generated GraphQL schema from Gleam type:"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"pub type Person {"/utf8>>),
    gleam_stdlib:println(
        <<"  Person(name: String, age: Int, needs_glasses: Bool)"/utf8>>
    ),
    gleam_stdlib:println(<<"}"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"Executing query: { person { name age needsGlasses } }"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Result = execute_person_query(),
    case erlang:element(2, Result) of
        {some, _} ->
            gleam_stdlib:println(
                <<"✅ Auto-generated schema executed successfully!"/utf8>>
            );

        none ->
            gleam_stdlib:println(
                <<"❌ Query execution failed with auto-generated schema"/utf8>>
            ),
            case erlang:element(3, Result) of
                [First_error | _] ->
                    case First_error of
                        {validation_error, Msg, _} ->
                            gleam_stdlib:println(
                                <<"Validation error: "/utf8, Msg/binary>>
                            );

                        {resolver_error, Msg@1, _} ->
                            gleam_stdlib:println(
                                <<"Resolver error: "/utf8, Msg@1/binary>>
                            );

                        {type_error, Msg@2, _} ->
                            gleam_stdlib:println(
                                <<"Type error: "/utf8, Msg@2/binary>>
                            )
                    end;

                [] ->
                    gleam_stdlib:println(<<"Unknown error occurred"/utf8>>)
            end
    end.
