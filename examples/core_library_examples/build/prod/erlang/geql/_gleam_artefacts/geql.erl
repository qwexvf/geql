-module(geql).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/geql.gleam").
-export([main/0, parse/1]).

-file("src/geql.gleam", 54).
-spec create_simple_schema() -> geql@schema:schema().
create_simple_schema() ->
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
                geql@schema:resolver(
                    _pipe@3,
                    fun(_) ->
                        {error, <<"Demo: Would return user ID here"/utf8>>}
                    end
                )
            end
        ),
        geql@schema:field(
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
                geql@schema:resolver(
                    _pipe@6,
                    fun(_) ->
                        {error, <<"Demo: Would return user name here"/utf8>>}
                    end
                )
            end
        )
    end,
    Query_type = begin
        _pipe@7 = geql@schema:object(<<"Query"/utf8>>),
        geql@schema:field(
            _pipe@7,
            begin
                _pipe@8 = geql@schema:field_def(
                    <<"user"/utf8>>,
                    geql@schema:named_type(<<"User"/utf8>>)
                ),
                _pipe@9 = geql@schema:field_description(
                    _pipe@8,
                    <<"Get a user"/utf8>>
                ),
                geql@schema:resolver(
                    _pipe@9,
                    fun(_) ->
                        {error, <<"Demo: Would return user data here"/utf8>>}
                    end
                )
            end
        )
    end,
    _pipe@10 = geql@schema:schema(),
    _pipe@11 = geql@schema:'query'(_pipe@10, Query_type),
    geql@schema:add_type(_pipe@11, {object_type_def, User_type}).

-file("src/geql.gleam", 8).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"=== GeQL Pure GraphQL Library ==="/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"GeQL is a pure GraphQL library for Gleam with zero dependencies beyond gleam_stdlib"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Query = <<"{ user { entries { id } } }"/utf8>>,
    case geql@parser:parse(Query) of
        {ok, _} ->
            gleam_stdlib:println(
                <<"✅ Parser: Successfully parsed GraphQL query!"/utf8>>
            );

        {error, _} ->
            gleam_stdlib:println(<<"❌ Parser: Parse error occurred"/utf8>>)
    end,
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🏗️ Schema Definition:"/utf8>>),
    Simple_schema = create_simple_schema(),
    gleam_stdlib:println(<<"✅ Created GraphQL schema with User type"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"⚡ Query Execution:"/utf8>>),
    Result = geql@executor:execute_query(
        Simple_schema,
        <<"{ user { id name } }"/utf8>>
    ),
    case erlang:element(2, Result) of
        {some, _} ->
            gleam_stdlib:println(<<"✅ Query executed successfully!"/utf8>>);

        none ->
            gleam_stdlib:println(
                <<"ℹ️  Query execution shows where Dynamic serialization would be needed"/utf8>>
            ),
            gleam_stdlib:println(
                <<"   (Use gleam_json or external functions in real implementations)"/utf8>>
            )
    end,
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"📁 Complete Examples Available:"/utf8>>),
    gleam_stdlib:println(
        <<"- ./examples/core_library_examples/ - Pure GraphQL examples"/utf8>>
    ),
    gleam_stdlib:println(
        <<"- ./examples/geql_web_app/ - Complete web server with database"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🧚‍♀️ GeQL: Pure GraphQL for Gleam!"/utf8>>).

-file("src/geql.gleam", 88).
-spec parse(binary()) -> {ok, geql@ast:document()} |
    {error, geql@parser:parse_error()}.
parse(Query) ->
    geql@parser:parse(Query).
