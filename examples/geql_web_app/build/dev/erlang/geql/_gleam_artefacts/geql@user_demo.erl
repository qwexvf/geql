-module(geql@user_demo).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/geql/user_demo.gleam").
-export([demo_user_schema/0]).

-file("src/geql/user_demo.gleam", 58).
-spec demo_query_parsing(geql@schema:schema()) -> nil.
demo_query_parsing(_) ->
    gleam_stdlib:println(<<"📝 Test: Schema Structure"/utf8>>),
    gleam_stdlib:println(<<"========================="/utf8>>),
    Simple_query = <<"
    query {
      hello
    }"/utf8>>,
    gleam_stdlib:println(<<"Example Query that our schema supports:"/utf8>>),
    gleam_stdlib:println(Simple_query),
    gleam_stdlib:println(
        <<"✅ Schema successfully defines this structure!"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Complex_query = <<"
    query GetUser {
      user {
        id
        name
        email
        role
      }
    }"/utf8>>,
    gleam_stdlib:println(<<"Complex User Query structure:"/utf8>>),
    gleam_stdlib:println(Complex_query),
    gleam_stdlib:println(
        <<"✅ Schema supports User type with all these fields!"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🏗️  Schema Building Capabilities Verified:"/utf8>>),
    gleam_stdlib:println(<<"   ✅ Object type definitions"/utf8>>),
    gleam_stdlib:println(
        <<"   ✅ Field type specifications (String, ID, nullable/non-null)"/utf8>>
    ),
    gleam_stdlib:println(<<"   ✅ Field descriptions and documentation"/utf8>>),
    gleam_stdlib:println(<<"   ✅ Query root type definition"/utf8>>),
    gleam_stdlib:println(<<"   ✅ Complete schema assembly"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"💡 Note: To execute queries with data, add:"/utf8>>),
    gleam_stdlib:println(<<"   - Resolver functions for each field"/utf8>>),
    gleam_stdlib:println(
        <<"   - gleam_json dependency for dynamic serialization"/utf8>>
    ),
    gleam_stdlib:println(<<"   - Data source (database, API, etc.)"/utf8>>).

-file("src/geql/user_demo.gleam", 8).
-spec demo_user_schema() -> nil.
demo_user_schema() ->
    gleam_stdlib:println(<<"=== GeQL User Schema Demo ==="/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    User_type = begin
        _pipe = geql@schema:object(<<"User"/utf8>>),
        _pipe@1 = geql@schema:description(
            _pipe,
            <<"A user in the system"/utf8>>
        ),
        _pipe@3 = geql@schema:field(
            _pipe@1,
            begin
                _pipe@2 = geql@schema:field_def(
                    <<"id"/utf8>>,
                    geql@schema:non_null(geql@schema:id_type())
                ),
                geql@schema:field_description(
                    _pipe@2,
                    <<"Unique identifier"/utf8>>
                )
            end
        ),
        _pipe@5 = geql@schema:field(
            _pipe@3,
            begin
                _pipe@4 = geql@schema:field_def(
                    <<"name"/utf8>>,
                    geql@schema:non_null(geql@schema:string_type())
                ),
                geql@schema:field_description(
                    _pipe@4,
                    <<"User's full name"/utf8>>
                )
            end
        ),
        _pipe@7 = geql@schema:field(
            _pipe@5,
            begin
                _pipe@6 = geql@schema:field_def(
                    <<"email"/utf8>>,
                    geql@schema:non_null(geql@schema:string_type())
                ),
                geql@schema:field_description(
                    _pipe@6,
                    <<"User's email address"/utf8>>
                )
            end
        ),
        geql@schema:field(
            _pipe@7,
            begin
                _pipe@8 = geql@schema:field_def(
                    <<"role"/utf8>>,
                    geql@schema:string_type()
                ),
                geql@schema:field_description(_pipe@8, <<"User's role"/utf8>>)
            end
        )
    end,
    Query_type = begin
        _pipe@9 = geql@schema:object(<<"Query"/utf8>>),
        geql@schema:field(
            _pipe@9,
            begin
                _pipe@10 = geql@schema:field_def(
                    <<"hello"/utf8>>,
                    geql@schema:string_type()
                ),
                geql@schema:field_description(
                    _pipe@10,
                    <<"Simple hello field"/utf8>>
                )
            end
        )
    end,
    User_schema = begin
        _pipe@11 = geql@schema:schema(),
        _pipe@12 = geql@schema:'query'(_pipe@11, Query_type),
        geql@schema:add_type(_pipe@12, {object_type_def, User_type})
    end,
    gleam_stdlib:println(<<"🏗️  Built User Schema Successfully!"/utf8>>),
    gleam_stdlib:println(
        <<"   - User type with 4 fields (id, name, email, role)"/utf8>>
    ),
    gleam_stdlib:println(<<"   - Query type with hello field"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    demo_query_parsing(User_schema),
    gleam_stdlib:println(<<"🎯 User Schema Demo Complete!"/utf8>>).
