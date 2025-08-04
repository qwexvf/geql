-module(core_library_examples).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/core_library_examples.gleam").
-export([main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/core_library_examples.gleam", 9).
?DOC(" Demonstrates core GeQL GraphQL library functionality\n").
-spec main() -> nil.
main() ->
    gleam_stdlib:println(<<"=== GeQL Core Library Examples ==="/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"This demonstrates the pure GraphQL functionality without any web server or database dependencies."/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    Query = <<"{ user { entries { id } } }"/utf8>>,
    case geql:parse(Query) of
        {ok, _} ->
            gleam_stdlib:println(
                <<"✅ Parser: Successfully parsed GraphQL query!"/utf8>>
            );

        {error, _} ->
            gleam_stdlib:println(<<"❌ Parser: Parse error occurred"/utf8>>)
    end,
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🎯 Core GraphQL Library Features:"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"📋 Auto-Generated Schema Example:"/utf8>>),
    person_example:run_person_example(),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🔧 Manual Schema Definition Example:"/utf8>>),
    example_execution:run_example(),
    gleam_stdlib:println(<<""/utf8>>),
    dataloader_example:run_dataloader_example(),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"🧚‍♀️ Pure GraphQL library demo complete!"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"For web server integration, see: ../geql_web_app/"/utf8>>
    ).
