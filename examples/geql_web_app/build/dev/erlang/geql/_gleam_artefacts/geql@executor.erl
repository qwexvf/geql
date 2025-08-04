-module(geql@executor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/geql/executor.gleam").
-export([new_debug_context/1, execute/5, execute_query/2, execute_query_debug/2]).
-export_type([execution_result/0, execution_error/0, query_execution_context/0, field_context/0, debug_context/0]).

-type execution_result() :: {execution_result,
        gleam@option:option(gleam@dynamic:dynamic_()),
        list(execution_error())}.

-type execution_error() :: {validation_error, binary(), list(binary())} |
    {resolver_error, binary(), list(binary())} |
    {type_error, binary(), list(binary())}.

-type query_execution_context() :: {query_execution_context,
        geql@schema:schema(),
        gleam@option:option(gleam@dynamic:dynamic_()),
        geql@schema:execution_context(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_())}.

-type field_context() :: {field_context,
        gleam@option:option(gleam@dynamic:dynamic_()),
        binary(),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
        list(binary())}.

-type debug_context() :: {debug_context, boolean(), integer(), integer()}.

-file("src/geql/executor.gleam", 44).
-spec new_debug_context(boolean()) -> debug_context().
new_debug_context(Enabled) ->
    {debug_context, Enabled, 0, 1}.

-file("src/geql/executor.gleam", 48).
-spec debug_log(debug_context(), binary()) -> debug_context().
debug_log(Debug, Message) ->
    case erlang:element(2, Debug) of
        true ->
            Indent = gleam@string:repeat(
                <<"  "/utf8>>,
                erlang:element(3, Debug)
            ),
            gleam_stdlib:println(
                <<<<Indent/binary, "🔍 "/utf8>>/binary, Message/binary>>
            ),
            Debug;

        false ->
            Debug
    end.

-file("src/geql/executor.gleam", 71).
-spec debug_result(debug_context(), binary()) -> debug_context().
debug_result(Debug, Message) ->
    case erlang:element(2, Debug) of
        true ->
            Indent = gleam@string:repeat(
                <<"  "/utf8>>,
                erlang:element(3, Debug)
            ),
            gleam_stdlib:println(
                <<<<Indent/binary, "✅ "/utf8>>/binary, Message/binary>>
            ),
            Debug;

        false ->
            Debug
    end.

-file("src/geql/executor.gleam", 82).
-spec debug_error(debug_context(), binary()) -> debug_context().
debug_error(Debug, Message) ->
    case erlang:element(2, Debug) of
        true ->
            Indent = gleam@string:repeat(
                <<"  "/utf8>>,
                erlang:element(3, Debug)
            ),
            gleam_stdlib:println(
                <<<<Indent/binary, "❌ "/utf8>>/binary, Message/binary>>
            ),
            Debug;

        false ->
            Debug
    end.

-file("src/geql/executor.gleam", 93).
-spec int_to_string(integer()) -> binary().
int_to_string(I) ->
    case I of
        0 ->
            <<"0"/utf8>>;

        1 ->
            <<"1"/utf8>>;

        2 ->
            <<"2"/utf8>>;

        3 ->
            <<"3"/utf8>>;

        4 ->
            <<"4"/utf8>>;

        5 ->
            <<"5"/utf8>>;

        6 ->
            <<"6"/utf8>>;

        7 ->
            <<"7"/utf8>>;

        8 ->
            <<"8"/utf8>>;

        9 ->
            <<"9"/utf8>>;

        _ ->
            <<"N"/utf8>>
    end.

-file("src/geql/executor.gleam", 59).
-spec debug_step(debug_context(), binary()) -> debug_context().
debug_step(Debug, Message) ->
    case erlang:element(2, Debug) of
        true ->
            Indent = gleam@string:repeat(
                <<"  "/utf8>>,
                erlang:element(3, Debug)
            ),
            Step_msg = <<<<(int_to_string(erlang:element(4, Debug)))/binary,
                    ". "/utf8>>/binary,
                Message/binary>>,
            gleam_stdlib:println(
                <<<<Indent/binary, "📋 "/utf8>>/binary, Step_msg/binary>>
            ),
            _record = Debug,
            {debug_context,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, Debug) + 1};

        false ->
            Debug
    end.

-file("src/geql/executor.gleam", 355).
-spec get_field_type_definition(geql@schema:schema(), geql@schema:field_type()) -> {ok,
        geql@schema:type_definition()} |
    {error, binary()}.
get_field_type_definition(Schema_def, Field_type) ->
    case Field_type of
        {named, Type_name} ->
            _pipe = gleam_stdlib:map_get(
                erlang:element(5, Schema_def),
                Type_name
            ),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    <<<<"Type '"/utf8, Type_name/binary>>/binary,
                        "' not found in schema"/utf8>>
                end
            );

        {non_null, Inner_type} ->
            get_field_type_definition(Schema_def, Inner_type);

        {list, Inner_type@1} ->
            get_field_type_definition(Schema_def, Inner_type@1)
    end.

-file("src/geql/executor.gleam", 397).
-spec create_string_dynamic(binary()) -> gleam@dynamic:dynamic_().
create_string_dynamic(_) ->
    erlang:error(#{gleam_error => panic,
            message => <<"Dynamic serialization needed: use gleam_json or similar library"/utf8>>,
            file => <<?FILEPATH/utf8>>,
            module => <<"geql/executor"/utf8>>,
            function => <<"create_string_dynamic"/utf8>>,
            line => 411}).

-file("src/geql/executor.gleam", 371).
-spec create_field_data(binary(), gleam@dynamic:dynamic_()) -> gleam@dynamic:dynamic_().
create_field_data(_, _) ->
    create_string_dynamic(<<"resolved_value"/utf8>>).

-file("src/geql/executor.gleam", 376).
-spec get_field_from_parent(gleam@dynamic:dynamic_(), binary()) -> gleam@dynamic:dynamic_().
get_field_from_parent(_, _) ->
    create_string_dynamic(<<"parent_field_value"/utf8>>).

-file("src/geql/executor.gleam", 390).
-spec create_empty_object() -> gleam@dynamic:dynamic_().
create_empty_object() ->
    create_string_dynamic(<<"{}"/utf8>>).

-file("src/geql/executor.gleam", 381).
-spec merge_selection_results(list(gleam@dynamic:dynamic_())) -> gleam@dynamic:dynamic_().
merge_selection_results(Results) ->
    case Results of
        [First | _] ->
            First;

        [] ->
            create_empty_object()
    end.

-file("src/geql/executor.gleam", 414).
-spec create_resolver_info() -> gleam@dynamic:dynamic_().
create_resolver_info() ->
    create_string_dynamic(<<"resolver_info"/utf8>>).

-file("src/geql/executor.gleam", 472).
-spec parse_error_to_string(geql@parser:parse_error()) -> binary().
parse_error_to_string(Error) ->
    case Error of
        {lex_error, _} ->
            <<"Lexer error"/utf8>>;

        {unexpected_token, Expected, _, _} ->
            <<"Expected "/utf8, Expected/binary>>;

        {unexpected_e_o_f, Expected@1} ->
            <<"Unexpected EOF, expected "/utf8, Expected@1/binary>>
    end.

-file("src/geql/executor.gleam", 235).
-spec execute_field(
    query_execution_context(),
    geql@ast:field(),
    geql@schema:object_type(),
    field_context()
) -> execution_result().
execute_field(Context, Field, Object_type, Field_context) ->
    Field_name = erlang:element(3, Field),
    Field_path = lists:append(erlang:element(5, Field_context), [Field_name]),
    case gleam_stdlib:map_get(erlang:element(4, Object_type), Field_name) of
        {ok, Field_def} ->
            Field_args = maps:new(),
            _ = {field_context,
                erlang:element(2, Field_context),
                Field_name,
                Field_args,
                Field_path},
            case erlang:element(6, Field_def) of
                {some, Resolver} ->
                    Resolver_info = {resolver_info,
                        erlang:element(2, Field_context),
                        Field_args,
                        erlang:element(4, Context),
                        create_resolver_info()},
                    case Resolver(Resolver_info) of
                        {ok, Resolved_value} ->
                            case erlang:element(6, Field) of
                                {some, Sub_selection_set} ->
                                    case get_field_type_definition(
                                        erlang:element(2, Context),
                                        erlang:element(4, Field_def)
                                    ) of
                                        {ok, {object_type_def, Sub_object_type}} ->
                                            Sub_field_context = {field_context,
                                                {some, Resolved_value},
                                                Field_name,
                                                Field_args,
                                                Field_path},
                                            execute_selection_set(
                                                Context,
                                                Sub_selection_set,
                                                Sub_object_type,
                                                Sub_field_context
                                            );

                                        {ok, _} ->
                                            {execution_result,
                                                none,
                                                [{type_error,
                                                        <<"Cannot execute selection set on non-object type"/utf8>>,
                                                        Field_path}]};

                                        {error, Msg} ->
                                            {execution_result,
                                                none,
                                                [{type_error, Msg, Field_path}]}
                                    end;

                                none ->
                                    {execution_result,
                                        {some,
                                            create_field_data(
                                                Field_name,
                                                Resolved_value
                                            )},
                                        []}
                            end;

                        {error, Error_msg} ->
                            {execution_result,
                                none,
                                [{resolver_error, Error_msg, Field_path}]}
                    end;

                none ->
                    case erlang:element(2, Field_context) of
                        {some, Parent} ->
                            Field_value = get_field_from_parent(
                                Parent,
                                Field_name
                            ),
                            {execution_result,
                                {some,
                                    create_field_data(Field_name, Field_value)},
                                []};

                        none ->
                            {execution_result,
                                none,
                                [{resolver_error,
                                        <<"No resolver and no parent value"/utf8>>,
                                        Field_path}]}
                    end
            end;

        {error, _} ->
            {execution_result,
                none,
                [{validation_error,
                        <<<<<<<<"Field '"/utf8, Field_name/binary>>/binary,
                                    "' not found on type '"/utf8>>/binary,
                                (erlang:element(2, Object_type))/binary>>/binary,
                            "'"/utf8>>,
                        Field_path}]}
    end.

-file("src/geql/executor.gleam", 177).
-spec execute_selection_set(
    query_execution_context(),
    geql@ast:selection_set(),
    geql@schema:object_type(),
    field_context()
) -> execution_result().
execute_selection_set(Context, Selection_set, Object_type, Field_context) ->
    Results = gleam@list:map(
        erlang:element(2, Selection_set),
        fun(Selection) ->
            execute_selection(Context, Selection, Object_type, Field_context)
        end
    ),
    Errors = gleam@list:fold(
        Results,
        [],
        fun(Acc, Result) -> lists:append(Acc, erlang:element(3, Result)) end
    ),
    Data_results = gleam@list:fold(
        Results,
        [],
        fun(Acc@1, Result@1) -> case erlang:element(2, Result@1) of
                {some, Data} ->
                    [Data | Acc@1];

                none ->
                    Acc@1
            end end
    ),
    case {Data_results, Errors} of
        {[], []} ->
            {execution_result, {some, create_empty_object()}, []};

        {[], _} ->
            {execution_result, none, Errors};

        {_, _} ->
            Merged_data = merge_selection_results(Data_results),
            {execution_result, {some, Merged_data}, Errors}
    end.

-file("src/geql/executor.gleam", 209).
-spec execute_selection(
    query_execution_context(),
    geql@ast:selection(),
    geql@schema:object_type(),
    field_context()
) -> execution_result().
execute_selection(Context, Selection, Object_type, Field_context) ->
    case Selection of
        {field_selection, Field} ->
            execute_field(Context, Field, Object_type, Field_context);

        {fragment_spread, _} ->
            {execution_result,
                none,
                [{validation_error,
                        <<"Fragment spreads not yet supported"/utf8>>,
                        erlang:element(5, Field_context)}]};

        {inline_fragment, _} ->
            {execution_result,
                none,
                [{validation_error,
                        <<"Inline fragments not yet supported"/utf8>>,
                        erlang:element(5, Field_context)}]}
    end.

-file("src/geql/executor.gleam", 138).
-spec execute_operation(query_execution_context(), geql@ast:operation()) -> execution_result().
execute_operation(Context, Operation) ->
    Root_type = case Operation of
        {operation, 'query', _, _, _, _} ->
            erlang:element(2, erlang:element(2, Context));

        {operation, mutation, _, _, _, _} ->
            erlang:element(3, erlang:element(2, Context));

        {operation, subscription, _, _, _, _} ->
            erlang:element(4, erlang:element(2, Context));

        {shorthand_query, _} ->
            erlang:element(2, erlang:element(2, Context))
    end,
    case Root_type of
        {some, Object_type} ->
            Selection_set = case Operation of
                {operation, _, _, _, _, Ss} ->
                    Ss;

                {shorthand_query, Ss@1} ->
                    Ss@1
            end,
            Field_context = {field_context,
                erlang:element(3, Context),
                <<"root"/utf8>>,
                maps:new(),
                []},
            execute_selection_set(
                Context,
                Selection_set,
                Object_type,
                Field_context
            );

        none ->
            {execution_result,
                none,
                [{validation_error,
                        <<"Schema does not define a root type for this operation"/utf8>>,
                        []}]}
    end.

-file("src/geql/executor.gleam", 109).
-spec execute(
    geql@schema:schema(),
    geql@ast:document(),
    gleam@option:option(gleam@dynamic:dynamic_()),
    geql@schema:execution_context(),
    gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
) -> execution_result().
execute(Schema_def, Document, Root_value, Execution_context, Variable_values) ->
    Context = {query_execution_context,
        Schema_def,
        Root_value,
        Execution_context,
        Variable_values},
    case erlang:element(2, Document) of
        [{operation_definition, Operation} | _] ->
            execute_operation(Context, Operation);

        [] ->
            {execution_result,
                none,
                [{validation_error,
                        <<"Document must contain at least one operation"/utf8>>,
                        []}]};

        _ ->
            {execution_result,
                none,
                [{validation_error,
                        <<"Document contains non-operation definitions"/utf8>>,
                        []}]}
    end.

-file("src/geql/executor.gleam", 481).
-spec execute_query(geql@schema:schema(), binary()) -> execution_result().
execute_query(Schema_def, Query) ->
    case geql@parser:parse(Query) of
        {ok, Document} ->
            Execution_context = geql@schema:execution_context(
                create_string_dynamic(<<"context"/utf8>>)
            ),
            execute(Schema_def, Document, none, Execution_context, maps:new());

        {error, _} ->
            {execution_result,
                none,
                [{validation_error, <<"Failed to parse query"/utf8>>, []}]}
    end.

-file("src/geql/executor.gleam", 419).
-spec execute_query_debug(geql@schema:schema(), binary()) -> execution_result().
execute_query_debug(Schema, Query) ->
    Debug = new_debug_context(true),
    Debug@1 = debug_step(Debug, <<"🚀 Starting GraphQL Query Execution"/utf8>>),
    Debug@2 = debug_log(Debug@1, <<"Query: "/utf8, Query/binary>>),
    Debug@3 = debug_step(Debug@2, <<"📝 Parsing Query"/utf8>>),
    case geql@parser:parse(Query) of
        {ok, Document} ->
            Debug@4 = debug_result(Debug@3, <<"Parse successful"/utf8>>),
            Debug@5 = debug_log(
                Debug@4,
                <<"Document definitions: "/utf8,
                    (int_to_string(erlang:length(erlang:element(2, Document))))/binary>>
            ),
            Debug@6 = debug_step(Debug@5, <<"🎯 Executing Query"/utf8>>),
            Debug@7 = debug_log(
                Debug@6,
                <<"Calling standard executor..."/utf8>>
            ),
            Result = execute_query(Schema, Query),
            case erlang:element(2, Result) of
                {some, _} ->
                    _ = debug_result(
                        Debug@7,
                        <<"✅ Query executed successfully!"/utf8>>
                    ),
                    nil;

                none ->
                    _ = debug_error(
                        Debug@7,
                        <<"❌ Query execution failed"/utf8>>
                    ),
                    _ = debug_log(
                        Debug@7,
                        <<"Errors: "/utf8,
                            (int_to_string(
                                erlang:length(erlang:element(3, Result))
                            ))/binary>>
                    ),
                    nil
            end,
            Result;

        {error, Error} ->
            _ = debug_error(
                Debug@3,
                <<"Parse failed: "/utf8, (parse_error_to_string(Error))/binary>>
            ),
            {execution_result,
                none,
                [{validation_error,
                        <<"Parse error: "/utf8,
                            (parse_error_to_string(Error))/binary>>,
                        []}]}
    end.
