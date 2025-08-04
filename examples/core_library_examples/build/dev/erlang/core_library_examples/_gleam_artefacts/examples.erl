-module(examples).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/examples.gleam").
-export([user_schema/0, blog_schema/0]).

-file("src/examples.gleam", 3).
-spec user_schema() -> geql@schema:schema().
user_schema() ->
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
                    <<"The unique identifier for the user"/utf8>>
                )
            end
        ),
        _pipe@5 = geql@schema:field(
            _pipe@3,
            begin
                _pipe@4 = geql@schema:field_def(
                    <<"name"/utf8>>,
                    geql@schema:string_type()
                ),
                geql@schema:field_description(
                    _pipe@4,
                    <<"The user's display name"/utf8>>
                )
            end
        ),
        _pipe@7 = geql@schema:field(
            _pipe@5,
            begin
                _pipe@6 = geql@schema:field_def(
                    <<"email"/utf8>>,
                    geql@schema:string_type()
                ),
                geql@schema:field_description(
                    _pipe@6,
                    <<"The user's email address"/utf8>>
                )
            end
        ),
        geql@schema:field(
            _pipe@7,
            begin
                _pipe@8 = geql@schema:field_def(
                    <<"entries"/utf8>>,
                    geql@schema:list_type(
                        geql@schema:named_type(<<"Entry"/utf8>>)
                    )
                ),
                geql@schema:field_description(
                    _pipe@8,
                    <<"User's blog entries"/utf8>>
                )
            end
        )
    end,
    Entry_type = begin
        _pipe@9 = geql@schema:object(<<"Entry"/utf8>>),
        _pipe@10 = geql@schema:description(_pipe@9, <<"A blog entry"/utf8>>),
        _pipe@12 = geql@schema:field(
            _pipe@10,
            begin
                _pipe@11 = geql@schema:field_def(
                    <<"id"/utf8>>,
                    geql@schema:non_null(geql@schema:id_type())
                ),
                geql@schema:field_description(
                    _pipe@11,
                    <<"The unique identifier for the entry"/utf8>>
                )
            end
        ),
        _pipe@14 = geql@schema:field(
            _pipe@12,
            begin
                _pipe@13 = geql@schema:field_def(
                    <<"title"/utf8>>,
                    geql@schema:string_type()
                ),
                geql@schema:field_description(
                    _pipe@13,
                    <<"The entry title"/utf8>>
                )
            end
        ),
        _pipe@16 = geql@schema:field(
            _pipe@14,
            begin
                _pipe@15 = geql@schema:field_def(
                    <<"content"/utf8>>,
                    geql@schema:string_type()
                ),
                geql@schema:field_description(
                    _pipe@15,
                    <<"The entry content"/utf8>>
                )
            end
        ),
        geql@schema:field(
            _pipe@16,
            begin
                _pipe@17 = geql@schema:field_def(
                    <<"author"/utf8>>,
                    geql@schema:named_type(<<"User"/utf8>>)
                ),
                geql@schema:field_description(
                    _pipe@17,
                    <<"The entry author"/utf8>>
                )
            end
        )
    end,
    Query_type = begin
        _pipe@18 = geql@schema:object(<<"Query"/utf8>>),
        _pipe@19 = geql@schema:description(
            _pipe@18,
            <<"The root query type"/utf8>>
        ),
        _pipe@23 = geql@schema:field(
            _pipe@19,
            begin
                _pipe@20 = geql@schema:field_def(
                    <<"user"/utf8>>,
                    geql@schema:named_type(<<"User"/utf8>>)
                ),
                _pipe@21 = geql@schema:field_description(
                    _pipe@20,
                    <<"Get a user"/utf8>>
                ),
                geql@schema:argument(
                    _pipe@21,
                    begin
                        _pipe@22 = geql@schema:arg(
                            <<"id"/utf8>>,
                            geql@schema:non_null(geql@schema:id_type())
                        ),
                        geql@schema:arg_description(
                            _pipe@22,
                            <<"The ID of the user to fetch"/utf8>>
                        )
                    end
                )
            end
        ),
        geql@schema:field(
            _pipe@23,
            begin
                _pipe@24 = geql@schema:field_def(
                    <<"users"/utf8>>,
                    geql@schema:list_type(
                        geql@schema:named_type(<<"User"/utf8>>)
                    )
                ),
                geql@schema:field_description(
                    _pipe@24,
                    <<"Get all users"/utf8>>
                )
            end
        )
    end,
    _pipe@25 = geql@schema:schema(),
    _pipe@26 = geql@schema:'query'(_pipe@25, Query_type),
    _pipe@27 = geql@schema:add_type(_pipe@26, {object_type_def, User_type}),
    _pipe@28 = geql@schema:add_type(_pipe@27, {object_type_def, Entry_type}),
    _pipe@29 = geql@schema:add_type(
        _pipe@28,
        {scalar_type_def, geql@schema:string_scalar()}
    ),
    _pipe@30 = geql@schema:add_type(
        _pipe@29,
        {scalar_type_def, geql@schema:int_scalar()}
    ),
    geql@schema:add_type(_pipe@30, {scalar_type_def, geql@schema:id_scalar()}).

-file("src/examples.gleam", 70).
-spec blog_schema() -> geql@schema:schema().
blog_schema() ->
    Post_type = begin
        _pipe = geql@schema:object(<<"Post"/utf8>>),
        _pipe@1 = geql@schema:description(_pipe, <<"A blog post"/utf8>>),
        _pipe@2 = geql@schema:field(
            _pipe@1,
            geql@schema:field_def(
                <<"id"/utf8>>,
                geql@schema:non_null(geql@schema:id_type())
            )
        ),
        _pipe@3 = geql@schema:field(
            _pipe@2,
            geql@schema:field_def(
                <<"title"/utf8>>,
                geql@schema:non_null(geql@schema:string_type())
            )
        ),
        _pipe@4 = geql@schema:field(
            _pipe@3,
            geql@schema:field_def(<<"content"/utf8>>, geql@schema:string_type())
        ),
        _pipe@5 = geql@schema:field(
            _pipe@4,
            geql@schema:field_def(
                <<"published"/utf8>>,
                geql@schema:boolean_type()
            )
        ),
        geql@schema:field(
            _pipe@5,
            geql@schema:field_def(
                <<"tags"/utf8>>,
                geql@schema:list_type(geql@schema:string_type())
            )
        )
    end,
    Query_type = begin
        _pipe@6 = geql@schema:object(<<"Query"/utf8>>),
        _pipe@8 = geql@schema:field(
            _pipe@6,
            begin
                _pipe@7 = geql@schema:field_def(
                    <<"post"/utf8>>,
                    geql@schema:named_type(<<"Post"/utf8>>)
                ),
                geql@schema:argument(
                    _pipe@7,
                    geql@schema:arg(
                        <<"id"/utf8>>,
                        geql@schema:non_null(geql@schema:id_type())
                    )
                )
            end
        ),
        geql@schema:field(
            _pipe@8,
            begin
                _pipe@9 = geql@schema:field_def(
                    <<"posts"/utf8>>,
                    geql@schema:non_null(
                        geql@schema:list_type(
                            geql@schema:named_type(<<"Post"/utf8>>)
                        )
                    )
                ),
                _pipe@10 = geql@schema:argument(
                    _pipe@9,
                    geql@schema:arg(<<"first"/utf8>>, geql@schema:int_type())
                ),
                geql@schema:argument(
                    _pipe@10,
                    begin
                        _pipe@11 = geql@schema:arg(
                            <<"after"/utf8>>,
                            geql@schema:string_type()
                        ),
                        geql@schema:arg_description(
                            _pipe@11,
                            <<"Cursor for pagination"/utf8>>
                        )
                    end
                )
            end
        )
    end,
    _pipe@12 = geql@schema:schema(),
    _pipe@13 = geql@schema:'query'(_pipe@12, Query_type),
    geql@schema:add_type(_pipe@13, {object_type_def, Post_type}).
