-record(field_definition, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    field_type :: geql@schema:field_type(),
    arguments :: gleam@dict:dict(binary(), geql@schema:argument_definition()),
    resolver :: gleam@option:option(fun((geql@schema:resolver_info()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()}))
}).
