-record(field_spec, {
    name :: binary(),
    field_type :: geql@schema:field_type(),
    description :: binary(),
    extractor :: fun((gleam@dynamic:dynamic_()) -> {ok,
            gleam@dynamic:dynamic_()} |
        {error, binary()})
}).
