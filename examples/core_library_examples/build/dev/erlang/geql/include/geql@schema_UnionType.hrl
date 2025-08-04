-record(union_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    types :: list(geql@schema:object_type())
}).
