-record(object_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    fields :: gleam@dict:dict(binary(), geql@schema:field_definition()),
    interfaces :: list(geql@schema:interface_type())
}).
