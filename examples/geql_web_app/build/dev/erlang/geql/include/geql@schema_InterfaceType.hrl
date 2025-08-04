-record(interface_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    fields :: gleam@dict:dict(binary(), geql@schema:field_definition())
}).
