-record(input_object_type, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    fields :: gleam@dict:dict(binary(), geql@schema:input_field_definition())
}).
