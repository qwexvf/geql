-record(argument_definition, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    arg_type :: geql@schema:field_type(),
    default_value :: gleam@option:option(gleam@dynamic:dynamic_())
}).
