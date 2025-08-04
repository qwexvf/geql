-record(variable_definition, {
    variable :: binary(),
    type_ :: geql@ast:type(),
    default_value :: gleam@option:option(geql@ast:value()),
    directives :: list(geql@ast:directive())
}).
