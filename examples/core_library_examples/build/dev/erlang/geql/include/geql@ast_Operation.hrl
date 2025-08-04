-record(operation, {
    operation_type :: geql@ast:operation_type(),
    name :: gleam@option:option(binary()),
    variable_definitions :: list(geql@ast:variable_definition()),
    directives :: list(geql@ast:directive()),
    selection_set :: geql@ast:selection_set()
}).
