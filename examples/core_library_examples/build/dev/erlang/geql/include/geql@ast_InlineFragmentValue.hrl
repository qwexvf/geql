-record(inline_fragment_value, {
    type_condition :: gleam@option:option(binary()),
    directives :: list(geql@ast:directive()),
    selection_set :: geql@ast:selection_set()
}).
