-record(field, {
    alias :: gleam@option:option(binary()),
    name :: binary(),
    arguments :: list(geql@ast:argument()),
    directives :: list(geql@ast:directive()),
    selection_set :: gleam@option:option(geql@ast:selection_set())
}).
