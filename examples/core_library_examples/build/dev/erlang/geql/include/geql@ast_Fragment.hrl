-record(fragment, {
    name :: binary(),
    type_condition :: binary(),
    directives :: list(geql@ast:directive()),
    selection_set :: geql@ast:selection_set()
}).
