-record(execution_result, {
    data :: gleam@option:option(gleam@dynamic:dynamic_()),
    errors :: list(geql@executor:execution_error())
}).
