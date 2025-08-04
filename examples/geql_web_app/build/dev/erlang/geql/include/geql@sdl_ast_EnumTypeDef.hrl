-record(enum_type_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(geql@sdl_ast:directive_usage()),
    values :: list(geql@sdl_ast:enum_value_def())
}).
