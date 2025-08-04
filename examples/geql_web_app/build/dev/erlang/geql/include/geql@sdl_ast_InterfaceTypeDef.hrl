-record(interface_type_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(geql@sdl_ast:directive_usage()),
    fields :: list(geql@sdl_ast:field_def())
}).
