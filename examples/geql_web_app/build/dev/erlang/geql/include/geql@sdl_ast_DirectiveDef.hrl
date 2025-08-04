-record(directive_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    locations :: list(geql@sdl_ast:directive_location()),
    arguments :: list(geql@sdl_ast:argument_def())
}).
