-record(field_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    arguments :: list(geql@sdl_ast:argument_def()),
    field_type :: geql@sdl_ast:s_d_l_type(),
    directives :: list(geql@sdl_ast:directive_usage())
}).
