-record(input_field_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    field_type :: geql@sdl_ast:s_d_l_type(),
    default_value :: gleam@option:option(geql@sdl_ast:s_d_l_value()),
    directives :: list(geql@sdl_ast:directive_usage())
}).
