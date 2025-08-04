-record(input_object_type_def, {
    name :: binary(),
    description :: gleam@option:option(binary()),
    directives :: list(geql@sdl_ast:directive_usage()),
    fields :: list(geql@sdl_ast:input_field_def())
}).
