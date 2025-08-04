-record(query_execution_context, {
    schema :: geql@schema:schema(),
    root_value :: gleam@option:option(gleam@dynamic:dynamic_()),
    execution_context :: geql@schema:execution_context(),
    variable_values :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_())
}).
