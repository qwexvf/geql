-record(resolver_info, {
    parent :: gleam@option:option(gleam@dynamic:dynamic_()),
    arguments :: gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
    context :: geql@schema:execution_context(),
    info :: gleam@dynamic:dynamic_()
}).
