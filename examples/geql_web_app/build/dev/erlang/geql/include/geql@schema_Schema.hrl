-record(schema, {
    'query' :: gleam@option:option(geql@schema:object_type()),
    mutation :: gleam@option:option(geql@schema:object_type()),
    subscription :: gleam@option:option(geql@schema:object_type()),
    types :: gleam@dict:dict(binary(), geql@schema:type_definition())
}).
