-record(execution_context, {
    user_context :: gleam@dynamic:dynamic_(),
    data_loaders :: gleam@dict:dict(binary(), geql@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()))
}).
