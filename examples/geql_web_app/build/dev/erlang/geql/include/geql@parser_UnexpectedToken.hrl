-record(unexpected_token, {
    expected :: binary(),
    got :: geql@lexer:token(),
    position :: geql@lexer:position()
}).
