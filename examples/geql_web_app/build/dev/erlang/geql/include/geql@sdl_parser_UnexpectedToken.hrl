-record(unexpected_token, {
    expected :: binary(),
    got :: geql@sdl_lexer:s_d_l_token(),
    position :: geql@sdl_lexer:position()
}).
