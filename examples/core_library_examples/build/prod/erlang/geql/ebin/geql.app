{application, geql, [
    {vsn, "1.0.0"},
    {applications, [gleam_stdlib]},
    {description, ""},
    {modules, [geql,
               geql@ast,
               geql@dataloader,
               geql@executor,
               geql@lexer,
               geql@parser,
               geql@schema,
               geql@schema_gen]},
    {registered, []}
]}.
