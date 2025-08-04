-record(connection, {
    body :: mist@internal@http:body(),
    socket :: glisten@socket:socket(),
    transport :: glisten@transport:transport(),
    client_ip :: {ok, {{integer(), integer(), integer(), integer()}, integer()}} |
        {error, nil}
}).
