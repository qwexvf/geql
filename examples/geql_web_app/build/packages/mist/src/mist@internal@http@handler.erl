-module(mist@internal@http@handler).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([initial_state/0, call/4]).
-export_type([state/0]).

-type state() :: {state, gleam@option:option(gleam@erlang@process:timer())}.

-spec initial_state() -> state().
initial_state() ->
    {state, none}.

-spec log_and_error(
    gleam@erlang:crash(),
    glisten@socket:socket(),
    glisten@transport:transport(),
    gleam@http@request:request(mist@internal@http:connection())
) -> gleam@erlang@process:exit_reason().
log_and_error(Error, Socket, Transport, Req) ->
    case Error of
        {exited, Msg} ->
            logging:log(error, gleam@string:inspect(Error)),
            _ = begin
                _pipe = gleam@http@response:new(500),
                _pipe@1 = gleam@http@response:set_body(
                    _pipe,
                    gleam_stdlib:wrap_list(<<"Internal Server Error"/utf8>>)
                ),
                _pipe@2 = gleam@http@response:prepend_header(
                    _pipe@1,
                    <<"content-length"/utf8>>,
                    <<"21"/utf8>>
                ),
                _pipe@3 = mist@internal@http:add_default_headers(
                    _pipe@2,
                    true,
                    erlang:element(2, Req) =:= head
                ),
                _pipe@4 = mist@internal@encoder:to_bytes_builder(_pipe@3),
                glisten@transport:send(Transport, Socket, _pipe@4)
            end,
            _ = glisten@transport:close(Transport, Socket),
            {abnormal, gleam@string:inspect(Msg)};

        {thrown, Msg} ->
            logging:log(error, gleam@string:inspect(Error)),
            _ = begin
                _pipe = gleam@http@response:new(500),
                _pipe@1 = gleam@http@response:set_body(
                    _pipe,
                    gleam_stdlib:wrap_list(<<"Internal Server Error"/utf8>>)
                ),
                _pipe@2 = gleam@http@response:prepend_header(
                    _pipe@1,
                    <<"content-length"/utf8>>,
                    <<"21"/utf8>>
                ),
                _pipe@3 = mist@internal@http:add_default_headers(
                    _pipe@2,
                    true,
                    erlang:element(2, Req) =:= head
                ),
                _pipe@4 = mist@internal@encoder:to_bytes_builder(_pipe@3),
                glisten@transport:send(Transport, Socket, _pipe@4)
            end,
            _ = glisten@transport:close(Transport, Socket),
            {abnormal, gleam@string:inspect(Msg)};

        {errored, Msg} ->
            logging:log(error, gleam@string:inspect(Error)),
            _ = begin
                _pipe = gleam@http@response:new(500),
                _pipe@1 = gleam@http@response:set_body(
                    _pipe,
                    gleam_stdlib:wrap_list(<<"Internal Server Error"/utf8>>)
                ),
                _pipe@2 = gleam@http@response:prepend_header(
                    _pipe@1,
                    <<"content-length"/utf8>>,
                    <<"21"/utf8>>
                ),
                _pipe@3 = mist@internal@http:add_default_headers(
                    _pipe@2,
                    true,
                    erlang:element(2, Req) =:= head
                ),
                _pipe@4 = mist@internal@encoder:to_bytes_builder(_pipe@3),
                glisten@transport:send(Transport, Socket, _pipe@4)
            end,
            _ = glisten@transport:close(Transport, Socket),
            {abnormal, gleam@string:inspect(Msg)}
    end.

-spec close_or_set_timer(
    gleam@http@response:response(mist@internal@http:response_data()),
    mist@internal@http:connection(),
    gleam@erlang@process:subject(glisten@internal@handler:message(any()))
) -> {ok, state()} | {error, gleam@erlang@process:exit_reason()}.
close_or_set_timer(Resp, Conn, Sender) ->
    case gleam@http@response:get_header(Resp, <<"connection"/utf8>>) of
        {ok, <<"close"/utf8>>} ->
            _ = glisten@transport:close(
                erlang:element(4, Conn),
                erlang:element(3, Conn)
            ),
            {error, normal};

        _ ->
            Timer = gleam@erlang@process:send_after(
                Sender,
                10000,
                {internal, close}
            ),
            {ok, {state, {some, Timer}}}
    end.

-spec handle_file_body(
    gleam@http@response:response(mist@internal@http:response_data()),
    mist@internal@http:response_data(),
    mist@internal@http:connection()
) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
handle_file_body(Resp, Body, Conn) ->
    {file, File_descriptor, Offset, Length} = case Body of
        {file, _, _, _} -> Body;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"mist/internal/http/handler"/utf8>>,
                        function => <<"handle_file_body"/utf8>>,
                        line => 137})
    end,
    Return = begin
        _pipe = Resp,
        _pipe@1 = gleam@http@response:prepend_header(
            _pipe,
            <<"content-length"/utf8>>,
            gleam@int:to_string(Length - Offset)
        ),
        _pipe@2 = gleam@http@response:set_body(
            _pipe@1,
            gleam@bytes_builder:new()
        ),
        _pipe@3 = (fun(R) ->
            mist@internal@encoder:response_builder(
                erlang:element(2, Resp),
                erlang:element(3, R)
            )
        end)(_pipe@2),
        _pipe@4 = glisten@transport:send(
            erlang:element(4, Conn),
            erlang:element(3, Conn),
            _pipe@3
        ),
        _pipe@6 = gleam@result:then(
            _pipe@4,
            fun(_) ->
                _pipe@5 = mist@internal@file:sendfile(
                    erlang:element(4, Conn),
                    File_descriptor,
                    erlang:element(3, Conn),
                    Offset,
                    Length,
                    []
                ),
                gleam@result:map_error(
                    _pipe@5,
                    fun(Err) ->
                        logging:log(
                            error,
                            <<"Failed to send file: "/utf8,
                                (gleam@string:inspect(Err))/binary>>
                        ),
                        badarg
                    end
                )
            end
        ),
        gleam@result:replace(_pipe@6, nil)
    end,
    case mist_ffi:file_close(File_descriptor) of
        {ok, _} ->
            nil;

        {error, Reason} ->
            logging:log(
                error,
                <<"Failed to close file: "/utf8,
                    (gleam@string:inspect(Reason))/binary>>
            )
    end,
    Return.

-spec handle_bytes_builder_body(
    gleam@http@response:response(mist@internal@http:response_data()),
    gleam@bytes_builder:bytes_builder(),
    mist@internal@http:connection(),
    gleam@http@request:request(mist@internal@http:connection())
) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
handle_bytes_builder_body(Resp, Body, Conn, Req) ->
    _pipe = Resp,
    _pipe@1 = gleam@http@response:set_body(_pipe, Body),
    _pipe@2 = mist@internal@http:add_default_headers(
        _pipe@1,
        true,
        erlang:element(2, Req) =:= head
    ),
    _pipe@3 = mist@internal@encoder:to_bytes_builder(_pipe@2),
    glisten@transport:send(
        erlang:element(4, Conn),
        erlang:element(3, Conn),
        _pipe@3
    ).

-spec int_to_hex(integer()) -> binary().
int_to_hex(Int) ->
    erlang:integer_to_list(Int, 16).

-spec handle_chunked_body(
    gleam@http@response:response(mist@internal@http:response_data()),
    gleam@iterator:iterator(gleam@bytes_builder:bytes_builder()),
    mist@internal@http:connection()
) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
handle_chunked_body(Resp, Body, Conn) ->
    Headers = [{<<"transfer-encoding"/utf8>>, <<"chunked"/utf8>>} |
        erlang:element(3, Resp)],
    Initial_payload = mist@internal@encoder:response_builder(
        erlang:element(2, Resp),
        Headers
    ),
    _pipe = glisten@transport:send(
        erlang:element(4, Conn),
        erlang:element(3, Conn),
        Initial_payload
    ),
    _pipe@8 = gleam@result:then(_pipe, fun(_) -> _pipe@1 = Body,
            _pipe@2 = gleam@iterator:append(
                _pipe@1,
                gleam@iterator:from_list([gleam@bytes_builder:new()])
            ),
            gleam@iterator:try_fold(
                _pipe@2,
                nil,
                fun(_, Chunk) ->
                    Size = erlang:iolist_size(Chunk),
                    Encoded = begin
                        _pipe@3 = Size,
                        _pipe@4 = int_to_hex(_pipe@3),
                        _pipe@5 = gleam_stdlib:wrap_list(_pipe@4),
                        _pipe@6 = gleam@bytes_builder:append_string(
                            _pipe@5,
                            <<"\r\n"/utf8>>
                        ),
                        _pipe@7 = gleam_stdlib:iodata_append(_pipe@6, Chunk),
                        gleam@bytes_builder:append_string(
                            _pipe@7,
                            <<"\r\n"/utf8>>
                        )
                    end,
                    glisten@transport:send(
                        erlang:element(4, Conn),
                        erlang:element(3, Conn),
                        Encoded
                    )
                end
            ) end),
    gleam@result:replace(_pipe@8, nil).

-spec call(
    gleam@http@request:request(mist@internal@http:connection()),
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist@internal@http:response_data())),
    mist@internal@http:connection(),
    gleam@erlang@process:subject(glisten@internal@handler:message(any()))
) -> {ok, state()} | {error, gleam@erlang@process:exit_reason()}.
call(Req, Handler, Conn, Sender) ->
    _pipe = gleam_erlang_ffi:rescue(fun() -> Handler(Req) end),
    _pipe@1 = gleam@result:map_error(
        _pipe,
        fun(_capture) ->
            log_and_error(
                _capture,
                erlang:element(3, Conn),
                erlang:element(4, Conn),
                Req
            )
        end
    ),
    gleam@result:then(_pipe@1, fun(Resp) -> case Resp of
                {response, _, _, {websocket, Selector}} ->
                    _ = gleam_erlang_ffi:select(Selector),
                    {error, normal};

                {response, _, _, {server_sent_events, Selector}} ->
                    _ = gleam_erlang_ffi:select(Selector),
                    {error, normal};

                {response, _, _, Body} = Resp@1 ->
                    _pipe@2 = case Body of
                        {bytes, Body@1} ->
                            handle_bytes_builder_body(Resp@1, Body@1, Conn, Req);

                        {chunked, Body@2} ->
                            handle_chunked_body(Resp@1, Body@2, Conn);

                        {file, _, _, _} ->
                            handle_file_body(Resp@1, Body, Conn);

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"This shouldn't ever happen 🤞"/utf8>>,
                                    module => <<"mist/internal/http/handler"/utf8>>,
                                    function => <<"call"/utf8>>,
                                    line => 51})
                    end,
                    _pipe@3 = gleam@result:replace_error(_pipe@2, normal),
                    gleam@result:then(
                        _pipe@3,
                        fun(_) -> close_or_set_timer(Resp@1, Conn, Sender) end
                    )
            end end).
