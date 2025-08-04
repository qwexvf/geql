-module(geql@dataloader).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/geql/dataloader.gleam").
-export([default_options/0, new_with_options/2, new/1, clear_cache/1, clear_key/2, prime/3, prime_error/3, load/2, load_many/2]).
-export_type([data_loader_options/0, data_loader_state/2, data_loader/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type data_loader_options() :: {data_loader_options, integer(), boolean()}.

-type data_loader_state(NHA, NHB) :: {data_loader_state,
        fun((list(NHA)) -> {ok, list({ok, NHB} | {error, binary()})} |
            {error, binary()}),
        data_loader_options(),
        gleam@dict:dict(NHA, {ok, NHB} | {error, binary()}),
        list(NHA),
        boolean()}.

-opaque data_loader(NHC, NHD) :: {data_loader, data_loader_state(NHC, NHD)}.

-file("src/geql/dataloader.gleam", 30).
?DOC(" Default DataLoader options\n").
-spec default_options() -> data_loader_options().
default_options() ->
    {data_loader_options, 100, true}.

-file("src/geql/dataloader.gleam", 61).
?DOC(" Create a new DataLoader instance with custom options\n").
-spec new_with_options(
    fun((list(NHS)) -> {ok, list({ok, NHT} | {error, binary()})} |
        {error, binary()}),
    data_loader_options()
) -> data_loader(NHS, NHT).
new_with_options(Batch_load_fn, Options) ->
    {data_loader,
        {data_loader_state, Batch_load_fn, Options, maps:new(), [], false}}.

-file("src/geql/dataloader.gleam", 56).
?DOC(" Create a new DataLoader instance\n").
-spec new(
    fun((list(NHM)) -> {ok, list({ok, NHN} | {error, binary()})} |
        {error, binary()})
) -> data_loader(NHM, NHN).
new(Batch_load_fn) ->
    new_with_options(Batch_load_fn, default_options()).

-file("src/geql/dataloader.gleam", 112).
?DOC(" Clear the cache for this DataLoader\n").
-spec clear_cache(data_loader(NIQ, NIR)) -> data_loader(NIQ, NIR).
clear_cache(Loader) ->
    {data_loader, State} = Loader,
    {data_loader,
        begin
            _record = State,
            {data_loader_state,
                erlang:element(2, _record),
                erlang:element(3, _record),
                maps:new(),
                erlang:element(5, _record),
                erlang:element(6, _record)}
        end}.

-file("src/geql/dataloader.gleam", 118).
?DOC(" Clear a specific key from the cache\n").
-spec clear_key(data_loader(NIW, NIX), NIW) -> data_loader(NIW, NIX).
clear_key(Loader, Key) ->
    {data_loader, State} = Loader,
    {data_loader,
        begin
            _record = State,
            {data_loader_state,
                erlang:element(2, _record),
                erlang:element(3, _record),
                gleam@dict:delete(erlang:element(4, State), Key),
                erlang:element(5, _record),
                erlang:element(6, _record)}
        end}.

-file("src/geql/dataloader.gleam", 128).
?DOC(
    " Prime the cache with a key-value pair\n"
    " Useful when you already have data and want to avoid future loads\n"
).
-spec prime(data_loader(NJC, NJD), NJC, NJD) -> data_loader(NJC, NJD).
prime(Loader, Key, Value) ->
    case Loader of
        {data_loader, State} when erlang:element(3, erlang:element(3, State)) ->
            New_cache = gleam@dict:insert(
                erlang:element(4, State),
                Key,
                {ok, Value}
            ),
            {data_loader,
                begin
                    _record = State,
                    {data_loader_state,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        New_cache,
                        erlang:element(5, _record),
                        erlang:element(6, _record)}
                end};

        _ ->
            Loader
    end.

-file("src/geql/dataloader.gleam", 143).
?DOC(" Prime the cache with an error for a specific key\n").
-spec prime_error(data_loader(NJI, NJJ), NJI, binary()) -> data_loader(NJI, NJJ).
prime_error(Loader, Key, Error) ->
    case Loader of
        {data_loader, State} when erlang:element(3, erlang:element(3, State)) ->
            New_cache = gleam@dict:insert(
                erlang:element(4, State),
                Key,
                {error, Error}
            ),
            {data_loader,
                begin
                    _record = State,
                    {data_loader_state,
                        erlang:element(2, _record),
                        erlang:element(3, _record),
                        New_cache,
                        erlang:element(5, _record),
                        erlang:element(6, _record)}
                end};

        _ ->
            Loader
    end.

-file("src/geql/dataloader.gleam", 183).
?DOC(" Determine if we should execute the current batch\n").
-spec should_execute_batch(data_loader_state(any(), any())) -> boolean().
should_execute_batch(State) ->
    Batch_size = erlang:length(erlang:element(5, State)),
    (Batch_size >= erlang:element(2, erlang:element(3, State))) orelse (Batch_size
    > 0).

-file("src/geql/dataloader.gleam", 272).
?DOC(" Get element at specific index from a list\n").
-spec get_at_index(list(NLE), integer()) -> {ok, NLE} | {error, nil}.
get_at_index(List, Index) ->
    case {List, Index} of
        {[], _} ->
            {error, nil};

        {[First | _], 0} ->
            {ok, First};

        {[_ | Rest], I} when I > 0 ->
            get_at_index(Rest, I - 1);

        {_, _} ->
            {error, nil}
    end.

-file("src/geql/dataloader.gleam", 282).
?DOC(" Find the index of a key in a list\n").
-spec find_key_index(list(NLI), NLI, integer()) -> gleam@option:option(integer()).
find_key_index(Keys, Target_key, Current_index) ->
    case Keys of
        [] ->
            none;

        [First | Rest] ->
            case First =:= Target_key of
                true ->
                    {some, Current_index};

                false ->
                    find_key_index(Rest, Target_key, Current_index + 1)
            end
    end.

-file("src/geql/dataloader.gleam", 255).
?DOC(" Find the result for a specific key from the batch results\n").
-spec find_result_for_key(list(NKW), list({ok, NKY} | {error, binary()}), NKW) -> {ok,
        NKY} |
    {error, binary()}.
find_result_for_key(Keys, Results, Target_key) ->
    case find_key_index(Keys, Target_key, 0) of
        {some, Index} ->
            case get_at_index(Results, Index) of
                {ok, Result} ->
                    Result;

                {error, _} ->
                    {error, <<"Result not found at expected index"/utf8>>}
            end;

        none ->
            {error, <<"Key not found in batch results"/utf8>>}
    end.

-file("src/geql/dataloader.gleam", 299).
?DOC(" Helper function to fold over two lists simultaneously\n").
-spec fold2(
    list(NLL),
    list({ok, NLN} | {error, binary()}),
    gleam@dict:dict(NLL, {ok, NLN} | {error, binary()}),
    fun((gleam@dict:dict(NLL, {ok, NLN} | {error, binary()}), NLL, {ok, NLN} |
        {error, binary()}) -> gleam@dict:dict(NLL, {ok, NLN} | {error, binary()}))
) -> gleam@dict:dict(NLL, {ok, NLN} | {error, binary()}).
fold2(Keys, Results, Cache, F) ->
    case {Keys, Results} of
        {[], []} ->
            Cache;

        {[Key | Rest_keys], [Result | Rest_results]} ->
            New_cache = F(Cache, Key, Result),
            fold2(Rest_keys, Rest_results, New_cache, F);

        {_, _} ->
            Cache
    end.

-file("src/geql/dataloader.gleam", 244).
?DOC(" Cache the results from a batch operation\n").
-spec cache_batch_results(
    gleam@dict:dict(NKI, {ok, NKJ} | {error, binary()}),
    list(NKI),
    list({ok, NKJ} | {error, binary()})
) -> gleam@dict:dict(NKI, {ok, NKJ} | {error, binary()}).
cache_batch_results(Cache, Keys, Results) ->
    fold2(
        Keys,
        Results,
        Cache,
        fun(Acc_cache, Key, Result) ->
            gleam@dict:insert(Acc_cache, Key, Result)
        end
    ).

-file("src/geql/dataloader.gleam", 189).
?DOC(" Execute the current batch of pending keys\n").
-spec execute_batch(data_loader(NKA, NKB), NKA) -> {data_loader(NKA, NKB),
    {ok, NKB} | {error, binary()}}.
execute_batch(Loader, Requested_key) ->
    {data_loader, State} = Loader,
    case erlang:element(5, State) of
        [] ->
            {Loader, {error, <<"No keys to batch"/utf8>>}};

        Pending_keys ->
            Unique_keys = gleam@list:unique(lists:reverse(Pending_keys)),
            case (erlang:element(2, State))(Unique_keys) of
                {ok, Results} ->
                    case erlang:length(Results) =:= erlang:length(Unique_keys) of
                        true ->
                            New_cache = case erlang:element(
                                3,
                                erlang:element(3, State)
                            ) of
                                true ->
                                    cache_batch_results(
                                        erlang:element(4, State),
                                        Unique_keys,
                                        Results
                                    );

                                false ->
                                    erlang:element(4, State)
                            end,
                            New_state = begin
                                _record = State,
                                {data_loader_state,
                                    erlang:element(2, _record),
                                    erlang:element(3, _record),
                                    New_cache,
                                    [],
                                    false}
                            end,
                            Result = find_result_for_key(
                                Unique_keys,
                                Results,
                                Requested_key
                            ),
                            {{data_loader, New_state}, Result};

                        false ->
                            Error = <<"Batch load function returned wrong number of results"/utf8>>,
                            {Loader, {error, Error}}
                    end;

                {error, Batch_error} ->
                    {Loader,
                        {error,
                            <<"Batch load failed: "/utf8, Batch_error/binary>>}}
            end
    end.

-file("src/geql/dataloader.gleam", 160).
?DOC(" Load a key without checking cache\n").
-spec load_uncached(data_loader(NJO, NJP), NJO) -> {data_loader(NJO, NJP),
    {ok, NJP} | {error, binary()}}.
load_uncached(Loader, Key) ->
    {data_loader, State} = Loader,
    New_pending = [Key | erlang:element(5, State)],
    Updated_state = begin
        _record = State,
        {data_loader_state,
            erlang:element(2, _record),
            erlang:element(3, _record),
            erlang:element(4, _record),
            New_pending,
            erlang:element(6, _record)}
    end,
    case should_execute_batch(Updated_state) of
        true ->
            execute_batch({data_loader, Updated_state}, Key);

        false ->
            execute_batch({data_loader, Updated_state}, Key)
    end.

-file("src/geql/dataloader.gleam", 76).
?DOC(
    " Load a single value by key\n"
    " This is the primary API - multiple calls to load() will be batched together\n"
).
-spec load(data_loader(NHY, NHZ), NHY) -> {data_loader(NHY, NHZ),
    {ok, NHZ} | {error, binary()}}.
load(Loader, Key) ->
    {data_loader, State} = Loader,
    case erlang:element(3, erlang:element(3, State)) of
        true ->
            case gleam_stdlib:map_get(erlang:element(4, State), Key) of
                {ok, Cached_result} ->
                    {Loader, Cached_result};

                {error, _} ->
                    load_uncached(Loader, Key)
            end;

        false ->
            load_uncached(Loader, Key)
    end.

-file("src/geql/dataloader.gleam", 96).
?DOC(
    " Load multiple values by keys\n"
    " More efficient than calling load() multiple times\n"
).
-spec load_many(data_loader(NIG, NIH), list(NIG)) -> {data_loader(NIG, NIH),
    list({ok, NIH} | {error, binary()})}.
load_many(Loader, Keys) ->
    {Final_loader, Results} = gleam@list:fold(
        Keys,
        {Loader, []},
        fun(Acc, Key) ->
            {Current_loader, Results_so_far} = Acc,
            {New_loader, Result} = load(Current_loader, Key),
            {New_loader, [Result | Results_so_far]}
        end
    ),
    {Final_loader, lists:reverse(Results)}.
