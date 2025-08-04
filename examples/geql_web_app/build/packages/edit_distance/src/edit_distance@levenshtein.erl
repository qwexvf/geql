-module(edit_distance@levenshtein).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([distance/2]).

-spec update_distance_list(
    list(binary()),
    list(integer()),
    binary(),
    integer(),
    list(integer())
) -> list(integer()).
update_distance_list(Other, Distance_list, Grapheme, Last_distance, Acc) ->
    case Other of
        [] ->
            gleam@list:reverse(Acc);

        [First | Rest] ->
            case Distance_list of
                [] ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"panic expression evaluated"/utf8>>,
                            module => <<"edit_distance/levenshtein"/utf8>>,
                            function => <<"update_distance_list"/utf8>>,
                            line => 61});

                [First_dist | Rest_dist] ->
                    Difference = case Grapheme =:= First of
                        false ->
                            1;

                        true ->
                            0
                    end,
                    Min = case Rest_dist of
                        [Second_dist | _] ->
                            _pipe = gleam@int:min(
                                First_dist + Difference,
                                Last_distance + 1
                            ),
                            gleam@int:min(_pipe, Second_dist + 1);

                        _ ->
                            gleam@int:min(
                                First_dist + Difference,
                                Last_distance + 1
                            )
                    end,
                    update_distance_list(
                        Rest,
                        Rest_dist,
                        Grapheme,
                        Min,
                        [Min | Acc]
                    )
            end
    end.

-spec do_distance(list(binary()), list(binary()), list(integer()), integer()) -> integer().
do_distance(One, Other, Distance_list, Step) ->
    case One of
        [] ->
            gleam@result:unwrap(gleam@list:last(Distance_list), -1);

        [First | Rest] ->
            Distance_list@1 = update_distance_list(
                Other,
                Distance_list,
                First,
                Step,
                [Step]
            ),
            do_distance(Rest, Other, Distance_list@1, Step + 1)
    end.

-spec distance(binary(), binary()) -> integer().
distance(One, Other) ->
    case {One, Other} of
        {_, _} when One =:= Other ->
            0;

        {<<""/utf8>>, String} ->
            gleam@string:length(String);

        {String, <<""/utf8>>} ->
            gleam@string:length(String);

        {One@1, Other@1} ->
            One@2 = gleam@string:to_graphemes(One@1),
            Other@2 = gleam@string:to_graphemes(Other@1),
            Distance_list = gleam@list:range(0, gleam@list:length(Other@2)),
            do_distance(One@2, Other@2, Distance_list, 1)
    end.
