-module(sdl_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/sdl_test.gleam").
-export([main/0]).

-file("src/sdl_test.gleam", 3).
-spec main() -> nil.
main() ->
    geql@sdl_demo:demo_sdl_parsing().
