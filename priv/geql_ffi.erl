-module(geql_ffi).
-export([coerce_to_dynamic/1, to_dynamic/1]).

coerce_to_dynamic(Value) ->
    Value.

to_dynamic(Value) ->
    Value.