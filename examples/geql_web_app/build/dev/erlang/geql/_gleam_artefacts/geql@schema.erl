-module(geql@schema).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/geql/schema.gleam").
-export([execution_context/1, add_data_loader/3, get_data_loader/2, update_data_loader/3, schema/0, 'query'/2, mutation/2, subscription/2, add_type/2, object/1, description/2, field/2, implements/2, field_def/2, field_description/2, argument/2, resolver/2, arg/2, arg_description/2, default_value/2, string_type/0, int_type/0, float_type/0, boolean_type/0, id_type/0, named_type/1, non_null/1, list_type/1, scalar/1, scalar_description/2, serialize/2, parse_value/2, parse_literal/2, string_scalar/0, int_scalar/0, float_scalar/0, boolean_scalar/0, id_scalar/0]).
-export_type([schema/0, type_definition/0, object_type/0, field_definition/0, field_type/0, argument_definition/0, scalar_type/0, enum_type/0, enum_value_definition/0, interface_type/0, union_type/0, input_object_type/0, input_field_definition/0, execution_context/0, resolver_info/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type schema() :: {schema,
        gleam@option:option(object_type()),
        gleam@option:option(object_type()),
        gleam@option:option(object_type()),
        gleam@dict:dict(binary(), type_definition())}.

-type type_definition() :: {object_type_def, object_type()} |
    {scalar_type_def, scalar_type()} |
    {enum_type_def, enum_type()} |
    {interface_type_def, interface_type()} |
    {union_type_def, union_type()} |
    {input_object_type_def, input_object_type()}.

-type object_type() :: {object_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), field_definition()),
        list(interface_type())}.

-type field_definition() :: {field_definition,
        binary(),
        gleam@option:option(binary()),
        field_type(),
        gleam@dict:dict(binary(), argument_definition()),
        gleam@option:option(fun((resolver_info()) -> {ok,
                gleam@dynamic:dynamic_()} |
            {error, binary()}))}.

-type field_type() :: {non_null, field_type()} |
    {list, field_type()} |
    {named, binary()}.

-type argument_definition() :: {argument_definition,
        binary(),
        gleam@option:option(binary()),
        field_type(),
        gleam@option:option(gleam@dynamic:dynamic_())}.

-type scalar_type() :: {scalar_type,
        binary(),
        gleam@option:option(binary()),
        fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()}),
        fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()}),
        fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
            {error, binary()})}.

-type enum_type() :: {enum_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), enum_value_definition())}.

-type enum_value_definition() :: {enum_value_definition,
        binary(),
        gleam@option:option(binary()),
        gleam@dynamic:dynamic_()}.

-type interface_type() :: {interface_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), field_definition())}.

-type union_type() :: {union_type,
        binary(),
        gleam@option:option(binary()),
        list(object_type())}.

-type input_object_type() :: {input_object_type,
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), input_field_definition())}.

-type input_field_definition() :: {input_field_definition,
        binary(),
        gleam@option:option(binary()),
        field_type(),
        gleam@option:option(gleam@dynamic:dynamic_())}.

-type execution_context() :: {execution_context,
        gleam@dynamic:dynamic_(),
        gleam@dict:dict(binary(), geql@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()))}.

-type resolver_info() :: {resolver_info,
        gleam@option:option(gleam@dynamic:dynamic_()),
        gleam@dict:dict(binary(), gleam@dynamic:dynamic_()),
        execution_context(),
        gleam@dynamic:dynamic_()}.

-file("src/geql/schema.gleam", 137).
?DOC(" Create a new execution context\n").
-spec execution_context(gleam@dynamic:dynamic_()) -> execution_context().
execution_context(User_context) ->
    {execution_context, User_context, maps:new()}.

-file("src/geql/schema.gleam", 142).
?DOC(" Add a DataLoader to the execution context\n").
-spec add_data_loader(
    execution_context(),
    binary(),
    geql@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())
) -> execution_context().
add_data_loader(Context, Name, Loader) ->
    _record = Context,
    {execution_context,
        erlang:element(2, _record),
        gleam@dict:insert(erlang:element(3, Context), Name, Loader)}.

-file("src/geql/schema.gleam", 154).
?DOC(" Get a DataLoader from the execution context\n").
-spec get_data_loader(execution_context(), binary()) -> {ok,
        geql@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())} |
    {error, binary()}.
get_data_loader(Context, Name) ->
    case gleam_stdlib:map_get(erlang:element(3, Context), Name) of
        {ok, Loader} ->
            {ok, Loader};

        {error, _} ->
            {error,
                <<<<"DataLoader '"/utf8, Name/binary>>/binary,
                    "' not found in execution context"/utf8>>}
    end.

-file("src/geql/schema.gleam", 166).
?DOC(" Update a DataLoader in the execution context\n").
-spec update_data_loader(
    execution_context(),
    binary(),
    geql@dataloader:data_loader(gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_())
) -> execution_context().
update_data_loader(Context, Name, Loader) ->
    _record = Context,
    {execution_context,
        erlang:element(2, _record),
        gleam@dict:insert(erlang:element(3, Context), Name, Loader)}.

-file("src/geql/schema.gleam", 178).
-spec schema() -> schema().
schema() ->
    {schema, none, none, none, maps:new()}.

-file("src/geql/schema.gleam", 182).
-spec 'query'(schema(), object_type()) -> schema().
'query'(Schema, Query_type) ->
    _record = Schema,
    {schema,
        {some, Query_type},
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record)}.

-file("src/geql/schema.gleam", 186).
-spec mutation(schema(), object_type()) -> schema().
mutation(Schema, Mutation_type) ->
    _record = Schema,
    {schema,
        erlang:element(2, _record),
        {some, Mutation_type},
        erlang:element(4, _record),
        erlang:element(5, _record)}.

-file("src/geql/schema.gleam", 190).
-spec subscription(schema(), object_type()) -> schema().
subscription(Schema, Subscription_type) ->
    _record = Schema,
    {schema,
        erlang:element(2, _record),
        erlang:element(3, _record),
        {some, Subscription_type},
        erlang:element(5, _record)}.

-file("src/geql/schema.gleam", 194).
-spec add_type(schema(), type_definition()) -> schema().
add_type(Schema, Type_def) ->
    Type_name = case Type_def of
        {object_type_def, Obj} ->
            erlang:element(2, Obj);

        {scalar_type_def, Scalar} ->
            erlang:element(2, Scalar);

        {enum_type_def, Enum} ->
            erlang:element(2, Enum);

        {interface_type_def, Interface} ->
            erlang:element(2, Interface);

        {union_type_def, Union} ->
            erlang:element(2, Union);

        {input_object_type_def, Input} ->
            erlang:element(2, Input)
    end,
    _record = Schema,
    {schema,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        gleam@dict:insert(erlang:element(5, Schema), Type_name, Type_def)}.

-file("src/geql/schema.gleam", 208).
-spec object(binary()) -> object_type().
object(Name) ->
    {object_type, Name, none, maps:new(), []}.

-file("src/geql/schema.gleam", 212).
-spec description(object_type(), binary()) -> object_type().
description(Obj, Desc) ->
    _record = Obj,
    {object_type,
        erlang:element(2, _record),
        {some, Desc},
        erlang:element(4, _record),
        erlang:element(5, _record)}.

-file("src/geql/schema.gleam", 216).
-spec field(object_type(), field_definition()) -> object_type().
field(Obj, Field_def) ->
    _record = Obj,
    {object_type,
        erlang:element(2, _record),
        erlang:element(3, _record),
        gleam@dict:insert(
            erlang:element(4, Obj),
            erlang:element(2, Field_def),
            Field_def
        ),
        erlang:element(5, _record)}.

-file("src/geql/schema.gleam", 220).
-spec implements(object_type(), interface_type()) -> object_type().
implements(Obj, Interface) ->
    _record = Obj,
    {object_type,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        [Interface | erlang:element(5, Obj)]}.

-file("src/geql/schema.gleam", 225).
-spec field_def(binary(), field_type()) -> field_definition().
field_def(Name, Field_type) ->
    {field_definition, Name, none, Field_type, maps:new(), none}.

-file("src/geql/schema.gleam", 235).
-spec field_description(field_definition(), binary()) -> field_definition().
field_description(Field, Desc) ->
    _record = Field,
    {field_definition,
        erlang:element(2, _record),
        {some, Desc},
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("src/geql/schema.gleam", 242).
-spec argument(field_definition(), argument_definition()) -> field_definition().
argument(Field, Arg_def) ->
    _record = Field,
    {field_definition,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        gleam@dict:insert(
            erlang:element(5, Field),
            erlang:element(2, Arg_def),
            Arg_def
        ),
        erlang:element(6, _record)}.

-file("src/geql/schema.gleam", 252).
-spec resolver(
    field_definition(),
    fun((resolver_info()) -> {ok, gleam@dynamic:dynamic_()} | {error, binary()})
) -> field_definition().
resolver(Field, Resolve_fn) ->
    _record = Field,
    {field_definition,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        {some, Resolve_fn}}.

-file("src/geql/schema.gleam", 257).
-spec arg(binary(), field_type()) -> argument_definition().
arg(Name, Arg_type) ->
    {argument_definition, Name, none, Arg_type, none}.

-file("src/geql/schema.gleam", 266).
-spec arg_description(argument_definition(), binary()) -> argument_definition().
arg_description(Arg, Desc) ->
    _record = Arg,
    {argument_definition,
        erlang:element(2, _record),
        {some, Desc},
        erlang:element(4, _record),
        erlang:element(5, _record)}.

-file("src/geql/schema.gleam", 273).
-spec default_value(argument_definition(), gleam@dynamic:dynamic_()) -> argument_definition().
default_value(Arg, Value) ->
    _record = Arg,
    {argument_definition,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        {some, Value}}.

-file("src/geql/schema.gleam", 281).
-spec string_type() -> field_type().
string_type() ->
    {named, <<"String"/utf8>>}.

-file("src/geql/schema.gleam", 285).
-spec int_type() -> field_type().
int_type() ->
    {named, <<"Int"/utf8>>}.

-file("src/geql/schema.gleam", 289).
-spec float_type() -> field_type().
float_type() ->
    {named, <<"Float"/utf8>>}.

-file("src/geql/schema.gleam", 293).
-spec boolean_type() -> field_type().
boolean_type() ->
    {named, <<"Boolean"/utf8>>}.

-file("src/geql/schema.gleam", 297).
-spec id_type() -> field_type().
id_type() ->
    {named, <<"ID"/utf8>>}.

-file("src/geql/schema.gleam", 301).
-spec named_type(binary()) -> field_type().
named_type(Name) ->
    {named, Name}.

-file("src/geql/schema.gleam", 305).
-spec non_null(field_type()) -> field_type().
non_null(Inner) ->
    {non_null, Inner}.

-file("src/geql/schema.gleam", 309).
-spec list_type(field_type()) -> field_type().
list_type(Inner) ->
    {list, Inner}.

-file("src/geql/schema.gleam", 314).
-spec scalar(binary()) -> scalar_type().
scalar(Name) ->
    {scalar_type,
        Name,
        none,
        fun(Value) -> {ok, Value} end,
        fun(Value@1) -> {ok, Value@1} end,
        fun(Value@2) -> {ok, Value@2} end}.

-file("src/geql/schema.gleam", 324).
-spec scalar_description(scalar_type(), binary()) -> scalar_type().
scalar_description(Scalar, Desc) ->
    _record = Scalar,
    {scalar_type,
        erlang:element(2, _record),
        {some, Desc},
        erlang:element(4, _record),
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("src/geql/schema.gleam", 328).
-spec serialize(
    scalar_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> scalar_type().
serialize(Scalar, Serialize_fn) ->
    _record = Scalar,
    {scalar_type,
        erlang:element(2, _record),
        erlang:element(3, _record),
        Serialize_fn,
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("src/geql/schema.gleam", 335).
-spec parse_value(
    scalar_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> scalar_type().
parse_value(Scalar, Parse_fn) ->
    _record = Scalar,
    {scalar_type,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        Parse_fn,
        erlang:element(6, _record)}.

-file("src/geql/schema.gleam", 342).
-spec parse_literal(
    scalar_type(),
    fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
        {error, binary()})
) -> scalar_type().
parse_literal(Scalar, Parse_fn) ->
    _record = Scalar,
    {scalar_type,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        Parse_fn}.

-file("src/geql/schema.gleam", 350).
-spec string_scalar() -> scalar_type().
string_scalar() ->
    _pipe = scalar(<<"String"/utf8>>),
    scalar_description(
        _pipe,
        <<"The String scalar type represents textual data"/utf8>>
    ).

-file("src/geql/schema.gleam", 355).
-spec int_scalar() -> scalar_type().
int_scalar() ->
    _pipe = scalar(<<"Int"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Int scalar type represents non-fractional signed whole numeric values"/utf8>>
    ).

-file("src/geql/schema.gleam", 362).
-spec float_scalar() -> scalar_type().
float_scalar() ->
    _pipe = scalar(<<"Float"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Float scalar type represents signed double-precision fractional values"/utf8>>
    ).

-file("src/geql/schema.gleam", 369).
-spec boolean_scalar() -> scalar_type().
boolean_scalar() ->
    _pipe = scalar(<<"Boolean"/utf8>>),
    scalar_description(
        _pipe,
        <<"The Boolean scalar type represents true or false"/utf8>>
    ).

-file("src/geql/schema.gleam", 374).
-spec id_scalar() -> scalar_type().
id_scalar() ->
    _pipe = scalar(<<"ID"/utf8>>),
    scalar_description(
        _pipe,
        <<"The ID scalar type represents a unique identifier"/utf8>>
    ).
