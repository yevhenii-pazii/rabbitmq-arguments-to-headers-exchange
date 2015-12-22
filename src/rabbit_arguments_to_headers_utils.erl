%%%-------------------------------------------------------------------
%%% @author Evgeniy.Paziy <epaziy@softserveinc.com>
%%% @copyright (C) 2015, SoftServe inc.
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2015 10:24 AM
%%%-------------------------------------------------------------------
-module(rabbit_arguments_to_headers_utils).
-author("Evgeniy.Paziy <epaziy@softserveinc.com>").

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-spec contains_arguments(
    Delivery::rabbit_types:delivery(),
    Exchange::rabbit_types:exchange()) -> boolean().

-spec make_delivery(
    Delivery::rabbit_types:delivery(),
    Exchange::rabbit_types:exchange()) -> rabbit_types:delivery().

%% API
-export([make_delivery/2, contains_arguments/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


% Tested
contains_arguments(Delivery, #exchange{arguments = Arguments}) ->
  MessageHeaders = get_defined(msg_headers(Delivery), []),
  ExchangeArgumetns = get_defined(Arguments, []),
  has_arguments(MessageHeaders, ExchangeArgumetns).

% Tested
has_arguments(_Headers, []) -> true;
has_arguments(Headers, [Argument | Tail]) ->
  case has_argument(Headers, Argument) of
    false -> false;
    true -> has_arguments(Headers, Tail)
  end.

%% Tested
has_argument(Headers, {Key, Type, Value}) ->
  HeadersRow = table_lookup(Headers, Key),
  case HeadersRow of
    undefined -> false;
    _ -> {HeaderType, HeaderValue} = HeadersRow, (Type == HeaderType) and (Value == HeaderValue)
  end.



make_delivery(Delivery, #exchange{arguments = Arguments} = Exchange) ->
  MessageHeaders = get_defined(msg_headers(Delivery), []),
  ExchangeArgumetns = get_defined(Arguments, []),
  NewHeaders = make_headers(MessageHeaders, ExchangeArgumetns),
  set_delivery_headers(Delivery, NewHeaders).

% Tested
make_headers(Headers, []) -> Headers;
make_headers(Headers, [Arg | Tail] ) ->
  case has_argument(Headers, Arg) of
    true ->
      make_headers(Headers, Tail);
    false ->
      {Key, Type, Value} = Arg,
      make_headers(set_table_value(Headers, Key, Type, Value), Tail)
  end.

%% Tested
get_defined(Arg, Default) ->
  case Arg of
    undefined -> Default;
    _ -> Arg
  end.

%% Taken from rabbit_exchange_type_delayed_message
set_delivery_headers(Delivery, H) ->
  Msg = get_msg(Delivery),
  Content = get_content(Msg),
  Props = get_props(Content),

  Props2 = Props#'P_basic'{headers = H},
  Content2 = Content#content{properties = Props2},
  Msg2 = Msg#basic_message{content = Content2},

  Delivery#delivery{message = Msg2}.

%% Taken from rabbit_exchange_type_delayed_message
msg_headers(Delivery) ->
  lists:foldl(fun (F, Acc) -> F(Acc) end,
    Delivery,
    [fun get_msg/1, fun get_content/1,
      fun get_props/1, fun get_headers/1]).

get_msg(#delivery{message = Msg}) -> Msg.
get_content(#basic_message{content = Content}) -> Content.
get_props(#content{properties = Props}) -> Props.
get_headers(#'P_basic'{headers = H}) -> H.

table_lookup(Table, Key) ->
  case lists:keysearch(Key, 1, Table) of
    {value, {_, TypeBin, ValueBin}} -> {TypeBin, ValueBin};
    false                           -> undefined
  end.

set_table_value(Table, Key, Type, Value) ->
  sort_field_table(
    lists:keystore(Key, 1, Table, {Key, Type, Value})).

sort_field_table(Arguments) ->
  lists:keysort(1, Arguments).




-ifdef(TEST).

%% Tests for
%% @see get_defined/2
get_defined_method_undefined_test() ->
  %Expect, Expr
  Default = "default",
  ?assertEqual(Default, get_defined(undefined, Default)).

get_defined_method_defined_test() ->
  Arg = "arg",
  ?assertEqual(Arg, get_defined(Arg, "Default")).



%% Tests for
%% @see has_argument/2
has_argument_method_empty_headers_test() ->
  Headers = [],
  Arg = {<<"key">>, longstr, <<"value">>},
  ?assertNot(has_argument(Headers, Arg)).

has_argument_method_headers_dont_contain_header_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arg = {<<"arg">>, longstr, <<"value">>},
  ?assertNot(has_argument(Headers, Arg)).

has_argument_method_headers_contain_header_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arg = {<<"header">>, longstr, <<"value">>},
  ?assert(has_argument(Headers, Arg)).

has_argument_method_headers_contain_header_with_wrong_value_test() ->
  Headers = [{<<"header">>, longstr, <<"value1">>}],
  Arg = {<<"header">>, longstr, <<"value2">>},
  ?assertNot(has_argument(Headers, Arg)).

has_argument_method_headers_contain_header_with_type_test() ->
  Headers = [{<<"header">>, bool, <<"value">>}],
  Arg = {<<"header">>, longstr, <<"value">>},
  ?assertNot(has_argument(Headers, Arg)).

%% Tests for
%% @see has_arguments/2
has_arguments_method_empty_headers_and_arguments_test() ->
  Headers = [],
  Arguments = [],
  ?assert(has_arguments(Headers, Arguments)).

has_arguments_method_empty_headers_test() ->
  Headers = [],
  Arguments = [{<<"arg">>, longstr, <<"value">>}],
  ?assertNot(has_arguments(Headers, Arguments)).

has_arguments_method_headers_dont_contain_arguments_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arguments = [{<<"arg">>, longstr, <<"value">>}],
  ?assertNot(has_arguments(Headers, Arguments)).

has_arguments_method_headers_contain_arguments_test() ->
  Headers = [{<<"arg">>, longstr, <<"value">>}],
  Arguments = [{<<"arg">>, longstr, <<"value">>}],
  ?assert(has_arguments(Headers, Arguments)).

has_arguments_method_headers_contain_not_all_arguments_test() ->
  Headers = [{<<"arg">>, longstr, <<"value">>}],
  Arguments = [{<<"arg">>, longstr, <<"value">>}, {<<"arg2">>, longstr, <<"value">>}],
  ?assertNot(has_arguments(Headers, Arguments)).



%% Tests for
%% @see contains_arguments/2
contains_arguments_method_conteins_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arguments = [{<<"header">>, longstr, <<"value">>}],
  Delivery = #delivery{message = #basic_message{content = #content{properties = #'P_basic'{headers = Headers}}}},
  Exchange = #exchange{arguments = Arguments},
  ?assert(contains_arguments(Delivery, Exchange)).

contains_arguments_method_not_conteins_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arguments = [{<<"arg">>, longstr, <<"value">>}],
  Delivery = #delivery{message = #basic_message{content = #content{properties = #'P_basic'{headers = Headers}}}},
  Exchange = #exchange{arguments = Arguments},
  ?assertNot(contains_arguments(Delivery, Exchange)).


%% Tests for
%% @see make_headers/2
make_headers_method_empty_arguments_and_headers_test() ->
  Headers = [],
  Arguments = [],
  ?assertEqual([], make_headers(Headers, Arguments)).

make_headers_method_empty_arguments_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arguments = [],
  Result = [{<<"header">>, longstr, <<"value">>}],
  ?assertEqual(Result, make_headers(Headers, Arguments)).

make_headers_method_one_header_one_argument_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arguments = [{<<"arg">>, longstr, <<"value">>}],
  Result = [{<<"header">>, longstr, <<"value">>}, {<<"arg">>, longstr, <<"value">>}],
  ?assert(has_arguments(Result, make_headers(Headers, Arguments))).

make_headers_method_one_header_one_argument_different_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}, {<<"arg1">>, longstr, <<"value">>}],
  Arguments = [{<<"arg1">>, longstr, <<"value">>}, {<<"arg2">>, longstr, <<"value">>}],
  Result = [{<<"header">>, longstr, <<"value">>}, {<<"arg1">>, longstr, <<"value">>}, {<<"arg2">>, longstr, <<"value">>}],
  ?assert(has_arguments(Result, make_headers(Headers, Arguments))).

make_headers_method_wrong_value_test() ->
  Headers = [{<<"header">>, longstr, <<"value1">>}],
  Arguments = [{<<"header">>, longstr, <<"value2">>}],
  Result = [{<<"header">>, longstr, <<"value2">>}],
  ?assert(has_arguments(Result, make_headers(Headers, Arguments))).

%add_arguments(Delivery, Arguments) ->
%  MessageHeaders = get_defined(msg_headers(Delivery), []),
%  ExchangeArgumetns = get_defined(Arguments, []),
%  NewHeaders = make_headers(MessageHeaders, ExchangeArgumetns),
%  set_delivery_headers(Delivery, NewHeaders).
add_arguments_method_test() ->
  Headers = [{<<"header">>, longstr, <<"value">>}],
  Arguments = [{<<"arg">>, longstr, <<"value">>}],
  ExpectedHeaders = [{<<"arg">>, longstr, <<"value">>}, {<<"header">>, longstr, <<"value">>}],

  Exchange = #exchange{arguments = Arguments},
  Delivery = #delivery{message = #basic_message{content = #content{properties = #'P_basic'{headers = Headers}}}},

  ExpectDelivery = #delivery{message = #basic_message{content = #content{properties = #'P_basic'{headers = ExpectedHeaders}}}},

  ?assertEqual(ExpectDelivery, make_delivery(Delivery, Exchange)).

-endif.