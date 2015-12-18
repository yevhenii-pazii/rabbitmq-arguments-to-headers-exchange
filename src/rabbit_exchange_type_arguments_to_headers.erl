%%%-------------------------------------------------------------------
%%% @author Evgeniy.Paziy <epaziy@softserveinc.com>
%%% @copyright (C) 2015, SoftServe inc.
%%% @doc
%%%
%%% @end
%%% Created : 16. Dec 2015 11:45 AM
%%%-------------------------------------------------------------------
-module(rabbit_exchange_type_arguments_to_headers).
-author("Evgeniy.Paziy <epaziy@softserveinc.com>").

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-behaviour(rabbit_exchange_type).

%% API
-export([
  description/0,
  serialise_events/0,
  route/2,
  validate/1,
  validate_binding/2,
  create/2,
  delete/3,
  policy_changed/2,
  add_binding/3,
  remove_bindings/3,
  assert_args_equivalence/2
]).

-rabbit_boot_step(
  {?MODULE,
    [{description, "exchange type argument-to-header"},
      {mfa,         {rabbit_registry, register, [exchange, <<"argument">>, ?MODULE]}},
      {cleanup, {rabbit_registry, unregister, [exchange, <<"argumente">>]}},
      {requires,    rabbit_registry},
      {enables,     kernel_ready}]
  }
).


description() ->
  [{name, <<"argument">>},
   {description, <<"Adds exchange argumets to message headers">>}].


serialise_events() -> false.

route(Exchange, Delivery) ->
  #exchange{name = Name, arguments = Arguments} = Exchange,
  BasicMessage = Delivery#delivery.message,
  Content = BasicMessage#basic_message.content,
  Headers = rabbit_basic:extract_headers(Content),

  NewHeaders = make_headers(Arguments, Headers),


  Routs = rabbit_router:match_routing_key(Name, ['_']),
  case Headers == NewHeaders of
    false ->
      % TODO create new delivery
      %NewDelivery = Delivery#delivery{message = #basic_message{content = #content{properties = #'P_basic'{headers = Headers}}}},
      rabbit_amqqueue:deliver(rabbit_amqqueue:lookup(Routs), Delivery), %rabbit_basic:publish(Exchange, Delivery),
      [];
    true -> Routs
  end.

make_headers(undefined, undefined) -> [];
make_headers(undefined, Headers) -> Headers;
make_headers(Arguments, undefined) -> Arguments;
make_headers([], Headers) -> Headers;
make_headers(Arguments, []) -> Arguments;
make_headers(Arguments, [{Header, _, _} = Head | Tail]) ->
  case  lists:keysearch(Header, 1, Arguments) of
    {value, _} -> make_headers(Arguments, Tail);
    false -> make_headers(lists:append([Head], Arguments), Tail)
  end.

validate_binding(_X, _B) -> ok.


validate(_Exchange) -> ok.
create(_Tx, _X) -> ok.
delete(_Tx, _X, _Bs) -> ok.
policy_changed(_X1, _X2) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
assert_args_equivalence(Exchange, Args) ->
  rabbit_exchange:assert_args_equivalence(Exchange, Args).
