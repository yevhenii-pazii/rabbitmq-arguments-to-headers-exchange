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

-import(rabbit_arguments_to_headers_utils, [add_arguments/2, contains_arguments/2]).

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

  Routs = rabbit_router:match_routing_key(Name, ['_']),
  case contains_arguments(Delivery, Arguments) of
    true -> Routs;
    false ->
      NewDelivery = add_arguments(Delivery, Arguments),
      rabbit_amqqueue:deliver(rabbit_amqqueue:lookup(Routs), NewDelivery),
      []
  end.


%  NewHeaders = make_headers(Arguments, Headers),
%  case Headers == NewHeaders of
%    false ->
      % TODO create new delivery
      %NewDelivery = Delivery#delivery{message = #basic_message{content = #content{properties = #'P_basic'{headers = Headers}}}},
      %Delivery#delivery.message#basic_message.content#content.
      %Content#content.payload_fragments_rev
      %NewMessage = rabbit_basic:message(Exchange, BasicMessage#basic_message.routing_keys, [{headers, table, NewHeaders}], <<>>),
      %NewDelivery = rabbit_basic:delivery(Delivery#delivery.mandatory, Delivery#delivery.confirm, NewMessage, Delivery#delivery.msg_seq_no),
%      rabbit_amqqueue:deliver(rabbit_amqqueue:lookup(Routs), Delivery), %rabbit_basic:publish(Exchange, Delivery),
%      [];
%    true -> Routs
%  end.

validate_binding(_X, _B) -> ok.


validate(_Exchange) -> ok.
create(_Tx, _X) -> ok.
delete(_Tx, _X, _Bs) -> ok.
policy_changed(_X1, _X2) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
assert_args_equivalence(Exchange, Args) ->
  rabbit_exchange:assert_args_equivalence(Exchange, Args).
