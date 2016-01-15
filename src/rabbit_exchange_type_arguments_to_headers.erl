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

-import(rabbit_arguments_to_headers_utils, [make_poperties/2, contains_arguments/2]).

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
    {cleanup,     {rabbit_registry, unregister, [exchange, <<"argument">>]}},
    {requires,    rabbit_registry},
    {enables,     kernel_ready}]
}
).


description() ->
  [{name, <<"argument">>},
    {description, <<"Adds exchange argumets to message headers">>}].


serialise_events() -> false.

route(#exchange{name = Name} = Exchange, Delivery) ->
  error_logger:info_msg("Exchange: ~p~n", [Exchange]),

  case contains_arguments(Delivery, Exchange) of
    true ->
      rabbit_router:match_routing_key(Name, ['_']);
    false ->
      rabbit_basic:publish(Exchange, get_routing_key(Delivery), make_poperties(Delivery, Exchange), get_body(Delivery)),
      []
  end.


get_body(#delivery{message = #basic_message{content = #content{payload_fragments_rev = Body}}}) -> Body.
get_routing_key(#delivery{message = #basic_message{routing_keys = RK}}) -> RK.


validate_binding(_X, _B) -> ok.

validate(_Exchange) -> ok.

create(Tx, Exchange) ->
  error_logger:info_msg("Created Exchange:~nTransaction:~p~nExchange:~p~n", [Tx, Exchange]),
  ok.
delete(Tx, Exchange, Bindings) ->
  error_logger:info_msg("Deleted exchange:~nTransaction:~p~nExchange:~p~nBinding:~p~n", [Tx, Exchange, Bindings]),
  ok.

policy_changed(_X1, _X2) -> ok.

add_binding(Tx, Exchange, Binding) ->
  error_logger:info_msg("Added binding:~nTransaction:~p~nExchange:~p~nBinding:~p~n", [Tx, Exchange, Binding]),
  ok.

remove_bindings(Tx, Exchange, Bindings) ->
  error_logger:info_msg("Removed binding:~nTransaction:~p~nExchange:~p~nBinding:~p~n", [Tx, Exchange, Bindings]),
  ok.

assert_args_equivalence(Exchange, Args) ->
  rabbit_exchange:assert_args_equivalence(Exchange, Args).
