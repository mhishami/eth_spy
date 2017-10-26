-module(eth_spy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile(route()),
  {ok, _} = cowboy:start_http(eth_spy, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  eth_spy_sup:start_link().

stop(_State) ->
  ok.

route() ->
  [
    {'_', [
      {"/", http_handler_main, #{}},      
      {"/eth/:from", http_handler_eth, #{}},
      {"/eth/:from/:to", http_handler_eth, #{}},
      {"/api/v1/ethereum/[...]", rest_handler_eth, #{}}
    ]}
  ].