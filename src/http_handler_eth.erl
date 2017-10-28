-module(http_handler_eth).
-behaviour(cowboy_http_handler).
-include ("eth_spy.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {From, Req1} = cowboy_req:binding(from, Req),
  {To, Req2} = cowboy_req:binding(to, Req1),
  ?DEBUG("From: ~p, To: ~p~n", [From, To]),
  
  End = case To of
          undefined ->
            {ok, Num} = eth_svr:get_block_number(),
            Num;
          _ -> binary_to_integer(To)
        end,
  ?INFO("Processing ETH data from ~p to ~p~n", [From, End]),
  {ok, BlockInfo} = eth_svr:get_blocks(binary_to_integer(From), End),

  {ok, Req3} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json">>}
  ], jsx:encode(BlockInfo), Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.
