-module(rest_handler_eth).
-include ("eth_spy.hrl").

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([route/2]).


init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.  

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, route}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, route}], Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

route(Req, State) ->
  {ok, Method, Path, Req1} = cowboy_helper:method_and_path(Req),
  ?DEBUG("~p: Processing ~p, ~p~n", [?MODULE, Method, Path]),
  route(Method, Path, Req1, State).

route(<<"GET">>, Path, Req, State) ->
  ?WARN("Invalid request: GET -> ~p~n", [Path]),
  {jsx:encode([{error, enoimpl}]), Req, State};

route(Method, Path, Req, State) ->
  ?WARN("Invalid request: ~p -> ~p~n", [Method, Path]),
  Resp = [{error, enoimpl}],
  {ok, Req1} = cowboy_helper:json_reply(Resp, Req),
  {false, Req1, State}.


