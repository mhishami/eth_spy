-module(eth_svr).
-behaviour(gen_server).
-include ("eth_spy.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, { url, pool }).

-define(SERVER, ?MODULE).
-define(ETH_POOL, ethereum_pool).

%% Ethereum Units
-define(ETH,     1000000000000000000).
-define(FINNEY,  1000000000000000).
-define(SHANNON, 1000000000).

% env variables
-define(ETH_URL, <<"ETH_URL">>).

%% API.
-export ([get_addresses/0]).
-export ([get_block_number/0]).
-export ([get_block/1]).
-export ([get_blocks/2]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  % gen_server:start_link(?MODULE, [], []).
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.
get_addresses() ->
  gen_server:call(?SERVER, {get_addresses}).

get_block_number() ->
  gen_server:call(?SERVER, {get_block_number}).

get_block(BlockNumber) when is_integer(BlockNumber) ->
  gen_server:call(?SERVER, {get_block_by_num, BlockNumber}).

get_blocks(From, To) when is_integer(From), 
                          is_integer(To),
                          From < To ->
  gen_server:call(?SERVER, {get_blocks_by_num, From, To}, infinity).

init([]) ->
  process_flag(trap_exit, true),
  Options = [{timeout, 150000}, {max_connections, 1000}],
  ok = hackney_pool:start_pool(?ETH_POOL, Options),
  %% some defaults value
  URL = <<"http://localhost:8545">>,

  {ok, #state{
    url = get_env(?ETH_URL, URL, list_to_binary),
    pool = ?ETH_POOL
  }}.

handle_call({get_addresses}, _From, #state{url=URL, pool=Pool} = State) ->
  Payload = [
    {id, util:uuid()},
    {jsonrpc, <<"2.0">>},
    {method, <<"eth_accounts">>},
    {params, []}
  ],
  Resp = util:call(post, URL, Pool, Payload),
  {reply, Resp, State};

handle_call({get_block_number}, _From, #state{url=URL, pool=Pool} = State) ->
  Payload = [
    {id, util:uuid()},
    {jsonrpc, <<"2.0">>},
    {method, <<"eth_blockNumber">>},
    {params, []}
  ],
  {ok, Num} = util:call(post, URL, Pool, Payload),
  {reply, {ok, hex_to_int(Num)}, State};

handle_call({get_block_by_num, BlockNumber}, _From, #state{url=URL, pool=Pool} = State) ->
  Num = int_to_hex(BlockNumber),
  Payload = [
    {id, util:uuid()},
    {jsonrpc, <<"2.0">>},
    {method, <<"eth_getBlockByNumber">>},
    {params, [Num, true]}
  ],
  {ok, Resp} = util:call(post, URL, Pool, Payload),
  {ok, Resp2} = sanitize_block(Resp),
  {reply, {ok, Resp2}, State};

handle_call({get_blocks_by_num, From, To}, _From, #state{url=URL, pool=Pool} = State) ->
  ?INFO("Getting Eth block from ~p to ~p~n", [From, To]),
  {ok, Resp} = get_eth_blocks(lists:seq(From, To), Pool, URL),
  {reply, {ok, Resp}, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% =============================================================================
% private functions
%% =============================================================================
-spec get_eth_blocks(Blocks::list(), Pool::binary(), URL::binary()) -> {ok, list()}.
get_eth_blocks(Blocks, Pool, URL) ->
  List = get_eth_blocks(Blocks, [], Pool, URL),
  {ok, List}.

get_eth_blocks([H|T], Accu, Pool, URL) ->
  Num = int_to_hex(H),
  Payload = [
    {id, util:uuid()},
    {jsonrpc, <<"2.0">>},
    {method, <<"eth_getBlockByNumber">>},
    {params, [Num, true]}
  ],
  {ok, Resp} = util:call(post, URL, Pool, Payload),
  {ok, Resp2} = sanitize_block(Resp),
  get_eth_blocks(T, [Resp2 | Accu], Pool, URL);

get_eth_blocks([], Accu, _Pool, _URL) -> lists:reverse(Accu).
  
-spec sanitize_block(map()) -> {ok, map()}.
sanitize_block(Resp) ->
  % sanitize some data for the block.
  Fun = 
    fun(K, V) ->
      case K of
        <<"difficulty">> -> hex_to_int(V);
        <<"gasLimit">> -> hex_to_int(V);
        <<"gasUsed">> -> hex_to_int(V);
        <<"nonce">> -> hex_to_int(V);
        <<"number">> -> hex_to_int(V);
        <<"size">> -> hex_to_int(V);
        <<"timestamp">> ->
          {ok, T} = tempo:format(iso8601, {unix, hex_to_int(V)}),
          T;
        <<"totalDifficulty">> -> hex_to_int(V);
        <<"transactions">> -> sanitize_transaction(V);
        _ -> V
      end
    end,
  Res = maps:map(Fun, Resp),
  % Res2 = maps:put(<<"id">>, maps:get(<<"hash">>, Res), Res),
  {ok, Res}.

-spec sanitize_transaction(Transactions::list()) -> list().
sanitize_transaction(Transactions) ->
  sanitize_transaction(Transactions, []).

sanitize_transaction([H|T], Accu) ->
  % ?INFO("T: ~p~n", [H]),
  TFun =
    fun(K, V) ->
      case K of 
        <<"blockNumber">> -> hex_to_int(V);
        <<"gas">> -> hex_to_int(V);
        <<"gasPrice">> -> hex_to_int(V);
        <<"nonce">> -> hex_to_int(V);
        <<"standardV">> -> hex_to_int(V);
        <<"transactionIndex">> -> hex_to_int(V);
        <<"value">> -> 
          Value = hex_to_int(V),
          to_eth_unit(Value);
        <<"r">> -> hex_to_int(V);
        <<"s">> -> hex_to_int(V);
        <<"v">> -> hex_to_int(V);
        _ -> V
      end
    end,
  sanitize_transaction(T, [maps:map(TFun, H) | Accu]);
sanitize_transaction([], Accu) -> lists:reverse(Accu).

get_env(Key, Def, Fun) when is_binary(Key) ->
  KeyVal =  
    case os:getenv(Key) of 
      false -> Def;
      Val -> erlang:apply(erlang, Fun, [Val])
    end,
  ?INFO("Getting env variable for ~p -> ~p~n", [Key, KeyVal]),
  KeyVal.

int_to_hex(Val) when is_integer(Val) ->
  Hex = list_to_binary(integer_to_list(Val, 16)),
  <<"0x", Hex/binary>>.

hex_to_int(Val) when is_binary(Val) ->
  <<"0x", Rest/binary>> = Val,
  list_to_integer(binary_to_list(Rest), 16).

to_eth_unit(Val) when is_integer(Val) ->
  to_eth_unit(Val, eth).

to_eth_unit(Val, Unit) when is_integer(Val), is_atom(Unit) ->
  case Unit of
    eth -> 
      binary_to_float(float_to_binary(Val/?ETH, [compact, {decimals, 15}]));
    shannon -> 
      binary_to_float(float_to_binary(Val/?SHANNON, [compact, {decimals, 15}]));
    finney -> 
      binary_to_float(float_to_binary(Val/?FINNEY, [compact, {decimals, 20}]))
  end.
