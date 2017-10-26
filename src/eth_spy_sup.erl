-module(eth_spy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(WORKER(N), {N, {N, start_link, []}, permanent, 5000, worker, [N]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  EthSvr = ?WORKER(eth_svr),

  Procs = [EthSvr],
  {ok, {{one_for_one, 1, 5}, Procs}}.
