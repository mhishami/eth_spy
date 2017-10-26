-module (db_parser).

-define (DB_PATH, "~/Library/Application Support/io.parity.ethereum/db/chains").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% Insert tests here.
-define (DB_TEST, "/tmp/db.test").

open_test_() ->
  Options = [{create_if_missing, true}],
  {ok, DB} = rocksdb:open(?DB_TEST, Options),
  {Res, _Info} = file:read_file_info(?DB_TEST),
  [
    ?_assert(ok =:= Res),
    ?_assert(ok =:= rocksdb:close(DB))
  ].

-endif.
