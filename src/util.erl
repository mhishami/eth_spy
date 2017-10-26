-module (util).
-include ("eth_spy.hrl").

-export ([uuid/0]).
-export ([hash/1]).
-export ([bin_to_hex/1]).
-export ([word_to_hex/1]).
-export ([bin_to_string/1]).
-export ([to_float/1]).
-export ([to_bin/1]).
-export ([call/4]).
-export ([is_email/1]).
-export ([mnemonics/0]).
-export ([int_to_hex/1]).
-export ([hex_to_int/1]).

-spec uuid() -> binary().
uuid() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

-spec hash(Data::binary()) -> binary().
hash(Data) ->
  Bin = crypto:hash(sha256, crypto:hash(sha256, Data)),
  base58:encode(Bin).

-spec bin_to_hex(Binary::binary()) -> string().
bin_to_hex(Binary) when is_binary(Binary) ->
  lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= Binary]).

% -spec int_to_hex(Int::non_neg_integer()) -> string().
% int_to_hex(Int) when is_integer(Int), Int >=0, Int =< 16#ffffffff ->
%   lists:flatten([io_lib:format("0x~8.16.0b",[Int])]).

-spec word_to_hex(Int::non_neg_integer()) -> string().
word_to_hex(Int) when is_integer(Int), Int >=0, Int =< 16#ffff ->
  lists:flatten([io_lib:format("0x~4.16.0b",[Int])]).

-spec bin_to_string(Binary::binary()) -> string().
bin_to_string(Binary) when is_binary(Binary) ->
  list_to_binary(bin_to_hex(Binary)).

-spec to_float(Val :: binary() | float() | integer()) -> float().
to_float(Val) when is_integer(Val) ->
  Tmp = integer_to_binary(Val),
  to_float(<<Tmp/binary, ".0">>);
to_float(Val) when is_binary(Val) ->
  try binary_to_float(Val) of 
    Res -> Res
  catch
    _:_ ->
      V = <<Val/binary, ".0">>,
      binary_to_float(V)
  end;
to_float(Val) when is_float(Val) ->
  Val.

-spec to_bin(Val :: binary() | float() | integer()) -> binary().
to_bin(Val) when is_float(Val) -> 
  float_to_binary(Val, [{decimals, 8}]);
to_bin(Val) when is_integer(Val) ->
  B = integer_to_binary(Val),
  <<B/binary, ".0">>;
to_bin(Val) when is_binary(Val) -> Val.
-spec call(Method :: binary(), 
           Endpoint :: binary(), 
           Pool :: atom(), 
           Payload :: iodata()) 
    -> {ok, iodata()} 
    |  {error, iodata()}.  
    
call(Method, Endpoint, Pool, Payload) ->
  % ?DEBUG("Executing '~p' with payload ~p~n", [Method, jsx:encode(Payload)]),
  ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
  Options = [{pool, Pool}],

  try hackney:request(Method, Endpoint, ReqHeaders, jsx:encode(Payload), Options) of
    {ok, _, _, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, Resp} ->
          Result = jsx:decode(Resp, [return_maps]),
          try maps:get(<<"result">>, Result) of 
            Res1 -> {ok, Res1}
          catch
            _:_ ->
              {error, maps:get(<<"error">>, Result)}
          end;
          % {ok, maps:get(<<"result">>, Result)};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  catch
    Error ->
      {error, Error}
  end.

%% @doc Check if the argument is an e-mail address (without any extras like name, comments, etc.)
-spec is_email(iodata()) -> boolean().
is_email(Email) ->
    case re:run(Email, [$^|re()]++"$", [extended]) of
        nomatch   -> false;
        {match,_} -> true
    end.

mnemonics() ->
  crypto:rand_seed(crypto:strong_rand_bytes(32)),
  ok.

int_to_hex(Val) when is_integer(Val) ->
  Hex = list_to_binary(integer_to_list(Val, 16)),
  <<"0x", Hex/binary>>.

hex_to_int(Val) when is_binary(Val) ->
  <<"0x", Rest/binary>> = Val,
  list_to_integer(binary_to_list(Rest), 16).  

%% @doc Regular expression to match e-mail addresses.
re() ->
    "(
            (\"[^\"\\f\\n\\r\\t\\v\\b]+\")
        |   ([\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+
                (\\.[\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+)*
            )
    )
    @
    (
        (
            ([A-Za-z0-9\\-])+\\.
        )+
        [A-Za-z\\-]{2,}
    )".
