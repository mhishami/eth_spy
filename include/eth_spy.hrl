
-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
-define(tuplelist_to_record(Rec, Ref), list_to_tuple([Rec|[V || {_,V} <- Ref]])).

-record (xw_user, {uuid, username, email, mobile_no, password, created_at, updated_at}).
-record (xw_wallet, {uuid, type, username, address, pin, mnemonics, created_at, updated_at}).

-define(DEBUG(Text), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(DEBUG(Text, Args), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(INFO(Text), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(INFO(Text, Args), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(WARN(Text), lager:log(warning, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(WARN(Text, Args), lager:log(warning, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(ERROR(Text), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE])).
-define(ERROR(Text, Args), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).