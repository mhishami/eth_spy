PROJECT = eth_spy
PROJECT_DESCRIPTION = Ethereum Spy
PROJECT_VERSION = 1.0.0

# Whitespace to be used when creating files from templates.
SP = 2

# DEPS
DEPS = lager cowboy eleveldb mnesia_eleveldb uuid sync hackney jsx ejwt tempo rocksdb
dep_mnesia_eleveldb = git https://github.com/klarna/mnesia_eleveldb.git master
dep_rocksdb = git https://gitlab.com/barrel-db/erlang-rocksdb.git master
dep_eleveldb_commit = 2.2
dep_cowboy_commit = 1.1.2

DEP_PLUGINS = cowboy
LOCAL_DEPS = mnesia crypto inets sasl

# unit tests
EUNIT_OPTS = verbose
EUNIT_ERL_OPTS = -args_file rel/vm.args -config rel/sys.config

include erlang.mk
