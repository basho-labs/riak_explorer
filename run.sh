#!/bin/bash

source $(pwd)/configure.sh

echo "Copying backend application files to basho-patches"
cp $(pwd)/ebin/* $(RIAK_LIB)/basho-patches/

echo "Restarting Riak"
$(RIAK_BIN)/riak stop
$(RIAK_BIN)/riak start
$(RIAK_BIN)/riak-admin wait-for-service riak_kv

echo "Starting backend application"
$(RIAK_BIN)/riak escript $(pwd)/bin/riak_explorer -name $(RIAK_NODE) -setcookie $(RIAK_COOKIE) -start

echo "Starting frontend webserver"
cd web && python -m SimpleHTTPServer