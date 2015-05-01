#!/bin/bash

source $(pwd)/configure.sh

echo "Copying backend application files to basho-patches"
cp $(pwd)/ebin/re_riak_patch* ${RIAK_LIB}/basho-patches/

echo "Restarting Riak"
${RIAK_BIN}/riak stop
${RIAK_BIN}/riak start
${RIAK_BIN}/riak-admin wait-for-service riak_kv

echo "Riak Ready"