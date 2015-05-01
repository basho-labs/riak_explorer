#!/bin/bash
cd `dirname $0`
source $(pwd)/configure.sh
ERTS=`$RIAK_BIN/riak ertspath`
exec ${ERTS}/erl -pa $(pwd)/ebin $(pwd)/deps/*/ebin -boot start_sasl -s reloader -s riak_explorer -name ${EXPLORER_NODE} -setcookie ${RIAK_COOKIE} -config $(pwd)/etc/app.config