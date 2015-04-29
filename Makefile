REBAR ?= $(shell pwd)/rebar
RIAK_NODE ?= riak@127.0.0.1
RIAK_COOKIE ?= riak

.PHONY: deps

all: deps compile

compile: deps
	$(REBAR) compile

compile-riak-test: compile
	$(REBAR) skip_deps=true

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean
	rm -rf build

distclean: clean
	$(REBAR) delete-deps

package: compile
	rm -rf dist/web/* && rm -rf dist/ebin/* && cp ebin/* dist/ebin/ &&  cp -R priv/ember_riak_explorer/dist/* dist/web/

install:
	cp dist/ebin/* $(RIAK_LIB)/basho-patches/

stop-riak:
	$(RIAK_BIN)/riak stop

start-riak:
	$(RIAK_BIN)/riak start

start-re-backend:
	$(RIAK_BIN)/riak escript $(shell pwd)/bin/riak_explorer -name $(RIAK_NODE) -setcookie $(RIAK_COOKIE) -start

stop-re-backend:
	$(RIAK_BIN)/riak escript $(shell pwd)/bin/riak_explorer -name $(RIAK_NODE) -setcookie $(RIAK_COOKIE) -stop

start-re-frontend:
	cd dist/web && python -m SimpleHTTPServer

start: start-re-backend start-re-frontend