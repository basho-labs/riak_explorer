REBAR ?= $(shell pwd)/rebar
RIAK_NODE ?= riak@127.0.0.1
RIAK_COOKIE ?= riak

.PHONY: deps

# Common
all: deps-backend compile-backend
compile: deps-backend compile-backend compile-frontend
deps: deps-backend deps-frontend
clean: clean-backend
distclean: clean-backend distclean-backend
install: install-backend
start: start-backend start-frontend
package: package-backend package-frontend
test: test-backend test-frontend
itest: itest-backend itest-frontend

# Riak
stop-riak:
	-$(RIAK_BIN)/riak stop
start-riak:
	$(RIAK_BIN)/riak start
wait-for-riak:
	$(RIAK_BIN)/riak-admin wait-for-service riak_kv

# Backend
compile-backend: deps-backend
	$(REBAR) compile
recompile-backend:
	$(REBAR) compile skip_deps=true
deps-backend:
	$(REBAR) get-deps
clean-backend:
	$(REBAR) clean
	rm -rf build
distclean-backend: clean-backend
	$(REBAR) delete-deps
install-backend:
	cp dist/ebin/* $(RIAK_LIB)/basho-patches/
start-backend: stop-riak start-riak wait-for-riak
	$(RIAK_BIN)/riak escript $(shell pwd)/bin/riak_explorer -name $(RIAK_NODE) -setcookie $(RIAK_COOKIE) -start
stop-backend:
	$(RIAK_BIN)/riak escript $(shell pwd)/bin/riak_explorer -name $(RIAK_NODE) -setcookie $(RIAK_COOKIE) -stop
package-backend: compile-backend
	rm -rf dist/ebin/* && cp ebin/* dist/ebin/
cleantest-backend:
	rm -rf .eunit/*
install-test-backend:
	cp ebin/* $(RIAK_LIB)/basho-patches/
test-backend: cleantest-backend
	$(REBAR)  skip_deps=true eunit
itest-backend: recompile-backend cleantest-backend install-test-backend start-backend
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit
reitest-backend: cleantest-backend
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit

# Frontend
start-frontend:
	cd dist/web && python -m SimpleHTTPServer
package-frontend: compile-frontend
	rm -rf dist/web/* && cp -R priv/ember_riak_explorer/dist/* dist/web/
deps-frontend:
	cd priv/ember_riak_explorer && npm install && bower install
compile-frontend: deps-frontend
	cd priv/ember_riak_explorer && ember build
recompile-frontend:
	cd priv/ember_riak_explorer && ember build
test-frontend:
	cd priv/ember_riak_explorer && ember test
itest-frontend:
	cd priv/ember_riak_explorer && ember server