BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
BUILD_DIR ?= _build
OVERLAY_VARS    ?=

.PHONY: deps

all: compile
compile: deps compile-backend compile-frontend
deps: deps-backend deps-frontend
clean-package: 
	-rm -rf $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)
package: rel clean-package
	cd rel && tar -zcvf riak_explorer210.tar.gz riak_explorer
	mv rel/riak_explorer210.tar.gz $(BUILD_DIR)/
test: test-backend test-frontend
itest: itest-backend
rel: deps compile
	$(REBAR) compile
	$(REBAR) skip_deps=true generate $(OVERLAY_VARS)
relclean:
	rm -rf rel/riak_explorer
stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak_explorer/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/riak_explorer/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/riak_explorer/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/riak_explorer/lib;)
	-rm -rf rel/riak_explorer/priv/ember_riak_explorer/dist
	cd rel/riak_explorer/priv/ember_riak_explorer && ln -sf ../../../../priv/ember_riak_explorer/dist dist

# Backend
compile-backend: deps-backend
	$(REBAR) compile
recompile-backend:
	$(REBAR) compile skip_deps=true
deps-backend:
	$(REBAR) get-deps
cleantest-backend:
	rm -rf .eunit/*
test-backend: cleantest-backend
	$(REBAR)  skip_deps=true eunit
itest-backend: recompile-backend cleantest-backend
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit
reitest-backend: cleantest-backend
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit

# Frontend
compile-frontend: deps-frontend
	cd priv/ember_riak_explorer && ember build
recompile-frontend:
	cd priv/ember_riak_explorer && ember build
deps-frontend:
	cd priv/ember_riak_explorer && npm install && bower install
test-frontend:
	cd priv/ember_riak_explorer && ember test

# Deployment
deploy: package
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_explorer210.tar.gz s3://riak-tools/