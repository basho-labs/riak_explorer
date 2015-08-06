BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
BUILD_DIR ?= _build
OVERLAY_VARS    ?=

.PHONY: deps

all: compile
compile: deps compile-backend compile-frontend
recompile: recompile-backend recompile-frontend
deps: deps-backend deps-frontend
clean-package:
	-rm -rf $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)
package-mac: rel clean-package
	cd rel && tar -zcvf riak_explorer_darwin_amd64.tar.gz riak_explorer
	mv rel/riak_explorer_darwin_amd64.tar.gz $(BUILD_DIR)/
package-trusty64:
	cd vagrant/ubuntu/trusty64 && vagrant up
package-linux: rel clean-package
	cd rel && tar -zcvf riak_explorer_linux_amd64.tar.gz riak_explorer
	mv rel/riak_explorer_linux_amd64.tar.gz $(BUILD_DIR)/
test: test-backend test-frontend
itest: itest-backend
rel: relclean webrelclean deps compile
	$(REBAR) compile
	$(REBAR) skip_deps=true generate $(OVERLAY_VARS)
relclean:
	rm -rf rel/riak_explorer
webrelclean:
	rm -rf rel/riak_explorer/priv/ember_riak_explorer/dist
stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak_explorer/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/riak_explorer/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/riak_explorer/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/riak_explorer/lib;)
	rm -rf rel/riak_explorer/priv/ember_riak_explorer/dist && ln -sf $(abspath priv/ember_riak_explorer/dist) rel/riak_explorer/priv/ember_riak_explorer

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
rel-backend: relclean deps-backend compile-backend
	$(REBAR) compile
	$(REBAR) skip_deps=true generate $(OVERLAY_VARS)
	
# Frontend
compile-frontend: deps-frontend
	-cd priv/ember_riak_explorer && ember build
recompile-frontend:
	-cd priv/ember_riak_explorer && ember build
deps-frontend:
	-cd priv/ember_riak_explorer && npm install && bower install
test-frontend:
	cd priv/ember_riak_explorer && ember test

# Deployment
deploy-mac:
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_explorer_darwin_amd64.tar.gz s3://riak-tools/
deploy-linux:
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_explorer_linux_amd64.tar.gz s3://riak-tools/
deploy-linux-riak:
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_linux_amd64.tar.gz s3://riak-tools/
