REBAR ?= $(shell pwd)/rebar
RIAK_NODE ?= riak@127.0.0.1
RIAK_COOKIE ?= riak
BUILD_BASE ?= _build
BUILD_DIR ?= $(BUILD_BASE)/riak_explorer

.PHONY: deps

all: compile
compile: deps compile-backend compile-frontend
deps: deps-backend deps-frontend
clean-package: 
	rm -rf $(BUILD_BASE)
	mkdir -p $(BUILD_DIR)
package: clean-package package-backend package-frontend
	cp start.sh $(BUILD_DIR)/
	cp patch.sh $(BUILD_DIR)/
	cp configure.example.sh $(BUILD_DIR)/configure.sh
	cd $(BUILD_BASE)/ && tar -zcvf riak_explorer210.tar.gz riak_explorer
test: test-backend test-frontend
itest: itest-backend

# Backend
compile-backend: deps-backend
	$(REBAR) compile
recompile-backend:
	$(REBAR) compile skip_deps=true
deps-backend:
	$(REBAR) get-deps
package-backend: compile-backend
	cp -R ebin $(BUILD_DIR)/
	cp -R etc $(BUILD_DIR)/
	mkdir -p $(BUILD_DIR)/deps/all
	cp -R deps/*/ebin $(BUILD_DIR)/deps/all/
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
deps-frontend:
	cd priv/ember_riak_explorer && npm install && bower install
package-frontend: compile-frontend
	mkdir -p $(BUILD_DIR)/priv/ember_riak_explorer/dist
	cp -R priv/ember_riak_explorer/dist/* $(BUILD_DIR)/priv/ember_riak_explorer/dist/
test-frontend:
	cd priv/ember_riak_explorer && ember test

# Deployment
deploy: package
	#cd $(BUILD_BASE)/ && s3cmd put --acl-public riak_explorer210.tar.gz s3://riak-tools/