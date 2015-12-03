BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
BUILD_DIR ?= _build
OVERLAY_VARS    ?=

.PHONY: deps

all: compile

compile: deps
	$(REBAR) compile
recompile:
	$(REBAR) compile skip_deps=true
deps:
	$(REBAR) get-deps
cleantest:
	rm -rf .eunit/*
test: cleantest
	$(REBAR)  skip_deps=true eunit
itest: recompile cleantest
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit
reitest: cleantest
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit
relclean:
	rm -rf rel/riak_explorer
rel: relclean deps compile
	$(REBAR) compile
	$(REBAR) skip_deps=true generate $(OVERLAY_VARS)
stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak_explorer/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/riak_explorer/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/riak_explorer/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/riak_explorer/lib;)
	rm -rf rel/riak_explorer/priv/ember_riak_explorer/dist && ln -sf $(abspath priv/ember_riak_explorer/dist) rel/riak_explorer/priv/ember_riak_explorer

riak-addon:
	-rm -rf rel/riak-addon
	mkdir -p rel/riak-addon/ebin
	mkdir -p rel/riak-addon/priv
	$(REBAR) compile
	cp -R deps/riakc/ebin/* rel/riak-addon/ebin/
	cp -R ebin/* rel/riak-addon/ebin/
	cp -R priv/* rel/riak-addon/priv/

# Build steps for posterity
## From riak_explorer
# git checkout master && git pull && git checkout riak-addon-master && git pull origin master
## From riak-explorer-gui
# git checkout master && git pull && git checkout riak-addon-master && git pull origin master && make && rm -rf ../riak_explorer/priv/ember_riak_explorer/dist/* && cp -R dist/* ../riak_explorer/priv/ember_riak_explorer/dist/
## From riak_explorer
# make riak-addon && git checkout master
## From riak-explorer-gui
# git checkout master && make && rm -rf ../riak_explorer/priv/ember_riak_explorer/dist/* && cp -R dist/* ../riak_explorer/priv/ember_riak_explorer/dist/
## From riak_explorer
# make rel

## Package steps for posterity
## From riak_explorer
# make package-mac
# make deploy-mac

# Deployment
deploy-mac:
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_explorer_darwin_amd64.tar.gz s3://riak-tools/
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_explorer_addon_darwin_amd64.tar.gz s3://riak-tools/
deploy-linux:
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_explorer_linux_amd64.tar.gz s3://riak-tools/
	cd $(BUILD_DIR) && s3cmd put --acl-public riak_explorer_addon_linux_amd64.tar.gz s3://riak-tools/

# Packaging
clean-package:
	-rm -rf $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)
package-mac: clean-package
	cd rel && tar -zcvf riak_explorer_darwin_amd64.tar.gz riak_explorer
	mv rel/riak_explorer_darwin_amd64.tar.gz $(BUILD_DIR)/
	cd rel && riak_explorer_addon_darwin_amd64.tar.gz riak-addon
	mv rel/riak_explorer_addon_darwin_amd64.tar.gz $(BUILD_DIR)/
package-trusty64:
	cd vagrant/ubuntu/trusty64 && vagrant up
package-linux: clean-package
	cd rel && tar -zcvf riak_explorer_linux_amd64.tar.gz riak_explorer
	mv rel/riak_explorer_linux_amd64.tar.gz $(BUILD_DIR)/
	cd rel && riak_explorer_addon_linux_amd64.tar.gz riak-addon
	mv rel/riak_explorer_addon_linux_amd64.tar.gz $(BUILD_DIR)/
