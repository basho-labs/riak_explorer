REPO            ?= riak_explorer
PKG_VERSION	    ?= $(shell git describe --tags --abbrev=0 | tr - .)
MAJOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f1)
MINOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f2)
ARCH            ?= amd64
OSNAME          ?= ubuntu
OSVERSION       ?= trusty
S3_BASE         ?= riak-tools
S3_PREFIX       ?= http://$(S3_BASE).s3.amazonaws.com/
DEPLOY_BASE     ?= $(REPO)/$(MAJOR).$(MINOR)/$(PKG_VERSION)/$(OSNAME)/$(OSVERSION)/
PKGNAME         ?= $(REPO)-$(PKG_VERSION)-$(ARCH).tar.gz

BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

.PHONY: deps

all: compile

compile: deps
	$(REBAR) compile
recompile:
	$(REBAR) compile skip_deps=true
deps:
	$(REBAR) get-deps
clean: cleantest relclean
	-rm -rf packages
cleantest:
	rm -rf .eunit/*
test: cleantest
	$(REBAR)  skip_deps=true eunit
itest: recompile cleantest
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit
reitest: cleantest
	INTEGRATION_TEST=true $(REBAR) skip_deps=true eunit
relclean:
	-rm -rf rel/riak_explorer
	-rm -rf rel/root
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

##
## Packaging targets
##
tarball-standalone: rel
	echo "Creating packages/"$(PKGNAME)
	mkdir -p packages
	tar -C rel -czf $(PKGNAME) $(REPO)/
	mv $(PKGNAME) packages/
	cd packages && shasum -a 256 $(PKGNAME) > $(PKGNAME).sha
	cd packages && echo "$(S3_PREFIX)$(DEPLOY_BASE)$(PKGNAME)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt
sync-standalone:
	echo "Uploading to "$(DEPLOY_BASE)
	cd packages && \
		s3cmd put --acl-public $(PKGNAME) s3://$(S3_BASE)/$(DEPLOY_BASE) && \
		s3cmd put --acl-public $(PKGNAME).sha s3://$(S3_BASE)/$(DEPLOY_BASE)

RIAK_BASE     ?= root
tarball: compile
	echo "Creating packages/"$(PKGNAME)
	-rm -rf rel/$(RIAK_BASE)
	mkdir -p rel/$(RIAK_BASE)/riak/lib/basho-patches
	mkdir -p rel/$(RIAK_BASE)/riak/priv
	cp -R deps/riakc/ebin/* rel/$(RIAK_BASE)/riak/lib/basho-patches/
	cp -R ebin/* rel/$(RIAK_BASE)/riak/lib/basho-patches/
	cp -R priv/* rel/$(RIAK_BASE)/riak/priv/
	mkdir -p packages
	tar -C rel -czf $(PKGNAME) root
	mv $(PKGNAME) packages/
	cd packages && shasum -a 256 $(PKGNAME) > $(PKGNAME).sha
	cd packages && echo "$(S3_PREFIX)$(DEPLOY_BASE)$(PKGNAME)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt
sync:
	echo "Uploading to "$(DEPLOY_BASE)
	cd packages && \
		s3cmd put --acl-public $(PKGNAME) s3://$(S3_BASE)/$(DEPLOY_BASE) && \
		s3cmd put --acl-public $(PKGNAME).sha s3://$(S3_BASE)/$(DEPLOY_BASE)
