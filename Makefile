REPO            ?= riak_explorer
PKG_VERSION	    ?= $(shell git describe --tags --abbrev=0 | tr - .)
ARCH            ?= amd64
OSNAME          ?= ubuntu
OSVERSION       ?= trusty
PKGNAME         ?= $(REPO)-$(PKG_VERSION)-$(OSNAME)-$(OSVERSION)-$(ARCH).tar.gz
OAUTH_TOKEN     ?= $(shell cat oauth.txt)
RELEASE_ID      ?= $(shell curl --silent https://api.github.com/repos/basho-labs/$(REPO)/releases/tags/$(PKG_VERSION) | python -c 'import sys, json; print json.load(sys.stdin)["id"]')
DEPLOY_BASE     ?= "https://uploads.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN)&name=$(PKGNAME)"
DOWNLOAD_BASE   ?= https://github.com/basho-labs/$(REPO)/releases/download/$(PKG_VERSION)/$(PKGNAME)

BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

ifneq (,$(shell whereis sha256sum | awk '{print $2}';))
SHASUM = sha256sum
else
SHASUM = shasum -a 256
endif

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
	cd packages && echo "$(DOWNLOAD_BASE)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt
sync-standalone:
	echo "Uploading to "$(DOWNLOAD_BASE)
	cd packages && \
		curl -XPOST -v -H 'Content-Type: application/gzip' $(DEPLOY_BASE) --data-binary @$(PKGNAME) && \
		curl -XPOST -v -H 'Content-Type: application/octet-stream' $(DEPLOY_BASE).sha --data-binary @$(PKGNAME).sha

RIAK_BASE             ?= root
PATCH_PKG_VERSION     ?= $(PKG_VERSION).patch
PATCH_PKGNAME         ?= $(REPO)-$(PATCH_PKG_VERSION)-$(OSNAME)-$(OSVERSION)-$(ARCH).tar.gz
PATCH_DEPLOY_BASE     ?= "https://uploads.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN)&name=$(PATCH_PKGNAME)"
PATCH_DOWNLOAD_BASE   ?= https://github.com/basho-labs/$(REPO)/releases/download/$(PKG_VERSION)/$(PATCH_PKGNAME)
tarball: compile
	echo "Creating packages/"$(PATCH_PKGNAME)
	-rm -rf rel/$(RIAK_BASE)
	mkdir -p rel/$(RIAK_BASE)/riak/lib/basho-patches
	mkdir -p rel/$(RIAK_BASE)/riak/priv
	cp -R deps/riakc/ebin/* rel/$(RIAK_BASE)/riak/lib/basho-patches/
	cp -R ebin/* rel/$(RIAK_BASE)/riak/lib/basho-patches/
	cp -R priv/* rel/$(RIAK_BASE)/riak/priv/
	mkdir -p packages
	tar -C rel -czf $(PATCH_PKGNAME) root
	mv $(PATCH_PKGNAME) packages/
	cd packages && $(SHASUM) $(PATCH_PKGNAME) > $(PATCH_PKGNAME).sha
	cd packages && echo "$(PATCH_DOWNLOAD_BASE)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PATCH_PKGNAME)" > local.txt
sync:
	echo "Uploading to "$(PATCH_DOWNLOAD_BASE)
	cd packages && \
		curl -XPOST -v -H 'Content-Type: application/gzip' $(PATCH_DEPLOY_BASE) --data-binary @$(PATCH_PKGNAME) && \
		curl -XPOST -v -H 'Content-Type: application/octet-stream' $(PATCH_DEPLOY_BASE).sha --data-binary @$(PATCH_PKGNAME).sha
