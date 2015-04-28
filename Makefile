REBAR ?= $(shell pwd)/rebar

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