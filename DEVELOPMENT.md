#Development

## Requirements

* [Install Erlang](http://docs.basho.com/riak/latest/ops/building/installing/erlang/)
* [Install Riak](http://docs.basho.com/riak/latest/ops/building/installing/)

## Developer Instructions
Riak Explorer uses Erlang on the server side (to serve the REST API and to talk
to Riak), and Ember.js on the client side. Ember itself uses Node.js for
[command-line tasks](http://www.ember-cli.com).

1. `make` - Loads and compiles all dependencies (depends on `erl`)

2. `make rel` - Performs release tasks, creates `rel/riak_explorer`

3. `make stage` - Enables faster development cycles; Only needs to be run once to set up lib and static web symlinks

4. Verify settings in `rel/riak_explorer/etc/riak_explorer.conf`

5. `./rel/riak_explorer/bin/riak_explorer start|console|attach|stop` - Controls the riak_explorer Erlang applicaiton

6. `make test` - Recompiles `src` and executes unit tests

7. `make itest` - Recompiles `src`, executes integration tests (run `./rel/riak_explorer/bin/riak_explorer start` first)

## Thank You!

Thank you for being part of the community! We love you for it.
