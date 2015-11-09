#Development

The Riak Explorer project consists of two sub-projects:
the Erlang API (this repo), and the front end JS
[riak-explorer-gui](https://github.com/basho-labs/riak-explorer-gui).
The GUI is optional; you can run the API as a standalone add-on to Riak.

## Requirements - Explorer API

* [Install Erlang](http://docs.basho.com/riak/latest/ops/building/installing/erlang/)
* [Install Riak](http://docs.basho.com/riak/latest/ops/building/installing/)
    (from pre-built package, or from source)
* Clone `riak_explorer` (this repo) and `cd` into it.

## Developer Instructions - Explorer API
These instructions assume that you have Erlang installed, and Riak installed
and started.

It's also helpful to locate Riak's `basho-patches` directory. If your Riak
is installed locally from source (or from the Mac OS X app), it will be located
at `<path to Riak install>/lib/basho-patches`. If you installed it on a server
OS from package, see the [basho-patches section](http://docs.basho.com/riak/latest/ops/upgrading/rolling-upgrades/#Basho-Patches)
of the online docs. This directory allows hot-loading of custom Erlang modules
into Riak, and that's exactly the mechanism the Explorer API will be using.

#### Compile the API code

1. `make` - Loads and compiles all dependencies (depends on `erl`)

2. `make rel` - Performs release tasks, creates `rel/riak_explorer`

3. `make stage` - Enables faster development cycles; Only needs to be run once to set up lib and static web symlinks

#### Configure and Test the connection to Riak

Riak Control only interacts with the clusters specified in its config file
(`riak_explorer.conf`).

By default, it's going to try and connect to a `default` cluster, expecting your
local Riak node to have the id `riak@127.0.0.1`. If your Riak's node id is
different (check Riak's `riak.conf`  file), you will need the change Explorer's
config to point it in the right direction. (For advanced Riak users: also
double-check that Riak's Erlang cookie matches Explorer's cookie.)

4. Verify settings in `rel/riak_explorer/etc/riak_explorer.conf`

5. `./rel/riak_explorer/bin/riak_explorer start` (or `console|attach|stop`) -
    Starts the `riak_explorer` Erlang API, as well as the Webmachine web server
    that will be serving the Ember.js GUI's HTTP and AJAX requests.

6. `curl localhost:9000/explorer/ping` - Test that Explorer is up and running.
    By default, Explorer listens on port `9000` (you can change this in
    `riak_explorer.conf`).

7. `curl localhost:9000/explorer/clusters/default/nodes` - Test to make sure
    Explorer can connect to the default cluster. You should see a response like
    `{"nodes":[{"id":"riak@127.0.0.1"}], ...` If the list of nodes comes back
    empty, make sure Riak is started, and the node id and Erlang cookie are
    correct.

#### Run the Tests

8. `make test` - Recompiles `src` and executes unit tests

9. `make itest` - Recompiles `src`, executes integration tests (run `./rel/riak_explorer/bin/riak_explorer start` first)

## Requirements - Explorer GUI (optional)

For instructions on how to install the front-end GUI (and its dependencies),
see the README at
[riak-explorer-gui](https://github.com/basho-labs/riak-explorer-gui).

## Thank You!

Thank you for being part of the community! We love you for it.
