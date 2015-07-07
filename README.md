# riak_explorer

Riak dev-mode and admin GUI.

*Front-end GUI:* Ember.js

*Back-end:* Erlang + WebMachine serving a REST API.

See also: [Riak Control Design Discussion
 doc](https://docs.google.com/document/d/1qcHyyEEL1jCAKrjNtmbIEcAFS3VAdLyoRK88FDy6o_0/edit#).

## Mac OS X Installation

1. Download and extract [http://riak-tools.s3.amazonaws.com/riak_explorer_darwin_amd64.tar.gz](http://riak-tools.s3.amazonaws.com/riak_explorer_darwin_amd64.tar.gz)

2. Verify settings in `etc/riak_explorer.conf`

4. `./bin/riak_explorer start|console|attach|stop` - Controls the riak_explorer Erlang application

4. Navigate to [http://localhost:9000/](http://localhost:9000/) to test

## Usage

#### API

In addition to the web interface, there is also an API exposed at [http://localhost:9000/explore](http://localhost:9000/explore). Following are the available routes (these can also be obtained from `/explore/routes`):

```
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket/keys/$key
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket/keys/$key/$resource
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket/keys
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket/keys/$key
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket/keys/$key/$resource
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket/keys
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket/$resource (Resources: [jobs])
/explore/nodes/$node/bucket_types/$bucket_type/buckets
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket/$resource (Resources: [jobs])
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets
/explore/nodes/$node/bucket_types/$bucket_type
/explore/nodes/$node/bucket_types/$bucket_type/$resource (Resources: [jobs])
/explore/nodes/$node/bucket_types
/explore/clusters/$cluster/bucket_types/$bucket_type
/explore/clusters/$cluster/bucket_types/$bucket_type/$resource (Resources: [jobs])
/explore/clusters/$cluster/bucket_types
/explore/nodes/$node
/explore/clusters/$cluster/nodes/$node
/explore/clusters/$cluster/nodes/$node/$resource
/explore/clusters/$cluster/nodes
/explore/clusters/$cluster
/explore/clusters/$cluster/$resource
/explore/clusters
/explore
/explore/$resource (Resources: [routes,props,jobs,ping])
/control/clusters/$cluster/ringready
/control/clusters/$cluster/status
/control/clusters/$cluster/clear
/control/clusters/$cluster/commit
/control/clusters/$cluster/plan
/control/clusters/$cluster/force-replace/$node1/$node2
/control/clusters/$cluster/replace/$node1/$node2
/control/clusters/$cluster/force-remove/$node1
/control/clusters/$cluster/leave/$node1
/control/clusters/$cluster/leave
/control/clusters/$cluster/join/$node1
/control/nodes/$node/ringready
/control/nodes/$node/status
/control/nodes/$node/clear
/control/nodes/$node/commit
/control/nodes/$node/plan
/control/nodes/$node/force-replace/$node1/$node2
/control/nodes/$node/replace/$node1/$node2
/control/nodes/$node/force-remove/$node1
/control/nodes/$node/leave/$node1
/control/nodes/$node/leave
/control/nodes/$node/join/$node1
/riak/nodes/$node/$* (Riak Direct HTTP Proxy)
/riak/clusters/$cluster/$* (Riak Direct HTTP Proxy)
/$* (Static Endpoint)
```

Explanation:

* `explore.handler`: This is the Erlang module responsible for serving routes.
* `explore.routes`: List of existing routes. URI sections beginning with $ are variables that need to be specified
    * `$cluster`: Specifying `default` will use the cluster that this riak_explorer is connected to.
    * `$node`: Example: `riak@127.0.0.1`
    * `$bucket_type`: Example: `default`
    * `$bucket`: Example: `mybucket`
    * `$key`: Example: `mykey`
    * `$schema`: Example: `_yz_default`
    * `$index`: Example: `myindex`
    * `$*`: Wildcard with deep paths. Example: `assets/ember-riak-explorer.js` for the static route, or `ping` for the riak_proxy route
    * `$resource`: A list of valid `resources` for a given module can be found in `explore.resources`
* `explore.resources`: A list of available operations or resources specific to the route; Example: `ping` for the `/explore` route.

## Developer Instructions
Riak Explorer uses Erlang on the server side (to serve the REST API and to talk
to Riak), and Ember.js on the client side. Ember itself uses Node.js for
[command-line tasks](http://www.ember-cli.com).

#### Full Stack

1. `make` - Loads and compiles all dependencies (depends on `erl`, `npm`, `ember-cli`, and `bower`)

2. `make rel` - Performs release tasks, creates `rel/riak_explorer`

3. `make stage` - Enables faster development cycles; Only needs to be run once to set up lib and static web symlinks

4. Verify settings in `rel/riak_explorer/etc/riak_explorer.conf`

5. `./rel/riak_explorer/bin/riak_explorer start|console|attach|stop` - Controls the riak_explorer Erlang applicaiton

#### Erlang

1. `make compile-backend` | `make recompile-backend` - Loads and compiles the webmachine app dependencies

2. `make test-backend` - Recompiles `src` and executes unit tests

3. `make itest-backend` - Recompiles `src`, executes integration tests (run `./rel/riak_explorer/bin/riak_explorer start` first)

#####Environment

* [Install Erlang](http://docs.basho.com/riak/latest/ops/building/installing/erlang/)
* [Install Riak](http://docs.basho.com/riak/latest/ops/building/installing/)

#### Ember.js
The Ember app lives in `priv/ember_riak_explorer`, and follows the standard
[ember-cli folder layout conventions](http://www.ember-cli.com/#folder-layout).

1. `make compile-frontend` | `make recompile-backend` - Loads and compiles the Ember.js app dependencies (depends on `npm`, `ember-cli`, and `bower`)

2. `make test-frontend` - Runs `ember test`

#####Environment

* (Optional) Install [nvm](https://github.com/creationix/nvm), the Node.js Version Manager.
    The provided [install script](https://github.com/creationix/nvm#install-script)
    is easiest:

    ```
    curl https://raw.githubusercontent.com/creationix/nvm/v0.25.0/install.sh | bash
    ```

    (After installing via script above, either reopen your terminal or
    `source ~/.bashrc`).

    Using `nvm` is optional - you can install [Node.js](https://nodejs.org/)
    directly from the website. However, much like with Ruby and `rvm`, it's
    easier to upgrade and manage Node.js versions using this tool.

    This enables you to:

    * `nvm ls-remote` - View what Node.js versions are available to install
    * `nvm install stable` - Install latest stable version
        of Node.js (`v0.12.2` at the moment)
    * `nvm alias default stable` - Tells `nvm` to use the stable version by
        default (for all new terminal sessions)

* Use `npm` (Node.js Package Manager) to install the `ember-cli` package.
    (If you did `nvm install stable` above, you now have `npm` installed.)

    ```
    npm install -g ember-cli
    ```

    (The `-g` flag means "install it globally")

* Install the `phantomjs` package, for headless browser unit testing

    ```
    npm install -g phantomjs
    ```

#### Related Projects
- [riak_control](https://github.com/basho/riak_control)
- [rekon](https://github.com/basho/rekon) (old bucket / object explorer gui)
- [riak_cs_control](https://github.com/basho/riak_cs_control)
- [RiakCS.net source code](https://github.com/basho/riak_cs_test_harness)
    (RiakCS.net itself is now defunct, but had nice CS-related features).
