# Riak Explorer

Riak Explorer provides browsing and admin capabilities for [Riak
KV](http://basho.com/products/riak-kv/), a distributed NoSQL key-value data
store that offers high availability, fault tolerance, operational simplicity,
and scalability.

Riak Explorer is useful while in a development or production. It includes
convenient methods to browse Bucket Types, Buckets, Keys, view and edit Riak
Objects, and more. To prevent heavy I/O requests from key listings, be sure to
edit the config file to reflect the environment as [explained in Using Riak
Explorer](#using-riak-explorer).

* [Installation](#installation)
* [System Architecture](#architecture)
* [Using Riak Explorer](#using-riak-explorer)
* [Development / Contributing](#development--contributing)

## Installation

### Installing from pre-built Package

The easiest way to install Riak Explorer (for non-developers) is to use one of
the pre-compiled packages below. These include both the Erlang backend API code
(this repository), and the front-end Ember.js GUI code (from
[riak-explorer-gui](https://github.com/basho-labs/riak-explorer-gui)).

#### Standalone Version

1. Download and extract [riak_explorer_darwin_amd64.tar.gz](http://riak-tools.s3.amazonaws.com/riak_explorer_darwin_amd64.tar.gz) for Mac OSX or [riak_explorer_linux_amd64.tar.gz](http://riak-tools.s3.amazonaws.com/riak_explorer_linux_amd64.tar.gz) for Ubuntu 14.04.
     * *Note: If you'd like to support further OSes, please [open an Issue](https://github.com/basho-labs/riak_explorer/issues)*

2. Verify the default settings in `riak_explorer/etc/riak_explorer.conf`
    will work for your configuration (primarily that port 9000 is available on your
    host). Pay special attention to development mode settings, this should be `off`
    for use with a production environment to prevent accidental keylistings.

3. Run `./riak_explorer/bin/riak_explorer start` to start the `riak_explorer`
    application

4. Navigate to [http://localhost:9000/](http://localhost:9000/) to see the
    interface

#### Riak Patch Version

1. Download and extract [riak_explorer_addon_darwin_amd64.tar.gz](http://riak-tools.s3.amazonaws.com/riak_explorer_addon_darwin_amd64.tar.gz) for Mac OSX or [riak_explorer_addon_linux_amd64.tar.gz](http://riak-tools.s3.amazonaws.com/riak_explorer_addon_linux_amd64.tar.gz) for Ubuntu 14.04.

2. Locate your Riak installation and `cp -R riak-addon/priv /path/to/riak/priv`, `cp -R riak-addon/ebin/* /path/to/riak/lib/basho-patches/`

3. Run `riak/bin/riak start`

4. Navigate to [http://localhost:8098/admin](http://localhost:8098/admin) to see the interface

### Installing the Dev Environment

For developer install instructions (and contribution guidelines), see the
[Development / Contributing](#development--contributing) section, below.

## System Architecture

*Front-end GUI:* [Ember.js](http://emberjs.com/).
    See [riak-explorer-gui](https://github.com/basho-labs/riak-explorer-gui)
    repository for the front-end code and setup instructions.

*Back-end:* [Erlang](http://www.erlang.org/) is the primary development language
    and [WebMachine](http://webmachine.github.io/) is used to serve a RESTful
    API (and, optionally, to serve the Ember.js GUI app).

### Development Mode

The concept of "Development Mode" is crucial to Riak Explorer.

Because Explorer allows users to perform operations that are disastrous in
production (such as List Keys or List Buckets operations), the app is careful
to enable those operations *only in Development Mode*. This setting is
toggled in the Explorer config file, on a per-cluster basis.

If you take a look in `rel/riak_explorer/etc/riak_explorer.conf`, you will see
a line like:

```
clusters.default.development_mode = on
```

This means that the `default` cluster has Dev Mode *enabled*, and *WILL* allow
prohibitive operations such as Streaming List Keys. Operators are strongly
encouraged to either:

a. Not point Explorer at production clusters, or

b. If used with production clusters, be sure to set `development_mode = off` for
    that cluster.

### Key and Bucket List Caches (in Dev Mode only)

Even in Dev Mode, Explorer tries not to run Key and Bucket listing operations
more than necessary. To that end, the API runs the List command once (per bucket
type, and per bucket, the first time it encounters them) and then *caches* the
result in a text file, on disk. The GUI app user, when browsing buckets, only
interacts with those caches.

The interaction with the API goes like this (omitting the `localhost:9000` part
from the URLs, for brevity):

```bash
# The first time you try to list buckets in the 'default' bucket type
# (cache is empty)
curl /explore/clusters/default/bucket_types/default/buckets
# -> HTTP 404 Not Found

# The app then *refreshes* the cache from a Streaming List Buckets operation
# by issuing an HTTP POST request to the `refresh_buckets` endpoint
# As expected, this operation may take some time, depending on how much data
# is in a cluster.
curl -XPOST \
  /explore/clusters/default/bucket_types/default/refresh_buckets/source/riak_kv
# -> HTTP 202 Accepted

# While the cache refresh runs, apps can poll the `jobs` endpoint to see when
# it's completed
curl /explore/clusters/default/bucket_types/default/jobs

# The app can then re-try the bucket list
curl /explore/clusters/default/bucket_types/default/buckets
# -> HTTP 200 OK
# ['bucket1', 'bucket2', etc...]

# To Refresh the cache, POST to the appropriate refresh endpoint as above
```

### Explorer API endpoints

Take a look at the listing of Explorer API resources below. The three types of
API endpoints available are:

1. The **Riak proxy** endpoints, `/riak/nodes/` and `/riak/clusters/`. The app
    uses these endpoints to make calls to the plain [Riak HTTP
    API](http://docs.basho.com/riak/latest/dev/references/http/). The proxy
    endpoints are used for several reasons, primarily due to CORS issues
    (on the Riak API side).

    So, for example, `curl localhost:9000/riak/nodes/riak@127.0.0.1/ping`
    proxies the request to that specific node's [ping HTTP API](http://docs.basho.com/riak/latest/dev/references/http/ping/).

    Similarly, using `curl localhost:9000/riak/clusters/default/ping`
    proxies the request to the *cluster* (which also ends up going to that same
    node, since this cluster just has one node in it).

    In general, it is preferable to use the `clusters/` proxy endpoint (unless
    you specifically want to access an individual node's REST API).

2. **Explore** endpoints, at `/explore/`. Think of it as an enhancement to
    Riak's own HTTP API, to fill in missing functionality. For example,
    the plain Riak API doesn't have a 'list bucket types' functionality --
    that can only be done via `riak-admin` CLI. The Explorer endpoints enable
    this, at `/explore/clusters/$cluster/bucket_types`.

3. **Control** endpoints at `/control/`. These provide a REST API to cluster
    operations that are normally available only through the [Riak Admin
    CLI](http://docs.basho.com/riak/latest/ops/running/tools/riak-admin/)
    (for example, `riak-admin cluster join`).

### API Documentation

You can generate Explorer API reference documentation using
[aglio](https://github.com/danielgtaylor/aglio):

```
# install the Aglio renderer for the API Blueprint markdown format
npm install -g aglio

# generate the documentation
aglio -i API.apib.md -o docs/api.html

# open them in your browser
open docs/api.html
```

#### Full API Listing

Riak Explorer exposes a REST API (by default located at [http://localhost:9000/explore](http://localhost:9000/explore)).

Following are the available routes (these can also be obtained from `/explore/routes`):

```
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket/keys
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket/refresh_keys/source/riak_kv
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket/keys
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket/refresh_keys/source/riak_kv
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket/$resource (Resources: [jobs])
/explore/nodes/$node/bucket_types/$bucket_type/buckets/$bucket
/explore/nodes/$node/bucket_types/$bucket_type/buckets
/explore/nodes/$node/bucket_types/$bucket_type/refresh_buckets/source/riak_kv
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket/$resource (Resources: [jobs])
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets/$bucket
/explore/clusters/$cluster/bucket_types/$bucket_type/buckets
/explore/clusters/$cluster/bucket_types/$bucket_type/refresh_buckets/source/riak_kv
/explore/nodes/$node/bucket_types/$bucket_type/$resource (Resources: [jobs])
/explore/nodes/$node/bucket_types/$bucket_type
/explore/nodes/$node/bucket_types
/explore/clusters/$cluster/bucket_types/$bucket_type/$resource (Resources: [jobs])
/explore/clusters/$cluster/bucket_types/$bucket_type
/explore/clusters/$cluster/bucket_types
/explore/nodes/$node
/explore/nodes/$node/$resource (Resources: [config]
/explore/clusters/$cluster/nodes/$node
/explore/clusters/$cluster/nodes/$node/$resource (Resources: [config])
/explore/clusters/$cluster/nodes
/explore/nodes/$node/config/files/$file
/explore/nodes/$node/config/files
/explore/clusters/$cluster/nodes/$node/config/files/$file
/explore/clusters/$cluster/nodes/$node/config/files
/explore/nodes/$node/log/files/$file
/explore/nodes/$node/log/files
/explore/clusters/$cluster/nodes/$node/log/files/$file
/explore/clusters/$cluster/nodes/$node/log/files
/explore/clusters/$cluster
/explore/clusters
/explore
/explore/$resource (Resources: [routes,props,jobs,ping])
/control/nodes/$node/repl-fullsync-stop/$arg1
/control/nodes/$node/repl-fullsync-stop
/control/nodes/$node/repl-fullsync-start/$arg1
/control/nodes/$node/repl-fullsync-start
/control/nodes/$node/repl-fullsync-disable/$arg1
/control/nodes/$node/repl-fullsync-enable/$arg1
/control/nodes/$node/repl-realtime-stop/$arg1
/control/nodes/$node/repl-realtime-stop
/control/nodes/$node/repl-realtime-start/$arg1
/control/nodes/$node/repl-realtime-start
/control/nodes/$node/repl-realtime-disable/$arg1
/control/nodes/$node/repl-realtime-enable/$arg1
/control/nodes/$node/repl-clusterstats-realtime
/control/nodes/$node/repl-clusterstats-proxy_get
/control/nodes/$node/repl-clusterstats-fullsync
/control/nodes/$node/repl-clusterstats-fs_coordinate
/control/nodes/$node/repl-clusterstats-cluster_mgr
/control/nodes/$node/repl-clusterstats/$arg1/$arg2
/control/nodes/$node/repl-clusterstats
/control/nodes/$node/repl-connections
/control/nodes/$node/repl-disconnect/$arg1
/control/nodes/$node/repl-connect/$arg1/$arg2
/control/nodes/$node/repl-clustername/$arg1
/control/nodes/$node/repl-clustername
/control/nodes/$node/aae-status
/control/nodes/$node/transfers
/control/nodes/$node/ringready
/control/nodes/$node/status
/control/nodes/$node/clear
/control/nodes/$node/commit
/control/nodes/$node/plan
/control/nodes/$node/force-replace/$arg1/$arg2
/control/nodes/$node/staged-replace/$arg1/$arg2
/control/nodes/$node/replace/$arg1/$arg2
/control/nodes/$node/force-remove/$arg1
/control/nodes/$node/staged-leave/$arg1
/control/nodes/$node/staged-leave
/control/nodes/$node/staged-join/$arg1
/control/nodes/$node/leave/$arg1
/control/nodes/$node/join/$arg1
/control/nodes/$node/repair
/control/clusters/$cluster/repl-fullsync-stop/$arg1
/control/clusters/$cluster/repl-fullsync-stop
/control/clusters/$cluster/repl-fullsync-start/$arg1
/control/clusters/$cluster/repl-fullsync-start
/control/clusters/$cluster/repl-fullsync-disable/$arg1
/control/clusters/$cluster/repl-fullsync-enable/$arg1
/control/clusters/$cluster/repl-realtime-stop/$arg1
/control/clusters/$cluster/repl-realtime-stop
/control/clusters/$cluster/repl-realtime-start/$arg1
/control/clusters/$cluster/repl-realtime-start
/control/clusters/$cluster/repl-realtime-disable/$arg1
/control/clusters/$cluster/repl-realtime-enable/$arg1
/control/clusters/$cluster/repl-clusterstats-realtime
/control/clusters/$cluster/repl-clusterstats-proxy_get
/control/clusters/$cluster/repl-clusterstats-fullsync
/control/clusters/$cluster/repl-clusterstats-fs_coordinate
/control/clusters/$cluster/repl-clusterstats-cluster_mgr
/control/clusters/$cluster/repl-clusterstats/$arg1/$arg2
/control/clusters/$cluster/repl-clusterstats
/control/clusters/$cluster/repl-connections
/control/clusters/$cluster/repl-disconnect/$arg1
/control/clusters/$cluster/repl-connect/$arg1/$arg2
/control/clusters/$cluster/repl-clustername/$arg1
/control/clusters/$cluster/repl-clustername
/control/clusters/$cluster/aae-status
/control/clusters/$cluster/transfers
/control/clusters/$cluster/ringready
/control/clusters/$cluster/status
/control/clusters/$cluster/clear
/control/clusters/$cluster/commit
/control/clusters/$cluster/plan
/control/clusters/$cluster/force-replace/$arg1/$arg2
/control/clusters/$cluster/staged-replace/$arg1/$arg2
/control/clusters/$cluster/replace/$arg1/$arg2
/control/clusters/$cluster/force-remove/$arg1
/control/clusters/$cluster/staged-leave/$arg1
/control/clusters/$cluster/staged-leave
/control/clusters/$cluster/staged-join/$arg1
/control/clusters/$cluster/leave/$arg1
/control/clusters/$cluster/join/$arg1
/control/clusters/$cluster/repair
/riak/nodes/$node/$* (Riak Direct HTTP Proxy)
/riak/clusters/$cluster/$* (Riak Direct HTTP Proxy)
/$* (Static Endpoint)
```

Explanation:

* `$cluster`: Specifying `default` will use the cluster that this riak_explorer is connected to.
* `$node`: Example: `riak@127.0.0.1`
* `$bucket_type`: Example: `default`
* `$bucket`: Example: `mybucket`
* `$key`: Example: `mykey`
* `$schema`: Example: `_yz_default`
* `$index`: Example: `myindex`
* `$*`: Wildcard with deep paths. Example: `assets/ember-riak-explorer.js` for the static route, or `ping` for the riak_proxy route
* `$resource`: A list of valid `resources` for a given module can be found in `explore.resources`

## Seed Data (For developers and testers)

Some suggestions on how to create some sample data, to try out the Explorer GUI.

1. Set up a couple of clusters in `riak_explorer.conf`. Have one or more with
    `development_mode = on`, and one or more with it set to `off` (meaning, in
    production mode).

2. Enable Search in Riak's config file (`riak.conf`). Set up a [Search
    Index](http://docs.basho.com/riak/latest/dev/using/search/#Simple-Setup).
    For example, to create a search index named `test-users-idx` that uses
    the default schema, do a PUT from the command-line (assuming your Riak
    node is available on `localhost`, using the default HTTP port `8098`):

    ```
    curl -XPUT http://localhost:8098/search/index/test-users-idx
    ```

3. Set up a `users` Bucket Type, and associate it with the `users-idx` Search
    index created above:

    ```
    riak-admin bucket-type create test-users '{"props":{"search_index":"test-users-idx"}}'
    riak-admin bucket-type activate test-users
    ```

4. Create and activate a Bucket Type for each main [Riak Data
    Type](http://docs.basho.com/riak/latest/dev/using/data-types/):

    ```
    riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
    riak-admin bucket-type activate maps
    riak-admin bucket-type create sets '{"props":{"datatype":"set"}}'
    riak-admin bucket-type activate sets
    riak-admin bucket-type create counters '{"props":{"datatype":"counter"}}'
    riak-admin bucket-type activate counters
    ```

5. Create and activate a `test-carts` Bucket Type, with [Siblings](http://docs.basho.com/riak/latest/dev/using/conflict-resolution/#Siblings)
    enabled:

    ```
    riak-admin bucket-type create test-carts '{"props":{"allow_mult":true}}'
    ```

6. Insert some sample Counter type objects, say to the `test-page-loads` bucket:

  ```
  curl localhost:8098/types/counters/buckets/test-page-loads/datatypes/page123 -XPOST \
    -H "Content-Type: application/json" \
    -d '{"increment": 5}'

  curl localhost:8098/types/counters/buckets/test-page-loads/datatypes/page456 -XPOST \
    -H "Content-Type: application/json" \
    -d '{"increment": 1}'
  ```

6. Insert some sample Set type objects, say to the `test-cities-visited` bucket:

  ```
  curl localhost:8098/types/sets/buckets/test-cities-visited/datatypes/user123 -XPOST \
    -H "Content-Type: application/json" \
    -d '{"add_all":["Toronto", "Montreal", "Quebec", "New York City"]}'

  curl localhost:8098/types/sets/buckets/test-cities-visited/datatypes/user456 -XPOST \
    -H "Content-Type: application/json" \
    -d '{"add_all":["Washington D.C.", "Los Angeles", "Las Vegas"]}'
  ```

6. Insert some sample Map type objects, say to the `test-tweets` bucket:

  ```
  curl localhost:8098/types/maps/buckets/test-tweets/datatypes/user123 -XPOST \
    -H "Content-Type: application/json" \
    -d '{"update":{ "favorited_flag": "disable", "id_str_register": "240859602684612608", "favourites_count_counter": 24, "entities_map":{ "update": { "urls_set":{ "add_all": ["url1", "url2", "url3"]}} }  }}'

  curl localhost:8098/types/maps/buckets/test-tweets/datatypes/user456 -XPOST \
    -H "Content-Type: application/json" \
    -d '{"update":{ "favorited_flag": "enable", "id_str_register": "240859602699912715", "favourites_count_counter": 1, "entities_map":{ "update": { "urls_set":{ "add_all": ["url4", "url5", "url6"]}} }  }}'
  ```

## Development / Contributing

For developer installation instructions and environment setup, visit
[DEVELOPMENT.md](DEVELOPMENT.md).

* Whether your contribution is for a bug fix or a feature request, **create an [Issue](https://github.com/basho/riak_explorer/issues)** and let us know what you are thinking.
* **For bugs**, if you have already found a fix, feel free to submit a Pull Request referencing the Issue you created.
* **For feature requests**, we want to improve upon the library incrementally which means small changes at a time. In order ensure your PR can be reviewed in a timely manner, please keep PRs small, e.g. <10 files and <500 lines changed. If you think this is unrealistic, then mention that within the Issue and we can discuss it.

Once you're ready to contribute code back to this repo, start with these steps:

* Fork the appropriate sub-projects that are affected by your change
* Create a topic branch for your change and checkout that branch
     `git checkout -b some-topic-branch`
* Make your changes and run the test suite if one is provided (see below)
* Commit your changes and push them to your fork
* Open a pull request for the appropriate project
* Contributors will review your pull request, suggest changes, and merge it when it’s ready and/or offer feedback
* To report a bug or issue, please open a new issue against this repository

You can [read the full guidelines for bug reporting and code contributions](http://docs.basho.com/riak/latest/community/bugs/) on the Riak Docs.

And **thank you!** Your contribution is incredibly important to us. It'd be great for you to add it to a current or past community release note [here](https://github.com/basho-labs/the-riak-community/tree/master/release-notes).

#### Related Projects
- [riak-explorer-gui](https://github.com/basho-labs/riak-explorer-gui) - the
    front-end Ember.js GUI code to go along with the Explorer API.
- [riak_control](https://github.com/basho/riak_control) - legacy official Riak
    GUI
- [riak_cs_control](https://github.com/basho/riak_cs_control) - legacy official
    Riak S2 (Riak CS) GUI
- [rekon](https://github.com/basho/rekon) (old bucket / object explorer gui) -
    legacy unofficial Javascript Riak GUI.
