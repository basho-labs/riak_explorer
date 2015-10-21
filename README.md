# Riak Explorer

Riak Explorer provides browsing and admin capabilities for [Riak KV](http://basho.com/products/riak-kv/), a distributed NoSQL key-value data store that offers high availability, fault tolerance, operational simplicity, and scalability.

Riak Explorer is useful while in a development or production. It includes convenient methods to browse Bucket Types, Buckets, Keys, view and edit Riak Objects, and more. To prevent heavy I/O requests from key listings, be sure to edit the config file to reflect the environment as [explained in Using Riak Explorer](#using-riak-explorer).


* [Installation](#installation)
* [System Architecture](#architecture)
* [Using Riak Explorer](#using-riak-explorer)
* [Development / Contributing](#development--contributing)


## Installation

1. Download and extract [This file on Mac OS X](http://riak-tools.s3.amazonaws.com/riak_explorer_darwin_amd64.tar.gz) or [this file for Ubuntu 14.04](http://riak-tools.s3.amazonaws.com/riak_explorer_linux_amd64.tar.gz).
     * *Note: If you'd like to support further OSes, please [open an Issue](https://github.com/basho-labs/riak_explorer/issues)*

1. Extract the tar file and `cd` into the directory

2. Verify the default settings in `etc/riak_explorer.conf` will work for your configuration (primarily that port 9000 is available on your host). Pay special attention to development mode settings, this should be `off` for use with a production environment to prevent accidental keylistings.

4. Run `bin/riak_explorer start` start the `riak_explorer` application

4. Navigate to [http://localhost:9000/](http://localhost:9000/) to see the interface

## System Architecture

*Front-end GUI:* [Ember.js](http://emberjs.com/)  
*Back-end:* [Erlang](http://www.erlang.org/) is the primary development language and [WebMachine](http://webmachine.github.io/) is used to serve a RESTful API

For more about this architecture, see [DEVELOPMENT.md](DEVELOPMENT.md).

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

## Development / Contributing

For specifics on builds, visit [DEVELOPMENT.md](DEVELOPMENT.md).

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
- [riak-explorer-gui](https://github.com/basho-labs/riak-explorer-gui)
- [riak_control](https://github.com/basho/riak_control)
- [rekon](https://github.com/basho/rekon) (old bucket / object explorer gui)
- [riak_cs_control](https://github.com/basho/riak_cs_control)
