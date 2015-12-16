FORMAT: 1A
HOST: http://localhost:9000

# Riak Explorer API

The Riak Explorer project provides REST APIs for data viewing and cluster
management for Riak clusters.

# Group Explore [/explore]
The `/explore` set of endpoints serves as an enhancement to
Riak's own HTTP API, and fills in missing data browsing and
management functionality.

## Clusters Collection [/explore/clusters/]

### List All Clusters [GET]
Get a list of all the clusters listed in Explorer's `riak_explorer.conf` file,
sorted alphabetically by cluster id.

+ Response 200 (application/json)

        {
            "clusters": [
                {
                    "available": false,
                    "development_mode": false,
                    "id": "production1",
                    "riak_node": "riak@192.168.0.1",
                    "riak_type": "unavailable",
                    "riak_version": "unavailable"
                },
                {
                    "available": false,
                    "development_mode": true,
                    "id": "qa1",
                    "riak_node": "dev8@127.0.0.1",
                    "riak_type": "unavailable",
                    "riak_version": "unavailable"
                },
                {
                    "available": true,
                    "development_mode": true,
                    "id": "localdev",
                    "riak_node": "dev1@127.0.0.1",
                    "riak_type": "oss",
                    "riak_version": "2.1.0"
                }
            ],
            "links": {
                "self": "/explore/clusters"
            }
        }

## Cluster [/explore/clusters/{cluster_id}]

### Retrieve a Cluster [GET]
Get the properties of a cluster listed in Explorer's `riak_explorer.conf` file.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id as defined in the
        `riak_explorer.conf` file.

+ Response 200 (application/json)

        {
            "cluster": {
                "available": true,
                "development_mode": true,
                "id": "localdev",
                "riak_node": "dev1@127.0.0.1",
                "riak_type": "oss",
                "riak_version": "2.1.0"
            },
            "links": {
                "self": "/explore/clusters/localdev"
            }
        }

+ Response 404 (text/html)

## Nodes Collection [/explore/clusters/{cluster_id}/nodes]

### List all Nodes [GET]
Get a list of all Nodes that are joined into the specified cluster.

+ Response 200 (application/json)

        {
            "nodes": [
                {
                    "id": "dev1@127.0.0.1"
                },
                {
                    "id": "dev2@127.0.0.1"
                },
                {
                    "id": "dev3@127.0.0.1"
                }
            ],
            "links": {
                "self": "/explore/clusters/localdev/nodes"
            }
        }

+ Response 404 (text/html)

## Node [/explore/clusters/{cluster_id}/nodes/{node_id}]

### Retrieve a Node [GET]
Retrieves the properties of a single node within the specified cluster.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `dev1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).

+ Response 200 (application/json)

        {
            "node": {
                "id": "riak@127.0.0.1"
            },
            "links": {
                "self": "/explore/clusters/localdev/nodes/dev1@127.0.0.1"
            }
        }

+ Response 404 (text/html)

## Node Effective Config [/explore/clusters/{cluster_id}/nodes/{node_id}/config]

### Retrieve a Node's effective config [GET]
Retrieves the effective config settings for a specified node (the equivalent
of running `riak config effective` on the command line).

Note the `advanced_config` section. Since Riak's `advanced.config` file is
not parsed by [cuttlefish](https://github.com/basho/cuttlefish), its settings
are not included in the effective config output. Explorer includes the
various settings in that file explicitly, for developer convenience, in
Erlang term config format.

In some cases where a cluster was upgraded from Riak 1.4, nodes will have
legacy `app.config` and `vm.args` files instead of `riak.conf`, and cuttlefish
will not be able to provide effective config listings. In those cases,
it will return a `404 Object Not Found` in `application/json` format, with
the following error body:

```
{
    "error": "Legacy configuration files found, effective config not available."
}
```

If an invalid node id is specified:

```
{
    "error": "Invalid node id or node not available."
}
```

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `dev1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).

+ Response 200 (application/json)

        {
            "config": {
                "advanced_config": [
                    "{riak_core,[{cluster_mgr,{\"127.0.0.1\",8098}}]}",
                    "{riak_repl,[{data_root,\"/var/db/riak/riak_repl/\"}]}"
                ],
                "config": {
                    "anti_entropy": "active",
                    "anti_entropy.bloomfilter": "on",
                    "anti_entropy.concurrency_limit": "2",
                    "anti_entropy.data_dir": "$(platform_data_dir)/anti_entropy",
                    "anti_entropy.max_open_files": "20",
                    "anti_entropy.throttle": "on",
                    "anti_entropy.tree.build_limit.number": "1",
                    "anti_entropy.tree.build_limit.per_timespan": "1h",
                    "anti_entropy.tree.expiry": "1w",
                    "anti_entropy.trigger_interval": "15s",
                    "anti_entropy.use_background_manager": "off",
                    "anti_entropy.write_buffer_size": "4MB",
                    "background_manager": "off",
                    "bitcask.data_root": "$(platform_data_dir)/bitcask",
                    "bitcask.expiry": "off",
                    "bitcask.expiry.grace_time": "0",
                    "bitcask.fold.max_age": "unlimited",
                    "bitcask.fold.max_puts": "0",
                    "bitcask.hintfile_checksums": "strict",
                    "bitcask.io_mode": "erlang",
                    "bitcask.max_file_size": "2GB",
                    "bitcask.max_merge_size": "100GB",
                    "bitcask.merge.policy": "always",
                    "bitcask.merge.thresholds.dead_bytes": "128MB",
                    "bitcask.merge.thresholds.fragmentation": "40",
                    "bitcask.merge.thresholds.small_file": "10MB",
                    "bitcask.merge.triggers.dead_bytes": "512MB",
                    "bitcask.merge.triggers.fragmentation": "60",
                    "bitcask.merge.window.end": "23",
                    "bitcask.merge.window.start": "0",
                    "bitcask.merge_check_interval": "3m",
                    "bitcask.merge_check_jitter": "30%",
                    "bitcask.open_timeout": "4s",
                    "bitcask.sync.strategy": "none",
                    "buckets.default.allow_mult": "false",
                    "buckets.default.basic_quorum": "false",
                    "buckets.default.dw": "quorum",
                    "buckets.default.last_write_wins": "false",
                    "buckets.default.merge_strategy": "1",
                    "buckets.default.n_val": "3",
                    "buckets.default.notfound_ok": "true",
                    "buckets.default.pr": "0",
                    "buckets.default.pw": "0",
                    "buckets.default.r": "quorum",
                    "buckets.default.rw": "quorum",
                    "buckets.default.w": "quorum",
                    "check_crl": "on",
                    "datatypes.compression_level": "1",
                    "distributed_cookie": "riak",
                    "dtrace": "off",
                    "erlang.K": "on",
                    "erlang.W": "w",
                    "erlang.async_threads": "64",
                    "erlang.crash_dump": "./log/erl_crash.dump",
                    "erlang.distribution_buffer_size": "32MB",
                    "erlang.fullsweep_after": "0",
                    "erlang.max_ets_tables": "256000",
                    "erlang.max_ports": "65536",
                    "erlang.process_limit": "256000",
                    "erlang.schedulers.compaction_of_load": "false",
                    "erlang.schedulers.force_wakeup_interval": "500",
                    "erlang.shutdown_time": "10s",
                    "erlang.smp": "enable",
                    "handoff.inbound": "on",
                    "handoff.ip": "127.0.0.1",
                    "handoff.max_rejects": "6",
                    "handoff.outbound": "on",
                    "handoff.port": "10019",
                    "handoff.use_background_manager": "off",
                    "honor_cipher_order": "on",
                    "javascript.hook_pool_size": "2",
                    "javascript.map_pool_size": "8",
                    "javascript.maximum_heap_size": "8MB",
                    "javascript.maximum_stack_size": "16MB",
                    "javascript.reduce_pool_size": "6",
                    "leveldb.block.restart_interval": "16",
                    "leveldb.block.size": "4KB",
                    "leveldb.block.size_steps": "16",
                    "leveldb.block_cache_threshold": "32MB",
                    "leveldb.bloomfilter": "on",
                    "leveldb.compaction.trigger.tombstone_count": "1000",
                    "leveldb.compression": "on",
                    "leveldb.data_root": "$(platform_data_dir)/leveldb",
                    "leveldb.fadvise_willneed": "false",
                    "leveldb.limited_developer_mem": "on",
                    "leveldb.maximum_memory.percent": "70",
                    "leveldb.sync_on_write": "off",
                    "leveldb.threads": "71",
                    "leveldb.tiered": "off",
                    "leveldb.verify_checksums": "on",
                    "leveldb.verify_compaction": "on",
                    "leveldb.write_buffer_size_max": "60MB",
                    "leveldb.write_buffer_size_min": "30MB",
                    "listener.http.internal": "127.0.0.1:10018",
                    "listener.protobuf.internal": "127.0.0.1:10017",
                    "log.console": "both",
                    "log.console.file": "$(platform_log_dir)/console.log",
                    "log.console.level": "info",
                    "log.crash": "on",
                    "log.crash.file": "$(platform_log_dir)/crash.log",
                    "log.crash.maximum_message_size": "64KB",
                    "log.crash.rotation": "$D0",
                    "log.crash.rotation.keep": "5",
                    "log.crash.size": "10MB",
                    "log.error.file": "$(platform_log_dir)/error.log",
                    "log.error.messages_per_second": "100",
                    "log.error.redirect": "on",
                    "log.syslog": "off",
                    "log.syslog.facility": "daemon",
                    "log.syslog.ident": "riak",
                    "log.syslog.level": "info",
                    "max_concurrent_requests": "50000",
                    "metadata_cache_size": "off",
                    "nodename": "dev1@127.0.0.1",
                    "object.format": "1",
                    "object.siblings.maximum": "100",
                    "object.siblings.warning_threshold": "25",
                    "object.size.maximum": "50MB",
                    "object.size.warning_threshold": "5MB",
                    "platform_bin_dir": "./bin",
                    "platform_data_dir": "./data",
                    "platform_etc_dir": "./etc",
                    "platform_lib_dir": "./lib",
                    "platform_log_dir": "./log",
                    "protobuf.backlog": "128",
                    "protobuf.keepalive": "on",
                    "protobuf.nagle": "off",
                    "retry_put_coordinator_failure": "on",
                    "riak_control": "off",
                    "riak_control.auth.mode": "off",
                    "ring.state_dir": "$(platform_data_dir)/ring",
                    "ring_size": "64",
                    "runtime_health.thresholds.busy_ports": "2",
                    "runtime_health.thresholds.busy_processes": "30",
                    "runtime_health.triggers.distribution_port": "on",
                    "runtime_health.triggers.port": "on",
                    "runtime_health.triggers.process.garbage_collection": "off",
                    "runtime_health.triggers.process.heap_size": "160444000",
                    "runtime_health.triggers.process.long_schedule": "off",
                    "sasl": "off",
                    "search": "off",
                    "search.anti_entropy.data_dir": "$(platform_data_dir)/yz_anti_entropy",
                    "search.root_dir": "$(platform_data_dir)/yz",
                    "search.solr.jmx_port": "10013",
                    "search.solr.jvm_options": "-d64 -Xms1g -Xmx1g -XX:+UseStringCache -XX:+UseCompressedOops",
                    "search.solr.port": "10014",
                    "search.solr.start_timeout": "30s",
                    "secure_referer_check": "on",
                    "storage_backend": "bitcask",
                    "strong_consistency": "off",
                    "tls_protocols.sslv3": "off",
                    "tls_protocols.tlsv1": "off",
                    "tls_protocols.tlsv1.1": "off",
                    "tls_protocols.tlsv1.2": "on",
                    "transfer_limit": "2",
                    "vnode_management_timer": "10s"
                }
            },
            "links": {
                "self": "/explore/clusters/localdev/nodes/dev1@127.0.0.1/config"
            }
        }

+ Response 404 (application/json)

        {
            "error": "Invalid node id or node not available."
        }

## Node Config Files Collection [/explore/clusters/{cluster_id}/nodes/{node_id}/config/files]

### List all config files for a Node [GET]
Retrieves a list of all config files that reside in that node's `etc` directory.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `dev1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).

+ Response 200 (application/json)

        {
            "files": [
                {
                    "id": "advanced.config"
                },
                {
                    "id": "riak.conf"
                },
                {
                    "id": "solr-log4j.properties"
                }
            ],
            "links": {
                "self": "/explore/clusters/localdev/nodes/dev1@127.0.0.1/config/files"
            }
        }

## Node Config File [/explore/clusters/{cluster_id}/nodes/{node_id}/config/files/{file_name}]

### Retrieve a Config File for a Node [GET]
Retrieves a specific config file that belongs to a node, in its entirety.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `dev1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).
    + file_name: `riak.conf` (string) - Name of the config file.

+ Response 200 (plain/text)

    + Headers

            Accept: plain/text

    + Body

            ## Where to emit the default log messages (typically at 'info'
            ## severity):
            ## off: disabled
            ## file: the file specified by log.console.file
            ## console: to standard output (seen when using `riak attach-direct`)
            ## both: log.console.file and standard out.
            ##
            ## Default: both
            ##
            ## Acceptable values:
            ##   - one of: off, file, console, both
            log.console = both
            ...

+ Response 404 (text/html)

## Node Log Files Collection [/explore/clusters/{cluster_id}/nodes/{node_id}/log/files]

### List all Log Files for a Node [GET]
Retrieves a list of all log files that reside in that node's `log` directory.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `dev1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).

+ Response 200 (application/json)

        {
            "files": [
                {
                    "id": "console.log"
                },
                {
                    "id": "console.log.0"
                },
                {
                    "id": "crash.log"
                },
                {
                    "id": "erlang.log.1"
                },
                {
                    "id": "error.log"
                },
                {
                    "id": "error.log.4"
                },
                {
                    "id": "run_erl.log"
                }
            ],
            "links": {
                "self": "/explore/clusters/localdev/nodes/dev1@127.0.0.1/log/files"
            }
        }

+ Response 404 (text/html)

## Node Log File [/explore/clusters/{cluster_id}/nodes/{node_id}/log/files/{file_name}?rows={rows}]

### Retrieve a Log File for a Node [GET]
Retrieves the results of tailing a specific log file that belongs to a node.
(Equivalent to `tail -n 1000 $log_file_name`, by default).
Only the log files that reside in the Riak node's `log` directory are allowed
(that is, only the files that appear in the response to `.../log/files`).

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `dev1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).
    + file_name: `console.log` (string) - Name of the log file.
    + rows: `5` (integer, optional) - Number of lines (from the end of the
          log) to return. Similar to the command line `tail -n $rows`.
          + Default: 1000

+ Response 200 (plain/text)

    + Headers

            Accept: plain/text

    + Body

            2015-12-14 12:36:23.709 [info] <0.20541.0>@re_riak_patch:check_existence:135 Checking ./etc/advanced.config exists... true
            2015-12-14 12:36:23.709 [info] <0.20541.0>@re_riak_patch:check_existence:135 Checking ./etc/vm.args exists... false
            2015-12-14 12:36:23.708 [info] <0.20541.0>@re_riak_patch:check_existence:135 Checking ./etc/app.config exists... false
            2015-12-14 12:36:23.588 [info] <0.20541.0>@re_riak_patch:check_existence:135 Checking ./etc/riak.conf exists... true

+ Response 404 (text/html)

## Bucket Types Collection [/explore/clusters/{cluster_id}/bucket_types]

### List all Bucket Types [GET]
Return all bucket types that exist on the cluster, both active and inactive.
Equivalent to running `riak-admin bucket-type list` on the command line.
Also includes each bucket type's properties hash (the equivalent of
`riak-admin bucket-type status $type`).

Unlike the command-line version, this API request returns the list of bucket
types sorted alphabetically.

+ Response 200 (application/json)

        {
            "bucket_types": [
                {
                    "id": "default",
                    "props": {
                        "active": true,
                        "allow_mult": false,
                        "basic_quorum": false,
                        "big_vclock": 50,
                        "chash_keyfun": "{riak_core_util,chash_std_keyfun}",
                        "dvv_enabled": false,
                        "dw": "quorum",
                        "last_write_wins": false,
                        "linkfun": "{modfun,riak_kv_wm_link_walker,mapreduce_linkfun}",
                        "n_val": 3,
                        "notfound_ok": true,
                        "old_vclock": 86400,
                        "postcommit": [],
                        "pr": 0,
                        "precommit": [],
                        "pw": 0,
                        "r": "quorum",
                        "rw": "quorum",
                        "small_vclock": 50,
                        "w": "quorum",
                        "write_once": false,
                        "young_vclock": 20
                    }
                },
                {
                    "id": "users",
                    "props": {
                        "active": true,
                        "allow_mult": true,
                        "basic_quorum": false,
                        "big_vclock": 50,
                        "chash_keyfun": "{riak_core_util,chash_std_keyfun}",
                        "claimant": "'riak@127.0.0.1'",
                        "dvv_enabled": true,
                        "dw": "quorum",
                        "last_write_wins": false,
                        "linkfun": "{modfun,riak_kv_wm_link_walker,mapreduce_linkfun}",
                        "n_val": 3,
                        "notfound_ok": true,
                        "old_vclock": 86400,
                        "postcommit": [],
                        "pr": 0,
                        "precommit": [],
                        "pw": 0,
                        "r": "quorum",
                        "rw": "quorum",
                        "search_index": "users_index",
                        "small_vclock": 50,
                        "w": "quorum",
                        "young_vclock": 20
                    }
                }
            ]
        }

## Bucket Type [/explore/clusters/{cluster_id}/bucket_types/{bucket_type_id}]

### Retrieve a Bucket Type [GET]
Returns the properties hash for the specified bucket type
(the equivalent of running `riak-admin bucket-type status $type` on the command
line).

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + bucket_type_id: `default` (string) - The name of the Riak Bucket Type.

+ Response 200 (application/json)

        {
            "bucket_type": {
                "id": "default",
                "props": {
                    "active": true,
                    "allow_mult": false,
                    "basic_quorum": false,
                    "big_vclock": 50,
                    "chash_keyfun": "{riak_core_util,chash_std_keyfun}",
                    "dvv_enabled": false,
                    "dw": "quorum",
                    "last_write_wins": false,
                    "linkfun": "{modfun,riak_kv_wm_link_walker,mapreduce_linkfun}",
                    "n_val": 3,
                    "notfound_ok": true,
                    "old_vclock": 86400,
                    "postcommit": [],
                    "pr": 0,
                    "precommit": [],
                    "pw": 0,
                    "r": "quorum",
                    "rw": "quorum",
                    "small_vclock": 50,
                    "w": "quorum",
                    "write_once": false,
                    "young_vclock": 20
                }
            },
            "links": {
                "self": "/explore/clusters/localdev/bucket_types/default"
            }
        }

+ Response 404

## Cached Bucket List [/explore/clusters/{cluster_id}/bucket_types/{bucket_type_id}/buckets]

Listing buckets is a prohibitively expensive operation in Riak, due to it being
a distributed K/V store based on consistent hashing. Although the
[List Buckets](http://docs.basho.com/riak/latest/dev/references/http/list-buckets/)
command exists, executing it on a production cluster with a non-trivial dataset
can have a catastrophic impact on its resources, and is strictly not recommended.

To provide a better developer experience while working within this limitation,
Riak Explorer stores a cached bucket list (as a plain newline-separated text
file) on the API side, located in
`$explorer_dir/data/buckets/{cluster_id}/{bucket_type_id}/`. The filename for
the cached list is based on the timestamp at which the cache was generated.

While a cluster is in [development mode](https://github.com/basho-labs/riak_explorer#development-mode),
this cached list can be automatically populated from a Streaming List Buckets
request. In Production mode (setting `clusters.{cluster_id}.development_mode = off`
in `riak_explorer.conf`), the List Buckets functionality is disabled.
However, users can still provide their own cached bucket list, either by
generating it elsewhere (like on a separate Staging cluster), or by editing
the bucket list directly through the Explorer GUI.

### Clear a Cached Bucket List [DELETE]
Clears the entire cached bucket list for the given bucket type (deletes the
cached text file on the API side).

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + bucket_type_id: `default` (string) - The name of the Riak Bucket Type.

+ Response 204

### Edit a Cached Bucket List [PUT]

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + bucket_type_id: `default` (string) - The name of the Riak Bucket Type.

+ Request (plain/text)

        accounts
        books
        authors
        users

+ Response 204

## Paginated Cached Bucket List [/explore/clusters/{cluster_id}/bucket_types/{bucket_type_id}/buckets?start={start}&rows={rows}]
Given that there is no limit to the number of buckets that can exist within a
bucket type, the cached list can get rather large. A pagination API is provided
for client convenience.

### Retrieve a Paginated Cached Bucket List [GET]
Retrieves the cached bucket list for a given bucket type, with pagination
controls parameters.

Returns a `404 Object Not Found` if no cached list exists on the API side
(note that this doesn't mean that no buckets actually exist, in the cluster).

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + bucket_type_id: `default` (string) - The name of the Riak Bucket Type.
    + start: `1` (integer, optional) - Which bucket to start from, counting
        from the start of the list. Used for pagination.

        + Default: 1

    + rows: `500` (integer, optional) - How many buckets to return in the
        response. Used for pagination.

        + Default: 1000

+ Response 200 (application/json)

        {
            "buckets": {
                "buckets": [
                    "accounts",
                    "transactions",
                    "users"
                ],
                "count": 3,
                "created": "2015-12-16 15:23:24",
                "total": 3
            },
            "links": {
                "self": "/explore/clusters/production1/bucket_types/default/buckets"
            }
        }

+ Response 404

## Cache Refresh from Streaming List Buckets [/explore/clusters/{cluster_id}/bucket_types/{bucket_type_id}/refresh_buckets/source/riak_kv?sort={sort}]
**(Development Mode Only)**
As a convenience to developers, Explorer can automatically populate a cached
bucket list from the result of a
[Streaming List Buckets](http://docs.basho.com/riak/latest/dev/references/http/list-buckets/)
request to Riak.

### Refresh the Cached Bucket List [POST]
Kicks off a Refresh Job to populate a bucket type's cached bucket list from a
Streaming List Buckets request.

Since the List Buckets results arrive in random order, a `sort` parameter is
also provided.

When used in Production Mode, requests made to this API endpoint result in an
HTTP `403 Forbidden`.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + bucket_type_id: `default` (string) - The name of the Riak Bucket Type.
    + sort: `true` (boolean, optional) - Sorts the cached list in alphabetical
        order if set to `true`.

        + Default: true

+ Response 204

+ Response 403

## Cached Key List [/explore/clusters/{cluster_id}/bucket_types/{bucket_type_id}/buckets/{bucket_id}/keys]

Listing keys is a prohibitively expensive operation in Riak, due to it being
a distributed K/V store based on consistent hashing. Although the
[List Keys](http://docs.basho.com/riak/latest/dev/references/http/list-keys/)
command exists, executing it on a production cluster with a non-trivial dataset
can have a catastrophic impact on its resources, and is strictly not recommended.

To provide a better developer experience while working within this limitation,
Riak Explorer stores a cached key list (as a plain newline-separated text
file) on the API side, located in
`$explorer_dir/data/buckets/{cluster_id}/{bucket_type_id}/{bucket_id}/`. The
filename for the cached list is based on the timestamp at which the cache was
generated.

While a cluster is in [development mode](https://github.com/basho-labs/riak_explorer#development-mode),
this cached list can be automatically populated from a Streaming List Keys
request. In Production mode (setting `clusters.{cluster_id}.development_mode = off`
in `riak_explorer.conf`), the List Keys functionality is disabled.
However, users can still provide their own cached key list, either by
generating it elsewhere (like on a separate Staging cluster), or by editing
the key list directly through the Explorer GUI.

### Clear a Cached Key List [DELETE]
Clears the entire cached key list for the given bucket type (deletes the
cached text file on the API side).

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + bucket_type_id: `default` (string) - The name of the Riak Bucket Type.
    + bucket_id: `users` (string) - The bucket name (unique within the bucket type).

+ Response 204

### Edit a Cached Key List [PUT]

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + bucket_type_id: `default` (string) - The name of the Riak Bucket Type.
    + bucket_id: `users` (string) - The bucket name (unique within the bucket type).

+ Request (plain/text)

        user1
        user3
        user4
        user5

+ Response 204

## Paginated Cached Key List [/explore/clusters/{cluster_id}/bucket_types/{type_id}/buckets/{bucket_id}/keys?start={start}&rows={rows}]
Given that there is no limit to the number of keys that can exist within a
bucket, the cached key list can get extremely large. A pagination API is provided
for client convenience.

### Retrieve a Paginated Cached Key List [GET]
Retrieves the cached key list for a given bucket type, with pagination
controls parameters.

Returns a `404 Object Not Found` if no cached list exists on the API side
(note that this doesn't mean that no keys actually exist in that bucket,
in the actual cluster).

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + type_id: `default` (string) - The name of the Riak Bucket Type.
    + bucket_id: `users` (string) - The bucket name (unique within the bucket type).
    + start: `1` (integer, optional) - Which key to start from, counting
        from the start of the list. Used for pagination.

        + Default: 1

    + rows: `500` (integer, optional) - How many keys to return in the
        response. Used for pagination.

        + Default: 1000

+ Response 200 (application/json)

        {
            "keys": {
                "count": 3,
                "created": "2015-12-15 16:42:19",
                "keys": [
                    "user1",
                    "user2",
                    "user3"
                ],
                "total": 3
            },
            "links": {
                "self": "/explore/clusters/production1/bucket_types/default/buckets/users/keys"
            }
        }

+ Response 404

## Cache Refresh from Streaming List Keys [/explore/clusters/{cluster_id}/bucket_types/{type_id}/buckets/{bucket_id}/refresh_keys/source/riak_kv?sort={sort}]
**(Development Mode Only)**
As a convenience to developers, Explorer can automatically populate a cached
key list from the result of a
[Streaming List Keys](http://docs.basho.com/riak/latest/dev/references/http/list-keys/)
request to Riak.

### Refresh the Cached Key List [POST]
Kicks off a Refresh Job to populate a bucket's cached key list from a
Streaming List Key request.

Since the List Keys results arrive in random order, a `sort` parameter is
also provided.

When used in Production Mode, requests made to this API endpoint result in an
HTTP `403 Forbidden`.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + type_id: `default` (string) - The name of the Riak Bucket Type.
    + bucket_id: `users` (string) - The bucket name (unique within the bucket type).
    + sort: `true` (boolean, optional) - Sorts the cached list in alphabetical
        order if set to `true`.

        + Default: true

+ Response 204

+ Response 403

# Group Riak Proxy [/riak]
The `/riak` set of endpoints allow for clients to proxy requests to nodes
in the cluster.

## Riak Ping [/riak/nodes/{node_id}/ping]

### Ping a Riak Node [GET]
Performs an HTTP `riak ping` and returns the result.

+ Parameters
    + node_id: `riak1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).

+ Response 200 (text/html)

        OK

+ Response 404 (application/json)

        {"error":"Node is not running."}
