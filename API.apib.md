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
Get a list of all the clusters listed in Explorer's `riak_explorer.conf` file.

+ Response 200 (application/json)

        {
            "clusters": [

                {
                    "id": "production1",
                    "development_mode": false,
                    "riak_node": "riak@192.168.0.1",
                    "riak_type": "ee"
                },
                {
                    "id": "qa1",
                    "development_mode": true,
                    "riak_node": "qa1@127.0.0.1",
                    "riak_type": "oss"
                },
                {
                    "id": "localdev",
                    "development_mode": true,
                    "riak_node": "riak@127.0.0.1",
                    "riak_type": "oss"
                }
            ],
            "links": {
                "self": "/explore/clusters/"
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
                "id": "localdev",
                "development_mode": true,
                "riak_node": "riak@127.0.0.1",
                "riak_type": "oss"
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
                    "id": "riak1@127.0.0.1"
                },
                {
                    "id": "riak2@127.0.0.1"
                },
                {
                    "id": "riak3@127.0.0.1"
                }
            ],
            "links": {
                "self": "/explore/clusters/localdev/nodes"
            }
        }

## Node [/explore/clusters/{cluster_id}/nodes/{node_id}]

### Retrieve a Node [GET]
Retrieves the properties of a single node within the specified cluster.

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `riak1@127.0.0.1` (string) - The Riak node id (in distributed
        Erlang node format).

+ Response 200 (application/json)

        {
            "node": {
                "id": "riak@127.0.0.1",
                "riak_type": "oss"
            },
            "links": {
                "self": "/explore/clusters/default/nodes/riak@127.0.0.1"
            }
        }

## Node Config [/explore/clusters/{cluster_id}/nodes/{node_id}/config]

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

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `riak1@127.0.0.1` (string) - The Riak node id (in distributed
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
                "self": "/explore/clusters/default/nodes/dev1@127.0.0.1/config"
            }
        }

+ Response 404 (application/json)

        {
            "error": "Invalid node id or node not available."
        }

## Bucket Types Collection [/explore/clusters/{cluster_id}/bucket_types]

### List all Bucket Types [GET]
Return all bucket types that exist on the cluster, both active and inactive.
Equivalent to running `riak-admin bucket-type list` on the command line.
Also includes each bucket type's properties hash (the equivalent of
`riak-admin bucket-type status $type`).

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
    + bucket_type_id: `default` (string) - The Riak name of the bucket type,
        which serves as a unique id within that cluster.

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

### Edit a Bucket Type [PUT]

+ Request (application/json)
+ Response 200 (application/json)

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
