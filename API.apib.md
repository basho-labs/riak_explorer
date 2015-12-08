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

+ Response 200 (application/json)

        {
            "clusters": [
                {
                    "development_mode": false,
                    "id": "production1",
                    "riak_node": "riak@192.168.0.1",
                    "riak_type": "ee"
                },
                {
                    "development_mode": true,
                    "id": "localdev",
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

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id as defined in the 
        `riak_explorer.conf` file.

+ Response 200 (application/json)

        {
            "cluster": {
                "development_mode": true,
                "id": "localdev",
                "riak_node": "riak@127.0.0.1",
                "riak_type": "oss"
            },
            "links": {
                "self": "/explore/clusters/localdev"
            }
        }

+ Response 404

## Nodes Collection [/explore/clusters/{cluster_id}/nodes]

### List all Nodes [GET]

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

+ Response 200

## Node [/explore/clusters/{cluster_id}/nodes/{node_id}]

### Retrieve a Node [GET]

+ Parameters
    + cluster_id: `localdev` (string) - The Cluster id.
    + node_id: `riak1@127.0.0.1` (string) - The Riak node id (in distributed 
        Erlang node format).

+ Response 200 (application/json)

## Bucket Types Collection [/explore/clusters/{cluster_id}/bucket_types]

### List all Bucket Types [GET]

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
