import Ember from 'ember';

function displayContentsForType(headers, contents) {
    var contentType = headers.other['content-type'];
    var displayContents;
    // Determine whether this is browser-displayable contents
    if(contentType.startsWith('text') ||
        contentType === 'application/json' ||
        contentType === 'application/xml' ||
        contentType.startsWith('multipart/mixed') ) {
        displayContents = contents;
    }
    return displayContents;
}

/**
* XmlHttpRequest's getAllResponseHeaders() method returns a string of response
* headers according to the format described here:
* http://www.w3.org/TR/XMLHttpRequest/#the-getallresponseheaders-method
*
* Which we then have to parse. Like savages.
*/
function parseHeaderString(headerString) {
    var other_headers = {};
    var indexes = [];
    var custom = [];

    if (!headerString) {
      return {
          custom: [],     // x-riak-meta-*
          indexes: [],    // x-riak-index-*
          other: {}       // everything else
      };
    }
    var headerLines = headerString.split("\r\n");

    for (var i = 0; i < headerLines.length; i++) {
        var headerLine = headerLines[i];

        // Can't use split() here because it does the wrong thing
        // if the header value has the string ": " in it.
        var index = headerLine.indexOf(': ');
        if (index > 0) {
          var key = headerLine.substring(0, index).toLowerCase();
          var val = headerLine.substring(index + 2);
          var header = {
              key: key,
              value: val
          };

          if(key.startsWith('x-riak-meta')) {
              custom.push(header);
          } else if(key.startsWith('x-riak-index')) {
              indexes.push(header);
          } else {
              other_headers[key] = val;
          }
        }
    }
    return {
        other: other_headers,
        indexes: indexes,
        custom: custom
    };
}

function objectFromAjax(key, bucket, rawHeader, responseText, store) {
    var headers = parseHeaderString(rawHeader);
    var contents = displayContentsForType(headers, responseText);

    return store.createRecord('riak-object', {
        key: key,
        bucket: bucket,
        headers: headers,
        contents: contents
    });
}

function deleteBucket(bucket) {
    var url = '/explore/clusters/' + bucket.get('clusterId') +
        '/bucket_types/' + bucket.get('bucketTypeId') +
        '/buckets/' + bucket.get('bucketId');

    var request = new Ember.RSVP.Promise(function(resolve, reject) {
        Ember.$.ajax({
            type: "DELETE",
            url: url
        }).then(
            function(data, textStatus, jqXHR) {
                resolve(jqXHR.status);
            },
            function(jqXHR, textStatus) {
                reject(textStatus);
            }
        );
    });

    return request.catch(function(error) {
        console.log('Error deleting riak bucket: %O', error);
    });
}

function deleteObject(object) {
    var url = getClusterProxyUrl(object.get('clusterId')) + '/types/' +
            object.get('bucketTypeId') + '/buckets/' +
            object.get('bucketId') + '/keys/' + object.get('key');

    var request = new Ember.RSVP.Promise(function(resolve, reject) {
        Ember.$.ajax({
            type: "DELETE",
            url: url,
            headers: { 'X-Riak-Vclock': object.get('headers').other['x-riak-vclock'] }
        }).then(
            function(data, textStatus, jqXHR) {
                resolve(jqXHR.status);
            },
            function(jqXHR, textStatus) {
                reject(textStatus);
            }
        );
    });

    return request.catch(function(error) {
        console.log('Error deleting riak object: %O', error);
    });
}

function getClusterInfo(clusterId, includeNodes, store) {
    var url = '/explore/clusters/'+ clusterId;
    var result = Ember.$.ajax({ url: url });  // returns a Promise obj
    var nodes;
    var indexes;
    var clusterUrl = getClusterProxyUrl(clusterId);
    if(includeNodes) {
        nodes = getNodes(clusterId);

        // Since we have the nodes, might as well grab indexes etc
        indexes = nodes.then(function(nodes) {
            if(!nodes) {
                return [];
            }
            return getIndexes(clusterId);
        });
    }
    return result.then(
        // Success
        function(data) {
            var bucketTypes = store.find('bucket_type', { cluster_id: clusterId });
            var cluster = store.createRecord('cluster', {
                clusterId: data.cluster.id,
                developmentMode: data.cluster.development_mode,
                connectedNode: data.cluster.riak_node
            });
            var clusterInfo = {
                cluster: cluster,
                nodes: nodes,
                indexes: indexes,
                bucketTypes: bucketTypes,
                cluster_proxy_url: clusterUrl
            };

            return new Ember.RSVP.hash(clusterInfo);
        },
        // Error
        function(error) {
            console.log('Error fetching cluster: ' + error);
            return {};
        }
    );
}

function getClusters() {
    var url = '/explore/clusters/';
    var result = Ember.$.ajax({ url: url });  // returns a Promise obj
    return result.then(
        // Success
        function(data) {
            return data.clusters;
        },
        // Error
        function(error) {
            console.log('Error fetching clusters: ' + error);
            return [];
        }
    );
}

function getClustersAndNodes() {
    return getClusters().then(function(clustersList) {
        return Ember.RSVP.all(
            clustersList.map(function(cluster) {
                return new Ember.RSVP.hash({
                    id: cluster.id,
                    props: cluster.props,
                    nodes: getNodes(cluster.id)
                });
            })
        );
    });
}

function getClusterProxyUrl(clusterId) {
    return '/riak/clusters/'+clusterId;
}

function getIndexes(clusterId) {
    var url = getClusterProxyUrl(clusterId) + '/search/index';

    var request = new Ember.RSVP.Promise(function(resolve, reject) {
        Ember.$.ajax({
            type: "GET",
            url: url
        }).then(
            // Success
            function(data) {
                resolve(data);
            },
            // Error
            function(jqXHR, textStatus) {
                if(jqXHR.status === 404) {
                    // No indexes found, simply return an empty list
                    resolve([]);
                } else {
                    // Some other error
                    reject(textStatus);
                }
            }
        );
    });
    return request;
}

function getNodes(clusterId) {
    var url = '/explore/clusters/'+ clusterId + '/nodes';
    var result = Ember.$.ajax({ url: url });  // returns a Promise obj
    return result.then(
        // Success
        function(data) {
            return data.nodes;
        },
        // Error
        function(error) {
            console.log('Error fetching nodes: ' + error);
            return [];
        }
    );
}

function getRiakObject(clusterId, bucketTypeId, bucketId, key, store) {
    var url = getClusterProxyUrl(clusterId) + '/types/' + bucketTypeId + '/buckets/' +
           bucketId + '/keys/' + key;

    var bucket = store.createRecord('bucket', {
        name: bucketId,
        bucketTypeId: bucketTypeId,
        clusterId: clusterId
    });

    var request = new Ember.RSVP.Promise(function(resolve, reject) {
        Ember.$.ajax({
            type: "GET",
            processData: false,
            cache: false,
            url: url,
            headers: { 'Accept': '*/*, multipart/mixed' }
        }).then(
            function(data, textStatus, jqXHR) {
                var headerString = jqXHR.getAllResponseHeaders();
                resolve(objectFromAjax(key, bucket, headerString,
                    jqXHR.responseText, store));
            },
            function(jqXHR, textStatus) {
                var headerString;
                if(jqXHR.status === 200 && textStatus === 'parsererror') {
                    // jQuery tries to parse JSON objects, and throws
                    // parse errors when they're invalid. Suppress this.
                    headerString = jqXHR.getAllResponseHeaders();
                    resolve(objectFromAjax(key, bucket, headerString,
                        jqXHR.responseText, store));
                }
                if(jqXHR.status === 300) {
                    // Handle 300 Multiple Choices case for siblings
                    headerString = jqXHR.getAllResponseHeaders();
                    resolve(objectFromAjax(key, bucket, headerString,
                        jqXHR.responseText, store));
                } else {
                    reject(textStatus);
                }
            }
        );
    });

    return request.then(function(request) {
        return new Ember.RSVP.hash({
            obj: request,
            url: url
        });
    }).catch(function(error) {
        console.log('Error fetching riak object: %O', error);
    });
}

// Fetch the cache of Deleted keys/buckets for a
//  given cluster and bucket type. Initialize objects whenever missing.
function deletedCacheFor(clusterId, bucketTypeId) {
    if(!this.deleted.clusters[clusterId]) {
        this.deleted.clusters[clusterId] = { types: {} };
    }
    if(!this.deleted.clusters[clusterId].types[bucketTypeId]) {
        this.deleted.clusters[clusterId].types[bucketTypeId] = { buckets: {} };
    }
    return this.deleted.clusters[clusterId].types[bucketTypeId];
}

function markDeletedKey(object) {
    var clusterId = object.get('clusterId');
    var bucketTypeId = object.get('bucketTypeId');
    var bucketId = object.get('bucketId');
    var key = object.get('key');

    var bucketTypeDelCache = this.deletedCacheFor(clusterId, bucketTypeId);

    if(!bucketTypeDelCache.buckets[bucketId]) {
        bucketTypeDelCache.buckets[bucketId] = {
            keysDeleted: {},
            bucketDeleted: false
        };
    }

    bucketTypeDelCache.buckets[bucketId].keysDeleted[key] = true;
}

function wasKeyDeleted(object) {
    var clusterId = object.get('clusterId');
    var bucketTypeId = object.get('bucketTypeId');
    var bucketId = object.get('bucketId');
    var key = object.get('key');
    var bucketTypeDelCache = this.deletedCacheFor(clusterId, bucketTypeId);
    if(!bucketTypeDelCache.buckets[bucketId]) {
        return false;
    }
    return bucketTypeDelCache.buckets[bucketId].keysDeleted[key];
}

function keyCacheCreate(clusterId, bucketTypeId, bucketId) {
    var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId +
        '/buckets/' + bucketId + '/keys';

    var req = new Ember.RSVP.Promise(function(resolve, reject) {
        Ember.$.ajax({
            type: "GET",
            url: url
        }).then(
            function(data, textStatus, jqXHR) {
                resolve(jqXHR.status);
            },
            function(jqXHR, textStatus) {
                reject(textStatus);
            }
        );
    });

    return req;
}


function keyCacheDelete(clusterId, bucketTypeId, bucketId) {
    var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId +
        '/buckets/' + bucketId + '/keys';

    var req = new Ember.RSVP.Promise(function(resolve, reject) {
        Ember.$.ajax({
            type: "DELETE",
            url: url
        }).then(
            function(data, textStatus, jqXHR) {
                resolve(jqXHR.status);
            },
            function(jqXHR, textStatus) {
                reject(textStatus);
            }
        );
    });

    return req;
}

function keyCacheRefresh(keyList) {
    var bucket = keyList.get('bucket');
    var clusterId = bucket.get('clusterId');
    var bucketTypeId = bucket.get('bucketTypeId');
    var bucketId = bucket.get('bucketId');
    var service = this;

    service.keyCacheDelete(clusterId, bucketTypeId, bucketId).then(function() {
        service.keyCacheCreate(clusterId, bucketTypeId, bucketId);
    });
}

function saveObject(object) {
    var url = getClusterProxyUrl(object.get('clusterId')) + '/types/' +
            object.get('bucketTypeId') + '/buckets/' +
            object.get('bucketId') + '/keys/' + object.get('key');

    var request = new Ember.RSVP.Promise(function(resolve, reject) {
        Ember.$.ajax({
            type: "PUT",
            processData: false,
            contentType: object.get('contentType'),
            url: url,
            headers: object.get('headersForUpdate'),
            data: object.get('contents')
        }).then(
            function(data, textStatus, jqXHR) {
                resolve(jqXHR.status);
            },
            function(jqXHR, textStatus) {
                reject(textStatus);
            }
        );
    });

    return request.catch(function(error) {
        console.log('Error saving riak object: %O', error);
    });
}

export default Ember.Service.extend({
    name: 'explorer',
    availableIn: ['controllers', 'routes'],

    // Keep track of keys/buckets deleted through the Explorer UI
    deleted: {
        clusters: {}
    },

    markDeletedKey: markDeletedKey,
    deletedCacheFor: deletedCacheFor,
    wasKeyDeleted: wasKeyDeleted,

    deleteObject: deleteObject,
    deleteBucket: deleteBucket,

    // Return the details for a single cluster
    getClusterInfo: getClusterInfo,

    // Return all clusters that Explorer knows about
    getClusters: getClusters,

    // Return a list of clusters with their nodes, in the form:
    // [ { id: 'default', props: [], nodes: [ <list of nodes> ] }, ... ]
    getClustersAndNodes: getClustersAndNodes,

    getClusterProxyUrl: getClusterProxyUrl,

    // Return all nodes for a particular cluster
    getNodes: getNodes,

    getRiakObject: getRiakObject,

    keyCacheCreate: keyCacheCreate,
    keyCacheDelete: keyCacheDelete,
    keyCacheRefresh: keyCacheRefresh,

    saveObject: saveObject
});
