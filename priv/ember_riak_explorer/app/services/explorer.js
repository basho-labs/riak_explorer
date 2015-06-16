import Ember from 'ember';

/**
* XmlHttpRequest's getAllResponseHeaders() method returns a string of response
* headers according to the format described here:
* http://www.w3.org/TR/XMLHttpRequest/#the-getallresponseheaders-method
*
* Which we then have to parse. Like savages.
*/
function parseHeaderString(headerString) {
    var headers = {};
    if (!headerString) {
      return headers;
    }
    var headerLines = headerString.split("\r\n");
    var header;

    for (var i = 0; i < headerLines.length; i++) {
        header = headerLines[i];
        // Can't use split() here because it does the wrong thing
        // if the header value has the string ": " in it.
        var index = header.indexOf(': ');
        if (index > 0) {
          var key = header.substring(0, index).toLowerCase();
          var val = header.substring(index + 2);
          headers[key] = val;
        }
    }
    return headers;
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

function getClusterProxyUrl(cluster_id) {
    var nodes = getNodes(cluster_id);
    return nodes.then(function(nodes) {
        return '/riak/nodes/' + nodes[0].id;
    });
}

function getIndexes(node_id) {
    var url = '/riak/nodes/' + node_id + '/search/index';
    var result = Ember.$.ajax({ url: url });  // returns a Promise obj
    return result.then(
        // Success
        function(data) {
            return data;
        },
        // Error
        function(error) {
            console.log('Error fetching indexes: ' + error);
            return [];
        }
    );
}

function getNodes(cluster_id) {
    var url = '/explore/clusters/'+ cluster_id + '/nodes';
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

export default Ember.Service.extend({
    availableIn: ['controllers', 'routes'],

    // Return the details for a single cluster
    getCluster: function(cluster_id, include_nodes) {
        var url = '/explore/clusters/'+ cluster_id;
        var result = Ember.$.ajax({ url: url });  // returns a Promise obj
        var nodes;
        var indexes;
        var clusterUrl;
        if(include_nodes) {
            nodes = getNodes(cluster_id);

            // Since we have the nodes, might as well grab indexes etc
            indexes = nodes.then(function(nodes) {
                if(!nodes) {
                    return [];
                }
                return getIndexes(nodes[0].id);
            });
            clusterUrl = nodes.then(function(nodes) {
                if(!nodes) {
                    return null;
                }
                return '/riak/nodes/'+nodes[0].id;
            });
        }
        return result.then(
            // Success
            function(data) {
                var cluster = {
                    id: data.cluster.id,
                    props: data.cluster.props,
                    nodes: nodes,
                    indexes: indexes,
                    cluster_proxy_url: clusterUrl
                };

                return new Ember.RSVP.hash(cluster);
            },
            // Error
            function(error) {
                console.log('Error fetching cluster: ' + error);
                return {};
            }
        );
    },

    // Return all clusters that Explorer knows about
    getClusters: getClusters,

    // Return all nodes for a particular cluster
    getNodes: getNodes,

    // Return a list of clusters with their nodes, in the form:
    // [ { id: 'default', props: [], nodes: [ <list of nodes> ] }, ... ]
    getClustersAndNodes: function() {
        return getClusters().then(function(clusters_list) {
            return Ember.RSVP.all(
                clusters_list.map(function(cluster) {
                    return new Ember.RSVP.hash({
                        id: cluster.id,
                        props: cluster.props,
                        nodes: getNodes(cluster.id)
                    });
                })
            );
        });
    },

    getClusterProxyUrl: getClusterProxyUrl,

    getRiakObjectHeader: function(cluster_id, bucket_type_id, bucket_id, object_key) {
        var url = getClusterProxyUrl(cluster_id).then(function(proxyUrl) {
            return proxyUrl + '/types/' + bucket_type_id + '/buckets/' +
               bucket_id + '/keys/' + object_key;
        });

        var request = url.then(function(objUrl) {
            return Ember.$.ajax({
                type: "HEAD",
                async: true,
                url: objUrl
            }).then(function(message, text, jqXHR) {
                return parseHeaderString(jqXHR.getAllResponseHeaders());
            });
        });
        return request.then(function(request) {
            return new Ember.RSVP.hash({
                headers: request,
                cluster_id: cluster_id,
                bucket_type_id: bucket_type_id,
                bucket_id: bucket_id,
                object_key: object_key,
                url: url
            });
        });

    }
});
