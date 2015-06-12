import Ember from 'ember';

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
        if(include_nodes) {
            nodes = getNodes(cluster_id);
            // Since we have the nodes, might as well grab indexes
            indexes = nodes.then(function(nodes) {
                if(!nodes) {
                    return [];
                }
                return getIndexes(nodes[0].id);
            });
        }
        return result.then(
            // Success
            function(data) {
                var cluster = {
                    id: data.cluster.id,
                    props: data.cluster.props,
                    nodes: nodes,
                    indexes: indexes
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
    }
});
