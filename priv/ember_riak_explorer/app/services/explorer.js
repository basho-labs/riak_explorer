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
