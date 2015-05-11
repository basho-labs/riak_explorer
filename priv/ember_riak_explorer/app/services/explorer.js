import Ember from 'ember';

export default Ember.Service.extend({
    getClusters: function() {
        var url = '/explore/clusters/';
        var result = Ember.$.ajax({ url: url });  // returns a Promise obj
        return result.then(
            // Success
            function(data) {
                return data;
            },
            // Error
            function(error) {
                console.log('Error fetching clusters: ' + error);
                return {
                    clusters: []
                };
            }
        );
    },

    getNodes: function(cluster_id) {
        var url = '/explore/clusters/'+ cluster_id + '/nodes';
        var result = Ember.$.ajax({ url: url });  // returns a Promise obj
        return result.then(
            // Success
            function(data) {
                return data;
            },
            // Error
            function(error) {
                console.log('Error fetching nodes: ' + error);
                return {
                    nodes: []
                };
            }
        );
    }
});
