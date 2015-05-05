import Ember from 'ember';

export default Ember.Route.extend({

    model: function() {
        // hardcode:
        // return {cluster_nodes: ["dev1@127.0.0.1", "dev2@127.0.0.1"]};

        var url = '/explore/clusters/default/nodes';

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
                    cluster_nodes: []
                };
            }
        );
    }
});
