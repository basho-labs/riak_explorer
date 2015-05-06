import Ember from 'ember';

export default Ember.Route.extend({

    model: function() {
        var url = '/explore/clusters/default/nodes';

        // hardcode for testing:
        // return {
        //     cluster_nodes: [ "dev1@127.0.0.1" ]
        // };

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
