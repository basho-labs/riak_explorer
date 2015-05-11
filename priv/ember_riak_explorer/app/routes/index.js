import Ember from 'ember';

export default Ember.Route.extend({

    model: function() {

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
                    nodes: []
                };
            }
        );
    }
});
