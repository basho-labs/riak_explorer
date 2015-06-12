import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        node_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var url = '/riak/nodes/' + params.node_id + '/ping';

        var result = Ember.$.ajax({ url: url });  // returns a Promise obj
        return result.then(
            // Success
            function(data) {
                return {
                    message: 'Available (' + data + ')'
                };
            },
            // Error
            function(error) {
                return {
                    message: 'Unavailable. Error encountered: ' + error.message
                };
            }
        ).fail(function(error) {
            return {
                message: 'Unavailable. Error encountered: ' + error.message
            };
        });
    }
});
