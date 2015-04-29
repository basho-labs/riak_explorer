import Ember from 'ember';

export default Ember.Route.extend({
    // Since this is a simple /explore/ping, we're going to inline the model
    model: function() {
        var url = EmberENV.explorerHost
            + ':' + EmberENV.explorerPort + '/explore/ping';
        var serviceName = 'Riak Explorer';

        var pingResult = Ember.$.ajax({ url: url });  // returns a Promise obj
        return pingResult.then(
            // Success
            function(data) {
                return {
                    service: serviceName,
                    pingResult: 'Available (' + data.message + ')'
                };
            },
            // Error
            function(error) {
                return {
                    service: serviceName,
                    pingResult: 'Unavailable. Error encountered: ' + error.message
                };
            }
        );
    }
});
