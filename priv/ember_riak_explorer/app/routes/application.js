import Ember from 'ember';

export default Ember.Route.extend({
    actions: {
        error: function(error) {
            // An error has occurred that wasn't handled by any route.
            console.log('Unknown error: %O', error);
            this.transitionTo('errors.unknown');
        }
    },

    // Load the list of available clusters, for the left nav
    model: function() {
        return this.store.findAll('cluster');
    }
});
