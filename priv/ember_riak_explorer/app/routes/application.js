import Ember from 'ember';

export default Ember.Route.extend({
    actions: {
        error: function(error) {
            // An error has occurred that wasn't handled by any route.
            this.transitionTo('errors.unknown');
        }
    },

    model: function() {
        return this.explorer.getClusters();
    }
});
