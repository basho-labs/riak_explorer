import Ember from 'ember';

export default Ember.Route.extend({
    // setupController: function(controller) {
    //     controller.set('clusters', this.explorer.getClustersAndNodes() );
    // }

    model: function() {
        return this.explorer.getClusters();
    }
});
