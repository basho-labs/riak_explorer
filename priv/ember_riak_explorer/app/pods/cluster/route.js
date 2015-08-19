import Ember from 'ember';

export default Ember.Route.extend({
    actions: {
        error: function(errors, transition) {
            var error = errors.errors[0];
            if (error && error.status === "404") {
                this.transitionTo('error.cluster-not-found',
                    { queryParams:
                        { cluster_id: transition.params.cluster_id } });
            } else {
                // Unknown error, bubble error event up to routes/application.js
                return true;
            }
        }
    },

    model: function(params) {
        return this.store.findRecord('cluster', params.cluster_id);
    },

    setupController: function(controller, model) {
        this._super(controller, model);
        var clusterId = model.get('id');
        this.explorer.getIndexes(clusterId).then(function(indexes) {
            model.set('indexes', indexes);
        });
        this.explorer.getNodes(clusterId).then(function(nodes) {
            model.set('nodes', nodes);
        });
        this.store.query('bucket-type', {clusterId: clusterId}).then(function(bucketTypes) {
            model.set('bucketTypes', bucketTypes);
        });
    }
});
