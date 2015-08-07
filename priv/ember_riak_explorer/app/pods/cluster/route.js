import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var cluster = this.store.findRecord('cluster', params.cluster_id);

        return cluster;
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
        this.store.query('cluster.bucket-type', {clusterId: clusterId}).then(function(bucketTypes) {
            model.set('bucketTypes', bucketTypes);
        });
    }
});
