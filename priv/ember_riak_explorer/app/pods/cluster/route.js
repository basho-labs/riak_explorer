import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var clusterId = params.cluster_id;
        var cluster = this.store.findRecord('cluster', clusterId);
        this.explorer.getIndexes(clusterId).then(function(indexes) {
            cluster.set('indexes', indexes);
        });
        this.explorer.getNodes(clusterId).then(function(nodes) {
            cluster.set('nodes', nodes);
        });

        return cluster;
    }
});
