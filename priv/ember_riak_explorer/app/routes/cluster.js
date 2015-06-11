import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: false
        }
    },

    model: function(params) {
        console.log('cid: ' +params.cluster_id);
        return this.explorer.getCluster(params.cluster_id, true);
    }
});
