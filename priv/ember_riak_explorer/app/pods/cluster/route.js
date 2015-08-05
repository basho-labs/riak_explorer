import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: false
        }
    },

    model: function(params) {
        return this.explorer.getClusterInfo(params.cluster_id, this.store);
    }
});
