import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        return {
            clusterId: params.cluster_id
        };
    }
});
