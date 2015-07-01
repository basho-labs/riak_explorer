import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: true
        },
        bucket_type_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        return this.explorer.getBucketList(params.cluster_id,
            params.bucket_type_id, this.store);
    }
});
