import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: true
        },
        bucket_type_id: {
            refreshModel: true
        },
        bucket_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        return this.explorer.getBucketProps(params.cluster_id,
            params.bucket_type_id, params.bucket_id, this.store);
    }
});
