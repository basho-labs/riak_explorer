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
        },
        object_key: {
            refreshModel: true
        }
    },

    model: function(params) {
        return {
            clusterId: params.cluster_id,
            bucketTypeId: params.bucket_type_id,
            bucketId: params.bucket_id,
            key: params.object_key
        };
    }
});
