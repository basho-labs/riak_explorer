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
            var explorer = this.explorer;
            var store = this.store;
            return this.explorer.getBucket(params.cluster_id, params.bucket_type_id,
                params.bucket_id, store)
            .then(function(bucket) {
                return explorer.getRiakObject(params.cluster_id,
                    params.bucket_type_id, bucket, params.object_key, store);
            });
        }
});
