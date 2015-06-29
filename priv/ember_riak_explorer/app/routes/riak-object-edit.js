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
            return this.explorer.getRiakObject(params.cluster_id,
                params.bucket_type_id, params.bucket_id, params.object_key, this.store);
        }
});
