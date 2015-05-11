import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var bucketTypes = this.store.find('bucket_type', { cluster_id: params.cluster_id });
        return {
            cluster_id: params.cluster_id,
            bucketTypes: bucketTypes
        };
    }
});
