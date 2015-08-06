import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var clusterId = this.modelFor('cluster').get('id');
        return this.explorer.getBucketType(clusterId,
            params.bucket_type_id, this.store);
    }
});
