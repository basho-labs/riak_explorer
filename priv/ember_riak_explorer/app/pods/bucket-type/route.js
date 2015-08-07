import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var cluster = this.modelFor('cluster');
        
        return this.explorer.getBucketType(clusterId,
            params.bucket_type_id, this.store);
    }
});
