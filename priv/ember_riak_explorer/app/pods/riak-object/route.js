import Ember from 'ember';

export default Ember.Route.extend({
    actions: {
        error: function(error, transition) {
            if (error && error.status === 404) {
                this.transitionTo('error.object-not-found', transition);
            } else {
                // Unknown error, bubble error event up to routes/application.js
                return true;
            }
        }
    },

    model: function(params) {
        var clusterId = params.clusterId;
        var bucketTypeId = params.bucketTypeId;
        var bucketId = params.bucketId;
        var key = params.key;
        var explorer = this.explorer;
        var store = this.store;

        return explorer.getBucket(clusterId, bucketTypeId, bucketId, store)
            .then(function(bucket) {
                return explorer.getRiakObject(clusterId,
                    bucketTypeId, bucket, key, store);
            });
    }
});
