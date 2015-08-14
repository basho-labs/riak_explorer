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
