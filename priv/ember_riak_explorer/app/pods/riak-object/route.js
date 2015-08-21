import Ember from 'ember';

export default Ember.Route.extend({
    actions: {
        error: function(error, transition) {
            if (error && error.status === 404) {
                transition.queryParams = transition.params['riak-object'];
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
    },

    setupController: function(controller, model) {
        this._super(controller, model);
        if(!model.get('isLoaded')) {
            this.explorer.getRiakObject(model.get('clusterId'),
                    model.get('bucketTypeId'), model.get('bucket'),
                    model.get('key'), this.store)
                .then(function(object) {
                    controller.set('model', object);
                });
        }
    }
});
