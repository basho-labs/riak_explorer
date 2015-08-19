import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var explorer = this.explorer;
        var store = this.store;
        return explorer.getBucket(params.clusterId,
                params.bucketTypeId, params.bucketId, store)
            .then(function(bucket) {
                return explorer.getBucketWithKeyList(bucket, store);
            });
    },

    setupController: function(controller, model) {
        this._super(controller, model);
        // When user follows a bucket link from the Bucket Type view,
        //   the props are not yet initialized. Also, the model()
        //   function, above, is not called. Handle this case.
        if(Ember.isEmpty(model.get('props'))) {
            this.explorer.getBucketProps(model.get('clusterId'),
                    model.get('bucketTypeId'), model.get('bucketId'), this.store)
                .then(function(bucketProps) {
                    model.set('props', bucketProps);
                });
        }
        // Start fetching the key list
        if(!model.get('isKeyListLoaded')) {
            controller.pollForModel(model, 3000);
        }
    }
});
