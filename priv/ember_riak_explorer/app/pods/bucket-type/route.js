import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var clusterId = params.clusterId;
        var bucketTypeId = params.bucketTypeId;
        var explorer = this.explorer;
        var store = this.store;

        return this.explorer.getBucketType(clusterId, bucketTypeId, store)
            .then(function(bucketType) {
                return explorer.getBucketTypeWithList(bucketType,
                    bucketType.get('cluster'), store);
            });
    },

    setupController: function(controller, model) {
        this._super(controller, model);

        if(!model.get('isBucketListLoaded')) {
            console.log('Model not loaded. Polling..');
            controller.pollForModel(model, 3000);
        } else {
            console.log('Model loaded.');
        }
    }
});
