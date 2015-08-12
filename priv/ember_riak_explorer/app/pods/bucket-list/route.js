import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var store = this.store;
        var explorer = this.explorer;
        return store.findRecord('cluster', params.clusterId).then(function(cluster) {
            console.log('found cluster: %O', cluster);
            return store.queryRecord('bucket-type', {clusterId: params.clusterId,
                bucketTypeId: params.bucketTypeId})
            .then(function(bucketType) {
                console.log('found buckettype: %O', bucketType);
                return explorer.getBucketList(cluster, bucketType, store);
            });
        });


        // var store = this.store;
        // return this.explorer.getBucketList(params.clusterId,
        //     params.bucketTypeId, this.store).then(function(model) {
        //         console.log('in model: %O', model);
        //         // model.set('cluster', store.findRecord('cluster', params.clusterId));
        //         // model.set('bucketType', store.query('bucket-type', {clusterId: params.clusterId}));
        //         return model;
        //     });
    },

    setupController: function(controller, model) {
        this._super(controller, model);

        if(!model.get('isLoaded')) {
            console.log('Model not loaded. Polling..');
            controller.pollForModel(model, 3000);
        } else {
            console.log('Model loaded.');
        }
    }
});
