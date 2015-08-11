import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var store = this.store;
        var explorer = this.explorer;
        return store.findRecord('cluster', params.clusterId).then(function(cluster) {
            return store.query('bucket-type', {clusterId: params.clusterId,
                bucketTypeId: params.bucketTypeId})
            .then(function(bucketType) {
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
            controller.pollForModel(model, 3000);
        }
    }
});
