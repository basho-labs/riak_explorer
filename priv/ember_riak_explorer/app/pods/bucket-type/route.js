import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var store = this.store;
        return store.findRecord('cluster', params.clusterId).then(function(cluster) {
            

            return store.queryRecord('bucket-type', {clusterId: params.clusterId,
                bucketTypeId: params.bucketTypeId}).then(function(model) {
                    return model;
                });
        });


        // return store.findRecord('cluster', params.clusterId).then(function(cluster) {
        //     return store.queryRecord('bucket-type', {clusterId: params.clusterId,
        //         bucketTypeId: params.bucketTypeId}).then(function(model) {
        //             console.log("cluster: %O", cluster);
        //             model.set('cluster', cluster);
        //             return model;
        //         });
        // });
    },

    setupController: function(controller, model) {
        this._super(controller, model);
        // console.log("model: %O", model);
        // this.store.findRecord('cluster', model.get('clusterId')).then(function(cluster) {
        //     model.set('cluster', cluster);
        // });
        // this.store.queryRecord('bucket-type', {clusterId: model.clusterId,
        //         bucketTypeId: params.bucketTypeId}).then(function(model) {
        //     model.set('bucketType', bucketType);
        // });
    }
});
