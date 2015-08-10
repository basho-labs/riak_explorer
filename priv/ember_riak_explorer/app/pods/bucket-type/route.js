import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        // var id = this.explorer.compositeId(params.clusterId,
        //     params.bucketTypeId);
        return this.store.query('bucket-type', {clusterId: params.clusterId,
            bucketTypeId: params.bucketTypeId});
    },

    setupController: function(controller, model) {
        this._super(controller, model);
    }
});
