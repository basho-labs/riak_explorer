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
        }
    },

    model: function(params) {
        return this.explorer.getKeyList(params.cluster_id, params.bucket_type_id,
            params.bucket_id, this.store);
    },

    setupController: function(controller, model) {
        this._super(controller, model);
        if(!model.get('isLoaded')) {
            controller.pollForModel(model, 3000);
        }
    }
});
