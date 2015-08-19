import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        console.log('in route');
        return this.explorer.getBucket(params.clusterId,
            params.bucketTypeId, params.bucketId, this.store);
    },

    setupController: function(controller, model) {
        this._super(controller, model);
        if(Ember.isEmpty(model.get('propsList'))) {
            this.explorer.getBucketProps(model.get('clusterId'),
                    model.get('bucketTypeId'), model.get('bucketId'))
                .then(function(bucketProps) {
                    model.set('props', bucketProps);
                });
        }
    }
});
