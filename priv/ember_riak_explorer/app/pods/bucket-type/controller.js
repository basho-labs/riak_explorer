import Ember from 'ember';

var BucketTypeController = Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    // delay in milliseconds
    pollForModel: function(bucketType, delay) {
        var self = this;
        Ember.run.later(function() {
            // console.log('controller: scheduling to refreshModel');
            self.refreshModel(bucketType);
        }, delay);
    },

    refreshModel: function(bucketType) {
        var self = this;
        // console.log("Refreshing model %O", bucketType);
        self.get('explorer').getBucketList(bucketType.get('cluster'),
            bucketType, self.store)
            .then(function(updatedBucketList) {
                // console.log('loaded bucket list: %O', updatedBucketList);
                var model = self.get('model');
                model.set('bucketList', updatedBucketList);
                if(!model.get('isBucketListLoaded')) {
                    self.pollForModel(model, 3000);
                }
            });
    },

    actions: {
        refreshBuckets: function(bucketType) {
            var clusterId = bucketType.get('clusterId');
            var bucketTypeId = bucketType.get('bucketTypeId');

            this.get('model').set('isBucketListLoaded', false);
            this.get('explorer').bucketCacheRefresh(clusterId, bucketTypeId);
            this.pollForModel(this.get('model'), 3000);
        }
    }
});

export default BucketTypeController;
