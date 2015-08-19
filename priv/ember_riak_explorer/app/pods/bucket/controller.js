import Ember from 'ember';

var BucketController = Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    // delay in milliseconds
    pollForModel: function(bucket, delay) {
        var self = this;
        Ember.run.later(function() {
            self.refreshModel(bucket);
        }, delay);
    },

    refreshModel: function(bucket) {
        var self = this;
        self.get('explorer').getKeyList(bucket, self.store)
            .then(function(updatedKeyList) {
                bucket.set('keyList', updatedKeyList);
                if(!bucket.get('isKeyListLoaded')) {
                    self.pollForModel(bucket, 3000);
                }
            });
    },

    actions: {
        deleteBucket: function(bucket) {
            bucket.set('isKeyListLoaded', false);
            this.get('explorer').deleteBucket(bucket);
            // Reload the model after the delete, triggers a cache refresh
            this.pollForModel(bucket, 5000);
            // Reload the second time
            this.pollForModel(bucket, 10000);
        },

        refreshKeys: function(bucket) {
            var clusterId = bucket.get('clusterId');
            var bucketTypeId = bucket.get('bucketTypeId');
            var bucketId = bucket.get('bucketId');

            bucket.set('isKeyListLoaded', false);
            this.get('explorer').keyCacheRefresh(clusterId, bucketTypeId, bucketId);
            this.pollForModel(bucket, 3000);
        }
    }
});

export default BucketController;
