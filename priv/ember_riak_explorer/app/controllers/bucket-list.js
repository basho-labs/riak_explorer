import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    queryParams: ['cluster_id', 'bucket_type_id'],
    cluster_id: null,
    bucket_type_id: null,

    // delay in milliseconds
    pollForModel: function(bucketList, delay) {
            var self = this;
            Ember.run.later(function() {
                self.refreshModel(bucketList);
            }, delay);
    },

    refreshModel: function(bucketList) {
        var clusterId = bucketList.get('cluster').get('clusterId');
        var bucketTypeId = bucketList.get('bucketTypeId');
        var self = this;

        self.get('explorer').getBucketList(clusterId, bucketTypeId, self.store)
            .then(function(updatedModel) {
                updatedModel.then(function(data) {
                    self.set('model', data);
                });
            });
    },

    actions: {
        deleteBucket: function(bucket) {
            this.get('explorer').deleteBucket(bucket);
            // this.get('explorer').markDeletedBucket(bucket);

            this.pollForModel(this.get('model'), 100);
        },
        refreshBuckets: function(bucketList) {
            var clusterId = bucketList.get('cluster').get('clusterId');
            var bucketTypeId = bucketList.get('bucketTypeId');

            this.get('model').set('isLoaded', false);
            this.get('explorer').bucketCacheRefresh(clusterId, bucketTypeId);
            this.pollForModel(this.get('model'), 3000);
        }
    }
});
