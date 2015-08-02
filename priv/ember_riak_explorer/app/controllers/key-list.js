import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id'],
    cluster_id: null,
    bucket_type_id: null,
    bucket_id: null,

    // delay in milliseconds
    pollForModel: function(bucketList, delay) {
            var self = this;
            Ember.run.later(function() {
                self.refreshModel(bucketList);
            }, delay);
    },

    refreshModel: function(keyList) {
        var clusterId = keyList.get('bucket').get('clusterId');
        var bucketTypeId = keyList.get('bucket').get('bucketTypeId');
        var bucketId = keyList.get('bucket').get('bucketId');
        var self = this;

        self.get('explorer').getKeyList(clusterId, bucketTypeId, bucketId, self.store)
            .then(function(updatedModel) {
                updatedModel.then(function(data) {
                    self.set('model', data);
                });
            });
    },

    actions: {
        deleteBucket: function(bucket) {
            this.get('model').set('isLoaded', false);
            this.get('explorer').deleteBucket(bucket);
            // Reload the model after the delete, triggers a cache refresh
            this.pollForModel(this.get('model'), 5000);
            // Reload the second time
            this.pollForModel(this.get('model'), 10000);
        },

        refreshKeys: function(keyList) {
            var clusterId = keyList.get('bucket').get('clusterId');
            var bucketTypeId = keyList.get('bucket').get('bucketTypeId');
            var bucketId = keyList.get('bucket').get('bucketId');

            this.get('model').set('isLoaded', false);
            this.get('explorer').keyCacheRefresh(clusterId, bucketTypeId, bucketId);
            this.pollForModel(this.get('model'), 3000);
        }
    }
});
