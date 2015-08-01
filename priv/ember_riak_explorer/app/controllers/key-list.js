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
            console.log("Polling...");
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
