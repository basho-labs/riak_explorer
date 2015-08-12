import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    // delay in milliseconds
    pollForModel: function(bucketList, delay) {
            var self = this;
            Ember.run.later(function() {
                console.log('controller: scheduling to refreshModel');
                self.refreshModel(bucketList);
            }, delay);
    },

    refreshModel: function(bucketList) {
        var clusterId = bucketList.get('cluster').get('clusterId');
        var bucketTypeId = bucketList.get('bucketTypeId');
        var self = this;

        self.get('explorer').getBucketList(clusterId, bucketTypeId, self.store)
            .then(function(updatedModel) {
                console.log('loaded bucket list: %O', updatedModel);
                updatedModel.then(function(data) {
                    self.set('model', data);
                });
            });
    },

    actions: {
        refreshBuckets: function(bucketList) {
            var clusterId = bucketList.get('cluster').get('clusterId');
            var bucketTypeId = bucketList.get('bucketTypeId');

            this.get('model').set('isLoaded', false);
            this.get('explorer').bucketCacheRefresh(clusterId, bucketTypeId);
            this.pollForModel(this.get('model'), 3000);
        }
    }
});
