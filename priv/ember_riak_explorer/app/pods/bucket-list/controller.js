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
        var self = this;
        console.log("Refreshing model %O", bucketList);
        self.get('explorer').getBucketList(bucketList.get('cluster'),
            bucketList.get('bucketType'), self.store)
            .then(function(updatedModel) {
                console.log('loaded bucket list: %O', updatedModel);
                self.set('model', updatedModel);
                if(!updatedModel.isLoaded) {
                    self.pollForModel(updatedModel, 3000);
                }
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
