import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id'],
    cluster_id: null,
    bucket_type_id: null,
    bucket_id: null,

    actions: {
        refreshKeys: function(keyList) {
            this.get('explorer').keyCacheRefresh(keyList);

            this.transitionToRoute('key_list',
                { queryParams: {
                    cluster_id: keyList.get('bucket').get('clusterId'),
                    bucket_type_id: keyList.get('bucket').get('bucketTypeId'),
                    bucket_id: keyList.get('bucket').get('bucketId')
                }});
        }
    }
});
