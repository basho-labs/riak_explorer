import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id', 'object_key'],
    cluster_id: null,
    bucket_type_id: null,
    bucket_id: null,
    object_key: null,

    actions: {
        saveObject: function(object) {
            this.get('explorer').saveObject(object);

            this.transitionToRoute('riak-object',
                { queryParams: {
                    cluster_id: object.get('clusterId'),
                    bucket_type_id: object.get('bucketTypeId'),
                    bucket_id: object.get('bucketId'),
                    object_key: object.get('key')
                }});
        }
    }
});
