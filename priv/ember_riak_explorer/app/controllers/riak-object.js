import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id', 'object_key'],
    cluster_id: null,
    bucket_type_id: null,
    bucket_id: null,
    object_key: null,

    actions: {
        deleteObject: function(cluster_id, bucket_type_id, bucket_id, object_key) {
            this.get('explorer').deleteObject(cluster_id, bucket_type_id, bucket_id, object_key);
        }
    }
});
