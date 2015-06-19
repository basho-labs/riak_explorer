import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id', 'object_key'],
    cluster_id: null,
    bucket_type_id: null,
    bucket_id: null,
    object_key: null,

    actions: {
        deleteObject: function(object) {
            this.get('explorer').deleteObject(object);
            
            // Once the delete has been issued,
            // return to the bucket's Key List view.
            this.transitionToRoute('key_list',
                { queryParams: {
                    cluster_id: object.get('bucket').get('clusterId'),
                    bucket_type_id: object.get('bucket').get('bucketTypeId'),
                    bucket_id: object.get('bucket').get('name')
                }});
        }
    }
});
