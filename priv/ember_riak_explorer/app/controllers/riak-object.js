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
            this.get('explorer').markDeletedKey(object);

            // Once the delete has been issued,
            // return to the bucket's Key List view.
            this.transitionToRoute('bucket', object.get('bucket'));
        }
    }
});
