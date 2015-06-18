import Ember from 'ember';

export default Ember.Component.extend({
    actions: {
        deleteObject: function(cluster_id, bucket_type_id, bucket_id, object_key) {
            // Send its primary action to riak-keys component
            this.sendAction('action', cluster_id, bucket_type_id, bucket_id, object_key);
        }
    },
});
