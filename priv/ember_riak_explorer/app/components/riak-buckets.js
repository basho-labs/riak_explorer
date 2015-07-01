import Ember from 'ember';

export default Ember.Component.extend({
        actions: {
            deleteBucket: function(bucket) {
                // Send the action to parent controller
                this.sendAction('deleteBucketAction', bucket);
            }
        }
});
