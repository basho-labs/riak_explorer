import Ember from 'ember';

export default Ember.Component.extend({
    actions: {
        refreshBuckets: function(bucketList) {
            // Send its primary action to parent controller
            this.sendAction('action', bucketList);
        }
    }
});
