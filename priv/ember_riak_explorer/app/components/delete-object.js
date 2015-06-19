import Ember from 'ember';

export default Ember.Component.extend({
    actions: {
        deleteObject: function(object) {
            // Send its primary action to riak-keys component
            this.sendAction('action', object);
        }
    },
});
