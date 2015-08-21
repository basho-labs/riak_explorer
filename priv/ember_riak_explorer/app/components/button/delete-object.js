import Ember from 'ember';

export default Ember.Component.extend({
    tagName: 'span',
    
    actions: {
        deleteObject: function(object) {
            // Send its primary action to riak-object controller
            this.sendAction('action', object);
        }
    }
});
