import Ember from 'ember';

export default Ember.Component.extend({
actions: {
    deleteObject: function(object) {
        // Send action to riak-object controller
        this.sendAction('deleteObject', object);
    },

    saveObject: function(object) {
        // Send its primary action to riak-object-edit controller
        this.sendAction('saveObject', object);
    }
}
});
