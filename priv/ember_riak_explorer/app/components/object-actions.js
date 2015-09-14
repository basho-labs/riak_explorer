import Ember from 'ember';

export default Ember.Component.extend({
  actions: {
    deleteObject: function(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
    }
  }
});
