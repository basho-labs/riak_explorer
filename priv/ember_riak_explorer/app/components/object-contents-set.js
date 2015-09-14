import Ember from 'ember';

export default Ember.Component.extend({
  elementToAdd: null,

  actions: {
    addElement: function(model) {
        this.sendAction('addElement', model, this.get('elementToAdd'));
        this.set('elementToAdd', null);  // Reset text box
    },

    deleteObject: function(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
    },

    removeElement: function(model, item) {
        // Send its action to parent controller
        this.sendAction('removeElement', model, item);
    }
  }
});
