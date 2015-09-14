import Ember from 'ember';

export default Ember.Component.extend({
    tagName: 'span',

    actions: {
        removeElement: function(model, item) {
            // Send its action to parent controller
            this.sendAction('removeElement', model, item);
        }
    }
});
