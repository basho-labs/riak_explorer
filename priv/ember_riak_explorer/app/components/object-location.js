import Ember from 'ember';

export default Ember.Component.extend({
    actions: {
        editCancel: function(object) {
            // Send its primary action to parent controller
            this.sendAction('action', object);
        }
    }
});
