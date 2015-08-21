import Ember from 'ember';

export default Ember.Component.extend({
    actions: {
        refreshKeys: function(keyList) {
            // Send its primary action to parent controller
            this.sendAction('action', keyList);
        }
    }
});
