import Ember from 'ember';

export default Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    actions: {
        deleteObject: function(object) {
            this.get('explorer').deleteObject(object);
            this.get('explorer').markDeletedKey(object);

            // Once the delete has been issued,
            // return to the bucket's Key List view.
            this.transitionToRoute('bucket', object.get('bucket'));
        }
    }
});
