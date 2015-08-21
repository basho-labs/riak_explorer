import Ember from 'ember';

var RiakObjectController = Ember.Controller.extend({
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
export default RiakObjectController;
