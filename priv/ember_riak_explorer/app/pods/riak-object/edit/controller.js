import Ember from 'ember';

var RiakObjectEditController = Ember.Controller.extend({
    explorer: Ember.inject.service('explorer'),

    actions: {
        saveObject: function(object) {
            this.get('explorer').saveObject(object);
            object.set('isLoaded', false);
            this.transitionToRoute('riak-object', object);
        }
    }
});
export default RiakObjectEditController;
