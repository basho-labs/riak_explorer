import Ember from 'ember';
import RiakObjectController from "../controller";

var RiakObjectSetController = RiakObjectController.extend({
    actions: {
        /**
        Adds element from the textbox to the set.
        @param {RiakSetObject} model
        @param {String} newItem Element to be added
        */
        addElement: function(model, newItem) {
            this.get('explorer').updateSet(model, newItem, 'addElement');

            model.addElement(newItem);
        },

        /**
        Polls the server to refresh the model
        (kicks off a delayed call to +refreshModel+)
        @param {RiakSetObject} model
        @param {Integer} delay Delay in milliseconds
        */
        pollForModel: function(model, delay) {
            var self = this;
            Ember.run.later(function() {
                self.refreshModel(model);
            }, delay);
        },

        /**
        Reloads the model from the server, updates the controller with it.
        @param {RiakSetObject} model
        */
        refreshModel: function(model) {
            var controller = this;
            controller.get('explorer').getRiakObject(model.get('bucket'),
                    model.get('key'), controller.store)
                .then(function(model) {
                    controller.set('model', model);
                });
        },

        /**
        Removes specified element from the set.
        @param {RiakSetObject} model
        @param {String} item Element to be removed
        */
        removeElement: function(model, item) {
            this.get('explorer').updateSet(model, item, 'remove');

            model.removeElement(item);
        }
    }
});
export default RiakObjectSetController;
