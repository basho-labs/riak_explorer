import Ember from 'ember';
import RiakObjectController from "../controller";

var RiakObjectCounterController = RiakObjectController.extend({
    actions: {
        incrementCounter: function(object) {
            this.get('explorer').updateCounter(object, 'increment');

            object.increment(object.get('incrementBy'));
        },
        decrementCounter: function(object) {
            this.get('explorer').updateCounter(object, 'decrement');

            object.decrement(object.get('decrementBy'));
        },
        // delay in milliseconds
        pollForModel: function(object, delay) {
            var self = this;
            Ember.run.later(function() {
                self.refreshModel(object);
            }, delay);
        },

        refreshModel: function(object) {
            var controller = this;
            controller.get('explorer').getRiakObject(object.get('bucket'),
                    object.get('key'), controller.store)
                .then(function(object) {
                    controller.set('model', object);
                });
        }
    }
});
export default RiakObjectCounterController;
