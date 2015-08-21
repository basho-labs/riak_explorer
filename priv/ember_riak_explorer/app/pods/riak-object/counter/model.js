import DS from 'ember-data';
import RiakObject from "../model";

var RiakCounterObject = RiakObject.extend({
    canBeEdited: function() {
        return false;
    }.property(),

    canBeViewedRaw: function() {
        return false;
    }.property(),

    contentsForDisplay: function() {
        return this.get('contents').value;
    }.property('contents'),

    increment: function(amount) {
        var newValue = this.get('contents').value + amount;
        this.set('contents', {value: newValue});
    },
    decrement: function(amount) {
        var newValue = this.get('contents').value - amount;
        this.set('contents', {value: newValue});
    },
    decrementBy: DS.attr('integer', {defaultValue: 1}),

    incrementBy: DS.attr('integer', {defaultValue: 1})
});
export default RiakCounterObject;
