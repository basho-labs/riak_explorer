import RiakObject from "../model";

var RiakSetObject = RiakObject.extend({
    /**
    Adds a given element to the set's contents.
    @param {String} item Element to be added
    */
    addElement: function(item) {
        if(!item) {
            return;
        }
        var set = this.get('contents').value;
        set.push(item);
        this.set('contents', {value: set});
    },

    /**
    Can this object type be edited directly, in a text box?
    @return {Boolean}
    */
    canBeEdited: function() {
        return false;
    }.property(),

    /**
    Can this object be viewed/downloaded directly from the browser?
    @return {Boolean}
    */
    canBeViewedRaw: function() {
        return false;
    }.property(),

    contentsForDisplay: function() {
        return this.get('contents').value;
    }.property('contents'),

    /**
    Removes a given element from the set's contents.
    @param {String} item Element to be removed
    */
    removeElement: function(item) {
        var set = this.get('contents').value;
        var index = set.indexOf(item);
        if (index > -1) {
            set.splice(index, 1);  // Remove item
        }
        this.set('contents', {value: set});
    }
});
export default RiakSetObject;
