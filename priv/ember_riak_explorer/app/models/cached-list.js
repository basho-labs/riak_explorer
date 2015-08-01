import DS from 'ember-data';

// A cached list loaded from the API (bucket list or key list)
export default DS.Model.extend({

    // Number of items displayed on the current page
    count: DS.attr('number', {defaultValue: 0}),

    // When was the cache created on the server side
    created: DS.attr(),

    // Is the List operation waiting for a cache to be generated?
    isLoaded: DS.attr('boolean', {defaultValue: false}),

    // Total number of items in the list
    total: DS.attr('total', {defaultValue: 0})
});
