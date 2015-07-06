import DS from 'ember-data';

export default DS.Model.extend({
    bucket: DS.attr(),
    cluster: DS.attr(),

    // Number of keys displayed on this page
    count: DS.attr('number', {defaultValue: 0}),

    // When was the cache created on the server side
    created: DS.attr(),

    // Is the List operation waiting for a cache to be generated?
    isLoading: DS.attr('boolean', {defaultValue: true}),

    // List of riak-object model instances
    keys: DS.attr(),

    // Total number of keys per bucket
    total: DS.attr('number', {defaultValue: 0})
});
