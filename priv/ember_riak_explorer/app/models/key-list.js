import DS from 'ember-data';

export default DS.Model.extend({
    cluster: DS.attr(),
    bucket: DS.attr(),

    // Number of keys displayed on this page
    count: DS.attr('number', {defaultValue: 0}),

    // When was the cache created on the server side
    created: DS.attr(),

    keys: DS.attr(),

    // Total number of keys per bucket
    total: DS.attr('number', {defaultValue: 0})
});
