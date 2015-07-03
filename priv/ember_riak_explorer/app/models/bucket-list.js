import DS from 'ember-data';

// BucketList object, used to list buckets for a selected bucket type.
export default DS.Model.extend({
    cluster: DS.attr(),
    bucketType: DS.attr(),

    // Number of buckets displayed on this page
    count: DS.attr('number', {defaultValue: 0}),

    // When was the cache created on the server side
    created: DS.attr(),

    buckets: DS.attr(),

    // Total number of buckets
    total: DS.attr('number', {defaultValue: 0})
});
