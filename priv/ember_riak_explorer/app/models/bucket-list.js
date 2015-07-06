import DS from 'ember-data';

// BucketList object, used to list buckets for a selected bucket type.
export default DS.Model.extend({
    // List of Bucket model instances
    buckets: DS.attr(),

    // Bucket-type model instance
    bucketType: DS.attr(),

    // Cluster model instance
    cluster: DS.attr(),

    // Number of buckets displayed on this page
    count: DS.attr('number', {defaultValue: 0}),

    // When was the cache created on the server side
    created: DS.attr(),

    // Is the List operation waiting for a cache to be generated?
    isLoading: DS.attr('boolean', {defaultValue: true}),

    // Total number of buckets
    total: DS.attr('number', {defaultValue: 0})
});
