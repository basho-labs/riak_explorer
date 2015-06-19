import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr(),
    clusterId: DS.attr(),
    bucketTypeId: DS.attr(),

    // Separate from .id because Ember requires the ID to be unique globally,
    // but buckets are unique only within a bucket type
    bucketId: function() {
        return this.get('name');
    }.property('name')
});
