import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr('string'),
    cluster: DS.belongsTo('cluster'),
    props: DS.attr(),
    links: DS.attr(),

    // Separate from .id because Ember requires the ID to be unique globally,
    // but bucket types are unique only within a cluster
    bucketTypeId: function() {
        return this.get('name');
    }.property('name'),

    clusterId: function() {
        return this.get('cluster').get('id');
    }.property('cluster')
});
