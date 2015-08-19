import DS from 'ember-data';

var BucketType = DS.Model.extend({
    cluster: DS.belongsTo('cluster'),

    bucketList: DS.belongsTo('bucket-list'),

    isBucketListLoaded: DS.attr('boolean', { defaultValue: false }),

    bucketTypeId: function() {
        return this.get('originalId');
    }.property('originalId'),

    clusterId: function() {
        return this.get('cluster').get('clusterId');
    }.property('cluster'),

    index: function() {
        return this.get('cluster').get('indexes')
            .findBy('name', this.get('props').get('searchIndexName'));
    }.property('cluster'),

    isActive: function() {
        return this.get('props').get('isActive');
    }.property('props'),

    isInactive: function() {
        return !this.get('props').get('isActive');
    }.property('props'),

    name: function() {
        return this.get('id');
    }.property('id'),

    originalId: DS.attr('string'),

    // {"allow_mult":false, "basic_quorum":false, ... }
    props: DS.belongsTo('bucket-props')
});

export default BucketType;
