import DS from 'ember-data';

var Bucket = DS.Model.extend({
    cluster: DS.belongsTo('cluster'),

    bucketType: DS.belongsTo('bucket-type'),

    isKeyListLoaded: DS.attr('boolean', { defaultValue: false }),

    keyList: DS.belongsTo('key-list'),

    name: DS.attr('string'),

    props: DS.belongsTo('bucket-props'),

    bucketId: function() {
        return this.get('name');
    }.property('name'),

    bucketTypeId: function() {
        return this.get('bucketType').get('bucketTypeId');
    }.property('cluster'),

    clusterId: function() {
        return this.get('cluster').get('clusterId');
    }.property('cluster'),

    isActive: function() {
        return this.get('props').get('isActive');
    }.property('props'),

    index: function() {
        return this.get('cluster').get('indexes')
            .findBy('name', this.get('props').get('searchIndexName'));
    }.property('cluster'),

    objectModelName: function() {
        if(this.get('props').get('isCounter')) {
            return 'riak-object.counter';
        }
        if(this.get('props').get('isSet')) {
            return 'riak-object.set';
        }
        return 'riak-object';
    }.property('props'),

    propsList: function() {
        if(!this.get('props')) {
            return [];
        }
        return this.get('props').get('propsList');
    }.property('props')
});

export default Bucket;
