import DS from 'ember-data';
import objectToArray from '../../utils/riak-util';

export default DS.Model.extend({
    name: DS.attr('string'),
    cluster: DS.attr(),

    bucketType: DS.belongsTo('bucket-type'),

    // {"allow_mult":false, "basic_quorum":false, ... }
    props: DS.attr(),

    bucketId: function() {
        return this.get('name');
    }.property('name'),

    bucketTypeId: function() {
        return this.get('bucketType').get('bucketTypeId');
    }.property('cluster'),

    clusterId: function() {
        return this.get('cluster').get('clusterId');
    }.property('cluster'),

    propsList: function() {
        if(!this.get('props')) {
            return [];
        }
        return objectToArray(this.get('props'));
    }.property('props')
});
