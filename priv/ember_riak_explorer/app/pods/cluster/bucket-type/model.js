import DS from 'ember-data';
import objectToArray from '../../../utils/riak-util';

export default DS.Model.extend({
    name: DS.attr('string'),
    cluster: DS.attr(),

    // {"allow_mult":false, "basic_quorum":false, ... }
    props: DS.attr(),

    bucketTypeId: function() {
        return this.get('name');
    }.property('name'),

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
