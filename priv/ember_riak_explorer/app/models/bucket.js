import DS from 'ember-data';
import objectToArray from '../utils/riak-util';

export default DS.Model.extend({
    name: DS.attr('string'),
    cluster: DS.attr(),
    clusterId: DS.attr(),
    bucketTypeId: DS.attr(),

    // {"allow_mult":false, "basic_quorum":false, ... }
    props: DS.attr(),

    bucketId: function() {
        return this.get('name');
    }.property('name'),

    propsList: function() {
        if(!this.get('props')) {
            return [];
        }
        return objectToArray(this.get('props'));
    }.property('props')
});
