import DS from 'ember-data';
import CachedList from "./cached-list";

var KeyList = CachedList.extend({
    bucket: DS.attr(),
    cluster: DS.attr(),

    // List of riak-object model instances
    keys: DS.attr(null, {defaultValue: []}),

    bucketId: function() {
        return this.get('bucket').get('bucketId');
    }.property('bucket'),

    bucketTypeId: function() {
        return this.get('bucket').get('bucketTypeId');
    }.property('bucket'),

    clusterId: function() {
        return this.get('cluster').get('clusterId');
    }.property('cluster'),

    hasKeys: function() {
        return this.get('count') > 0;
    }.property('count'),

    showDeleteKeys: function() {
        return this.get('cluster').get('developmentMode') &&
            this.get('hasKeys');
    }.property('cluster', 'count')
});
export default KeyList;
