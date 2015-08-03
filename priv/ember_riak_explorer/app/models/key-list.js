import DS from 'ember-data';
import CachedList from "./cached-list";

export default CachedList.extend({
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

    showDeleteKeys: function() {
        return this.get('cluster').developmentMode &&
            this.get('keys').length > 0;
    }.property('keys')
});
