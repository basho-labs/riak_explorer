import DS from 'ember-data';
import CachedList from "./cached-list";

// BucketList object, used to list buckets for a selected bucket type.
export default CachedList.extend({
    // List of Bucket model instances
    buckets: DS.attr(null, {defaultValue: []}),

    // Bucket-type model instance
    bucketType: DS.belongsTo('bucket-type'),

    // Cluster model instance
    cluster: DS.belongsTo('cluster'),

    bucketTypeId: function() {
        return this.get('bucketType').get('bucketTypeId');
    }.property('bucketType'),

    clusterId: function() {
        return this.get('cluster').get('clusterId');
    }.property('cluster')
});
