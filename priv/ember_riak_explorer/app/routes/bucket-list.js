import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: true
        },
        bucket_type_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var url = '/explore/clusters/' + params.cluster_id +
            '/bucket_types/' + params.bucket_type_id + '/buckets' ;

        var result = Ember.$.ajax( url, { dataType: "json" } );
        var store = this.store;

        return result.then(
            function(data) {
                var bucketList = data.buckets.buckets.map(function(bucketName) {
                    return store.createRecord('bucket', {
                        name: bucketName,
                        clusterId: params.cluster_id,
                        bucketTypeId: params.bucket_type_id
                    });
                });
                return store.createRecord('bucket-list', {
                    clusterId: params.cluster_id,
                    bucketTypeId: params.bucket_type_id,
                    buckets: bucketList,
                    total: data.buckets.total,
                    count: data.buckets.count,
                    created: data.buckets.created
                });
            }
        );
    }
});
