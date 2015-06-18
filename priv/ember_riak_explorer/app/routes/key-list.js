import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: true
        },
        bucket_type_id: {
            refreshModel: true
        },
        bucket_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var url = '/explore/clusters/' + params.cluster_id +
            '/bucket_types/' + params.bucket_type_id + '/buckets/' +
            params.bucket_id + '/keys' ;

        var result = Ember.$.ajax( url, { dataType: "json" } );
        var store = this.store;

        return result.then(
            function(data) {
                var bucket = store.createRecord('bucket', {
                    name: params.bucket_id,
                    clusterId: params.cluster_id,
                    bucketTypeId: params.bucket_type_id
                });
                var keyList = data.keys.keys.map(function(key) {
                    return store.createRecord('riak-object', {
                        key: key,
                        bucket: bucket
                    });
                });

                return {
                    cluster_id: params.cluster_id,
                    bucket_type_id: params.bucket_type_id,
                    bucket_id: params.bucket_id,
                    key_list: keyList,
                    count: data.keys.count,
                    total: data.keys.total
                };
            }
        );
    }
});
