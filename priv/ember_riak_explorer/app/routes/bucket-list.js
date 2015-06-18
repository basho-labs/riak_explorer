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
                return {
                    cluster_id: params.cluster_id,
                    bucket_type_id: params.bucket_type_id,
                    bucket_list: bucketList
                };
            }
        );
    }
});
