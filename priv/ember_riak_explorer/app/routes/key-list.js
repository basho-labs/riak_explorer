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
        var explorerService = this.explorer;

        var bucket = store.createRecord('bucket', {
            name: params.bucket_id,
            bucketTypeId: params.bucket_type_id,
            clusterId: params.cluster_id
        });

        return result.then(
            function(data) {
                var keyList = data.keys.keys.map(function(key) {
                    var obj = store.createRecord('riak-object', {
                        key: key,
                        bucket: bucket
                    });
                    if(explorerService.wasKeyDeleted(obj)) {
                        obj.set('markedDeleted', true);
                    }
                    return obj;
                });
                return store.createRecord('key-list', {
                    bucket: bucket,
                    created: data.keys.created,
                    count: data.keys.count,
                    keys: keyList,
                    total: data.keys.total
                });
            }
        );
    }
});
