import Ember from 'ember';

var RiakObjectEditRoute = Ember.Route.extend({
        model: function(params) {
            var explorer = this.explorer;
            var store = this.store;
            return explorer.getBucket(params.clusterId,
                    params.bucketTypeId, params.bucketId, store)
                .then(function(bucket) {
                    return explorer.getRiakObject(bucket, params.key, store);
                });
        }
});
export default RiakObjectEditRoute;
