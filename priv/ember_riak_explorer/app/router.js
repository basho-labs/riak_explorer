import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
    location: config.locationType
});

export default Router.map(function() {
    this.route('explorer_api');
    this.route('cluster', { path: '/cluster/:cluster_id' });
    this.route('bucket-type',
        { path: '/cluster/:clusterId/bucket_type/:bucketTypeId' });
    this.route('bucket',
        { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId' });
    this.route('riak-object',
        { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/key/:key' });
    this.route('riak-object.edit',
        { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/key/:key/edit' });
    this.route('riak-object.counter',
        { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/counter/:key' });
    this.route('riak-object.set',
        { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/set/:key' });
    this.route('riak_ping');
    this.route('node_stats');
    this.route('error', { path: '/error' }, function() {
        this.route('unknown');
        this.route('cluster-not-found');
        this.route('object-not-found');
    });
});
