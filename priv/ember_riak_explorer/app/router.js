import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
  location: config.locationType
});

export default Router.map(function() {
  this.route('explorer_api');
  this.route('bucket_type');
  this.route('bucket_list');
  this.route('key_list');
  this.route('riak_ping');
  this.route('node_stats');
  this.route('cluster');
  this.route('riak-object');
  this.route('riak-object-edit');
});
