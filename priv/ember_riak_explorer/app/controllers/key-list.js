import Ember from 'ember';

export default Ember.Controller.extend({
    queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id'],
    cluster_id: null,
    bucket_type_id: null,
    bucket_id: null
});
