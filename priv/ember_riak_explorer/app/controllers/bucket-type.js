import Ember from 'ember';

export default Ember.Controller.extend({
    queryParams: ['node_id', 'bucket_type_id'],
    node_id: null,
    bucket_type_id: null
});
