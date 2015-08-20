import Ember from 'ember';

export default Ember.Controller.extend({
    queryParams: ['clusterId', 'bucketTypeId', 'bucketId', 'key'],
    clusterId: null,
    bucketTypeId: null,
    bucketId: null,
    key: null
});
