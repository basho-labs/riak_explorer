import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        clusterId: {
            refreshModel: true
        },
        bucketTypeId: {
            refreshModel: true
        },
        bucketId: {
            refreshModel: true
        },
        key: {
            refreshModel: true
        }
    },

    model: function(params) {
        return params;
    }
});
