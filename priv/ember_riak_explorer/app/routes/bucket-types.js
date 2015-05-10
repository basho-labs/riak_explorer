import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        node_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var bucketTypes = this.store.find('bucket_type', { node_id: params.node_id });
        return {
            node_id: params.node_id,
            bucketTypes: bucketTypes
        };
    }
});
