import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: false
        }
    },

    model: function(params) {
        var cluster = {
            cluster: this.explorer.getCluster(params.cluster_id, true),
            bucket_types: this.store.find('bucket_type', { cluster_id: params.cluster_id }),
        };
        
        return new Ember.RSVP.hash(cluster);
    }
});
