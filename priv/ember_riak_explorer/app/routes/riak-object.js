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
        },
        object_key: {
            refreshModel: true
        }
    },

    actions: {
        error: function() {
            if (error && error.status === 404) {
                // error substate and parent routes do not handle this error
                // return this.transitionTo('modelNotFound');
                alert('404');
            } else {
                // Unknown error, bubble error event up to routes/application.js
                return true;
            }
        }
    },

    model: function(params) {
        return this.explorer.getRiakObject(params.cluster_id,
            params.bucket_type_id, params.bucket_id, params.object_key, this.store);
    }
});
