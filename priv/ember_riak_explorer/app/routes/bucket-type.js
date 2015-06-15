import Ember from 'ember';
import objectToArray from '../utils/riak-util';

export default Ember.Route.extend({
    queryParams: {
        cluster_id: {
            refreshModel: true
        },
        bucket_type_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var propsUrl = '/riak/clusters/' + params.cluster_id + '/types/' +
            params.bucket_type_id + '/props' ;
        var propsResult = Ember.$.ajax( propsUrl, { dataType: "json" } );
        return propsResult.then(
            function(data) {
                var propsArray = objectToArray(data.props);
                return {
                    bucketType: params.bucket_type_id,
                    props: propsArray
                };
            }
        );
    }
});
