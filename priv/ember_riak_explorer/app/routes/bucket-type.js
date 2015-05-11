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
        var propsUrl = '/explore/clusters/' + params.cluster_id + '/bucket_types/' + params.bucket_type_id + '/props' ;
        var propsResult = Ember.$.ajax( propsUrl, { dataType: "json" } );
        return propsResult.then(
            function(data) {
                console.log(data);
                var propsArray = objectToArray(data.props);
                console.log(propsArray);
                return {
                    bucketType: params.bucket_type_id,
                    props: propsArray
                };
            }
        );
    }
});
