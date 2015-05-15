import Ember from 'ember';
import objectToArray from '../utils/riak-util';

export default Ember.Route.extend({
    queryParams: {
        node_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var propsUrl = '/riak/nodes/' + params.node_id + '/stats' ;
        var propsResult = Ember.$.ajax( propsUrl, { dataType: "json" } );
        return propsResult.then(
            function(data) {
                var statsArray = objectToArray(data);
                return {
                    node: params.node_id,
                    stats: statsArray
                };
            }
        );
    }
});
