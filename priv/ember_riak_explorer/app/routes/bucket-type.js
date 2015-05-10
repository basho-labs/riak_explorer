import Ember from 'ember';

function objectToString(obj) {
    var propsStr = "{";
    var i = 0;
    for(var prop in obj) {
        if (obj.hasOwnProperty(prop)) {
            if (i > 0) {
                propsStr += ", ";
            }
            propsStr += prop + ': ' + obj[prop];
            i++;
        }
    }
    return propsStr + "}";
}

function objectToArray(obj) {
    var propsArray = [];
    for(var prop in obj) {
        if (obj.hasOwnProperty(prop)) {
            if (obj[prop] !== null && typeof obj[prop] === 'object') {
                propsArray.push({key:prop, value:objectToString(obj[prop], true)});
            } else {
                propsArray.push({key:prop, value:obj[prop]});
            }
            
        }
    }
    return propsArray;
}

export default Ember.Route.extend({
    queryParams: {
        node_id: {
            refreshModel: true
        },
        bucket_type_id: {
            refreshModel: true
        }
    },

    model: function(params) {
        var propsUrl = '/riak/' + params.node_id + '/types/' + params.bucket_type_id + '/props' ;
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
