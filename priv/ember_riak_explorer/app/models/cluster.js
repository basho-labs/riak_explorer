import DS from 'ember-data';

/**
* Represents a Riak cluster as a whole.
*/
export default DS.Model.extend({
    clusterId: DS.attr('string'),

    // Riak node through which Explorer connects to the riak cluster
    connectedNode: DS.attr('string', {defaultValue: 'riak@127.0.0.1'}),

    // Is this cluster in Dev Mode? Set in the Explorer config file
    // Dev mode allows expensive operations like list keys, delete bucket, etc
    developmentMode: DS.attr('boolean', {defaultValue: false}),

    productionMode: function() {
        return !this.get('developmentMode');
    }.property('developmentMode')
});
