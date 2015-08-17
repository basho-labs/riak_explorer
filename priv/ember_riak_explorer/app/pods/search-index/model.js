import DS from 'ember-data';

var SearchIndex = DS.Model.extend({
    cluster: DS.belongsTo('cluster'),
    
    name: DS.attr('string'),

    // Index's n_val
    nVal: DS.attr('integer', {defaultValue: 3}),

    // Name of the schema the index is using
    schema: DS.attr('string')
});
export default SearchIndex;
