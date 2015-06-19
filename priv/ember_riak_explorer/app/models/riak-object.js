import DS from 'ember-data';

export default DS.Model.extend({
    key: DS.attr(),
    bucket: DS.belongsTo('bucket'),
    headers: DS.attr(),
    contents: DS.attr()
});
