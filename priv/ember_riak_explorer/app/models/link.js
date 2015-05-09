import DS from 'ember-data';

export default DS.Model.extend({
    self: DS.attr(),
    related: DS.attr()
});
