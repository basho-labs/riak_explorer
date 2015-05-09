import DS from 'ember-data';

export default DS.Model.extend({
    links: DS.attr(),
    resources: DS.attr()
});
