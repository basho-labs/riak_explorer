import DS from 'ember-data';

export default DS.Model.extend({
    props: DS.attr(),
    links: DS.attr()
});
