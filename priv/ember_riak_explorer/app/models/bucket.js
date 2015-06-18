import DS from 'ember-data';

export default DS.Model.extend({
    name: DS.attr(),
    clusterId: DS.attr(),
    bucketTypeId: DS.attr()
});
