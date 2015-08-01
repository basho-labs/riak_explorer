import DS from 'ember-data';
import CachedList from "./cached-list";

export default CachedList.extend({
    bucket: DS.attr(),
    cluster: DS.attr(),

    // List of riak-object model instances
    keys: DS.attr(null, {defaultValue: []})
});
