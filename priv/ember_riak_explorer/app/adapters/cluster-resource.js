import DS from 'ember-data';
import Ember from "ember";

// ClusterResourceAdapter
// Models fetching Riak resources from Explorer's cluster proxy API
var ClusterResourceAdapter = DS.RESTAdapter.extend({
    namespace: 'explore',

    /**
      Builds a URL for a given type and optional ID.

      By default, it pluralizes the type's name (for example, 'post'
      becomes 'posts' and 'person' becomes 'people'). To override the
      pluralization see [pathForType](#method_pathForType).

      If an ID is specified, it adds the ID to the path generated
      for the type, separated by a `/`.

      When called by RESTAdapter.findMany() the `id` and `snapshot` parameters
      will be arrays of ids and snapshots.

      @method buildURL
      @param {String} modelName
      @param {(String|Array|Object)} id single id or array of ids or query
      @param {(DS.Snapshot|Array)} snapshot single snapshot or array of snapshots
      @param {String} requestType
      @param {Object} query object of query parameters to send for query requests.
      @return {String} url
    */
    buildURL: function(modelName, id, snapshot, requestType, query) {
        var url = [];
        var host = Ember.get(this, 'host');
        var prefix = this.urlPrefix();

        if (modelName) { url.push(this.pathForType(modelName)); }

        // We might get passed in an array of ids from findMany
        // in which case we don't want to modify the url, as the
        // ids will be passed in through a query param
        if (id && !Ember.isArray(id)) { url.push(encodeURIComponent(id)); }

        if (query && query.cluster_id) { url.unshift('clusters/' + query.cluster_id); }

        if (query && query.node_id) { url.unshift('nodes/' + query.node_id); }

        if (prefix) { url.unshift(prefix); }

        url = url.join('/');
        if (!host && url) { url = '/' + url; }

        return url;
    },

    /**
    The `findRecord()` method is invoked when the store is asked for a record that
    has not previously been loaded. In response to `findRecord()` being called, you
    should query your persistence layer for a record with the given ID. Once
    found, you can asynchronously call the store's `push()` method to push
    the record into the store.
    @method findRecord
    @param {DS.Store} store
    @param {DS.Model} type
    @param {String} id
    @param {DS.Snapshot} snapshot of the model instance (immutable)
    @return {Promise} promise
    */
    findRecord: function(store, type, id, snapshot) {
        var url = this.buildURL(type.modelName, id, snapshot, 'findRecord');
        //   if (this.sortQueryParams) {
        //     query = this.sortQueryParams(query);
        //   }

          return this.ajax(url, 'GET');
    },

    pathForType: function(type) {
      return Ember.String.underscore(Ember.String.pluralize(type));
    }
});
export default ClusterResourceAdapter;
