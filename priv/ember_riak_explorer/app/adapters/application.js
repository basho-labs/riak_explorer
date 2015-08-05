import DS from 'ember-data';
import Ember from "ember";

export default DS.RESTAdapter.extend({
    namespace: 'explore',

    /**
     @method buildURL
     @param {String} type
     @param {String} id
     @param {Object} query
     @return {String} url
    */
    buildURL: function(type, id, query) {
        var url = [];
        var host = Ember.get(this, 'host');
        var prefix = this.urlPrefix();

        if (type) { url.push(this.pathForType(type)); }

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
    This method delegates a query to the adapter. This is the one place where
    adapter-level semantics are exposed to the application.
    Exposing queries this way seems preferable to creating an abstract query
    language for all server-side queries, and then require all adapters to
    implement them.
    The call made to the server, using a Rails backend, will look something like this:
    ```
    Started GET "/api/v1/person?page=1"
    Processing by Api::V1::PersonsController#index as HTML
    Parameters: {"page"=>"1"}
    ```
    If you do something like this:
    ```javascript
    store.query('person', {ids: [1, 2, 3]});
    ```
    The call to the server, using a Rails backend, will look something like this:
    ```
    Started GET "/api/v1/person?ids%5B%5D=1&ids%5B%5D=2&ids%5B%5D=3"
    Processing by Api::V1::PersonsController#index as HTML
    Parameters: {"ids"=>["1", "2", "3"]}
    ```
    This method returns a promise, which is resolved with a `RecordArray`
    once the server returns.
    @method query
    @param {String} modelName
    @param {any} query an opaque query to be used by the adapter
    @return {Promise} promise
    */
    findQuery: function(store, type, query) {
      var url = this.buildURL(type.modelName, null, query);

      if (this.sortQueryParams) {
        query = this.sortQueryParams(query);
      }

      return this.ajax(url, 'GET');
    },

    pathForType: function(type) {
      return Ember.String.underscore(Ember.String.pluralize(type));
    }
});
