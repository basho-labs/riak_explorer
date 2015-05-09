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

        if (query && query.node_id) { url.unshift('nodes/' + query.node_id); }

        if (prefix) { url.unshift(prefix); }

        url = url.join('/');
        if (!host && url) { url = '/' + url; }

        return url;
    },

    /**
      Called by the store in order to fetch a JSON array for
      the records that match a particular query.
      The `findQuery` method makes an Ajax (HTTP GET) request to a URL computed
      by `buildURL`, and returns a promise for the resulting payload.
      The `query` argument is a simple JavaScript object that will be passed directly
      to the server as parameters.
      @private
      @method findQuery
      @param {DS.Store} store
      @param {subclass of DS.Model} type
      @param {Object} query
      @return {Promise} promise
    */
    findQuery: function(store, type, query) {
      var url = this.buildURL(type.typeKey, null, query);

      if (this.sortQueryParams) {
        query = this.sortQueryParams(query);
      }

      return this.ajax(url, 'GET');
    },

    pathForType: function(type) {
      return Ember.String.underscore(Ember.String.pluralize(type));
    }
});
