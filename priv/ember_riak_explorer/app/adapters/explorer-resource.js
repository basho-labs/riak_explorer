import DS from 'ember-data';
import Ember from "ember";

// Models fetching Riak resources from Explorer's own API
var ExplorerResourceAdapter = DS.RESTAdapter.extend({
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
        switch (requestType) {
            case 'findRecord':
                return this.urlForFindRecord(id, modelName, snapshot);
            case 'findAll':
                return this.urlForFindAll(modelName);
            case 'query':
                return this.urlForQuery(query, modelName);
            default:
                return this._buildURL(modelName, id);
        }
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

    injectParentIds: function(payload, query) {
        if(query.clusterId) {
            payload.cluster_id = query.clusterId;
        }
        if(query.bucketTypeId) {
            payload.bucket_type_id = query.bucketTypeId;
        }
    },

    /**
    Normalize the ID in a given resource into globally unique
    version required by Ember Data.
    (Most Riak cluster resources do not have globally unique IDs.
    For example, bucket types are only unique within a cluster.)
    */
    normalizeId: function(record, query, idKey) {
        var prefix = [];
        if(query.clusterId) {
            prefix.push(query.clusterId);
        }
        if(query.bucketTypeId) {
            prefix.push(query.bucketTypeId);
        }
        if(!idKey) {
            idKey = 'id';
        }
        var originalId = record[idKey];
        record.original_id = originalId;
        prefix.push(originalId);
        record['id'] = prefix.join('/');
    },

    pathForType: function(type) {
        return Ember.String.underscore(Ember.String.pluralize(type));
    },

    /**
    Called by the store in order to fetch a JSON array for
    the records that match a particular query.
    @private
    @method query
    @param {DS.Store} store
    @param {DS.Model} type
    @param {Object} query (POJO, contains query parameters)
    @return {Promise} promise
    */
    query: function(store, type, query) {
        var url = this.buildURL(type.modelName, null, null, 'query', query);
        var adapter = this;
        var root = this.pathForType(type.modelName);
        var promise = this.ajax(url, 'GET').then(function(payload) {
            for(let i=0; i < payload[root].length; i++) {
                var record = payload[root][i];
                adapter.normalizeId(record, query);
                adapter.injectParentIds(record, query);
                console.log('record: %O', record);
            }
            return payload;
        });
        return promise;
    },

    urlForQuery: function(query, modelName) {
        if(modelName.indexOf('.') > -1) {
            // Deal with nested model names, like 'cluster.bucket_types'
            modelName = modelName.split('.').pop();
        }

        // For the moment, assume we're only dealing with cluster-based resources
        var url = this._buildURL('cluster', query.clusterId);

        return url + '/' + this.pathForType(modelName);
    }
});
export default ExplorerResourceAdapter;
