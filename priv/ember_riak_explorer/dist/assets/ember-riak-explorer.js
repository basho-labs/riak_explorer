"use strict";
/* jshint ignore:start */

/* jshint ignore:end */

define('ember-riak-explorer/adapters/application', ['exports', 'ember-data', 'ember', 'ember-riak-explorer/config/environment'], function (exports, _emberData, _ember, _emberRiakExplorerConfigEnvironment) {

  var ApplicationAdapter = _emberData['default'].RESTAdapter.extend({
    namespace: _emberRiakExplorerConfigEnvironment['default'].baseURL + 'explore'
  });

  exports['default'] = ApplicationAdapter;
});
define('ember-riak-explorer/adapters/bucket-type', ['exports', 'ember-riak-explorer/adapters/explorer-resource'], function (exports, _emberRiakExplorerAdaptersExplorerResource) {
  exports['default'] = _emberRiakExplorerAdaptersExplorerResource['default'].extend({});
});
define('ember-riak-explorer/adapters/bucket', ['exports', 'ember-riak-explorer/adapters/explorer-resource'], function (exports, _emberRiakExplorerAdaptersExplorerResource) {
  exports['default'] = _emberRiakExplorerAdaptersExplorerResource['default'].extend({});
});
define('ember-riak-explorer/adapters/config-file', ['exports', 'ember-riak-explorer/adapters/application', 'ember-riak-explorer/config/environment'], function (exports, _emberRiakExplorerAdaptersApplication, _emberRiakExplorerConfigEnvironment) {
  exports['default'] = _emberRiakExplorerAdaptersApplication['default'].extend({
    buildURL: function buildURL(modelName, id, snapshot, requestType, query) {
      return _emberRiakExplorerConfigEnvironment['default'].baseURL + 'explore/clusters/' + query.clusterId + '/nodes/' + query.nodeId + '/config/files';
    },

    query: function query(store, type, _query) {
      var url = this.buildURL(type.modelName, null, null, 'query', _query);

      var promise = this.ajax(url, 'GET').then(function (data) {

        data.files.forEach(function (file) {
          // assign id to file id
          file.file_id = file.id;
          // Then create a composite id for the file
          file.id = _query.clusterId + '/' + _query.nodeId + '/' + file.file_id;
        });

        return data;
      });

      return promise;
    }
  });
});
define('ember-riak-explorer/adapters/explorer-resource', ['exports', 'ember-riak-explorer/adapters/application', 'ember'], function (exports, _emberRiakExplorerAdaptersApplication, _ember) {

  // Models fetching Riak resources from Explorer's own API

  var ExplorerResourceAdapter = _emberRiakExplorerAdaptersApplication['default'].extend({
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
    buildURL: function buildURL(modelName, id, snapshot, requestType, query) {
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
    findRecord: function findRecord(store, type, id, snapshot) {
      var url = this.buildURL(type.modelName, id, snapshot, 'findRecord');
      //   if (this.sortQueryParams) {
      //     query = this.sortQueryParams(query);
      //   }

      return this.ajax(url, 'GET');
    },

    injectParentIds: function injectParentIds(payload, query) {
      if (query.clusterId) {
        payload.cluster_id = query.clusterId;
      }
      if (query.bucketTypeId) {
        payload.bucket_type_id = query.bucketTypeId;
      }
    },

    /**
     Normalize the ID in a given resource into globally unique
     version required by Ember Data.
     (Most Riak cluster resources do not have globally unique IDs.
     For example, bucket types are only unique within a cluster.)
     */
    normalizeId: function normalizeId(record, type, query) {
      var idKey = arguments.length <= 3 || arguments[3] === undefined ? 'id' : arguments[3];

      var fragments = [];
      var compositeId = null;

      if (query.clusterId) {
        fragments.push(query.clusterId);
      }
      if (query.bucketTypeId && type.modelName !== 'bucket-type') {
        fragments.push(query.bucketTypeId);
      }

      fragments.push(record[idKey]);
      record.original_id = record[idKey];
      compositeId = fragments.join('/');
      record.id = compositeId;

      if (record.props) {
        record.props.id = record.id;
      }
    },

    /**
     Relevant for Bucket Type Properties and Bucket Properties
     */
    normalizeProps: function normalizeProps(record, modelName) {
      if (modelName === 'bucket-type' || modelName === 'bucket') {
        record.props = {
          id: record.props.id,
          props: record.props
        };

        delete record.props.props.id;
      }
    },

    pathForType: function pathForType(type) {
      return _ember['default'].String.underscore(_ember['default'].String.pluralize(type));
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
    query: function query(store, type, _query) {
      var adapter = this;
      var url = this.buildURL(type.modelName, null, null, 'query', _query);

      var promise = this.ajax(url, 'GET').then(function (payload) {
        var root = adapter.pathForType(type.modelName);

        payload[root].forEach(function (record) {
          adapter.normalizeId(record, type, _query);
          adapter.injectParentIds(record, _query);
          adapter.normalizeProps(record, type.modelName);
        });

        return payload;
      });

      return promise;
    },

    /**
     Invoked when the store is asked for a single
     record through a query object.
     @private
     @method query
     @param {DS.Store} store
     @param {DS.Model} type
     @param {Object} query (POJO, contains query parameters)
     @return {Promise} promise
     */
    queryRecord: function queryRecord(store, type, query) {
      var adapter = this;
      var url = this.buildURL(type.modelName, null, null, 'query', query);
      var root = _ember['default'].String.underscore(type.modelName);

      var promise = this.ajax(url, 'GET').then(function (payload) {
        adapter.normalizeId(payload[root], type, query);
        adapter.injectParentIds(payload[root], query);
        adapter.normalizeProps(payload[root], type.modelName);

        return payload;
      });

      return promise;
    },

    urlForQuery: function urlForQuery(query, modelName) {
      var urlFragments = [];

      if (modelName.indexOf('.') > -1) {
        // Deal with nested model names, like 'cluster.bucket_types'
        modelName = modelName.split('.').pop();
      }

      // For the moment, assume we're only dealing with cluster-based resources
      urlFragments.push(this._buildURL('cluster', query.clusterId));
      urlFragments.push(this.pathForType('bucket-type'));

      if (query.bucketTypeId) {
        urlFragments.push(query.bucketTypeId);
      }

      return urlFragments.join('/');
    }
  });

  exports['default'] = ExplorerResourceAdapter;
});
define('ember-riak-explorer/adapters/log-file', ['exports', 'ember-riak-explorer/adapters/application', 'ember-riak-explorer/config/environment'], function (exports, _emberRiakExplorerAdaptersApplication, _emberRiakExplorerConfigEnvironment) {
  exports['default'] = _emberRiakExplorerAdaptersApplication['default'].extend({
    buildURL: function buildURL(modelName, id, snapshot, requestType, query) {
      return _emberRiakExplorerConfigEnvironment['default'].baseURL + 'explore/clusters/' + query.clusterId + '/nodes/' + query.nodeId + '/log/files';
    },

    query: function query(store, type, _query) {
      var url = this.buildURL(type.modelName, null, null, 'query', _query);

      var promise = this.ajax(url, 'GET').then(function (data) {

        data.files.forEach(function (file) {
          // assign id to file id
          file.file_id = file.id;
          // Then create a composite id for the file
          file.id = _query.clusterId + '/' + _query.nodeId + '/' + file.file_id;
        });

        return data;
      });

      return promise;
    }
  });
});
define('ember-riak-explorer/adapters/node', ['exports', 'ember-riak-explorer/adapters/application', 'ember-riak-explorer/config/environment'], function (exports, _emberRiakExplorerAdaptersApplication, _emberRiakExplorerConfigEnvironment) {
  exports['default'] = _emberRiakExplorerAdaptersApplication['default'].extend({
    buildURL: function buildURL(modelName, id, snapshot, requestType, query) {
      return _emberRiakExplorerConfigEnvironment['default'].baseURL + 'explore/clusters/' + query.clusterId + '/nodes';
    },

    query: function query(store, type, _query) {
      var url = this.buildURL(type.modelName, null, null, 'query', _query);

      return this.ajax(url, 'GET');
    }
  });
});
define('ember-riak-explorer/adapters/search-index', ['exports', 'ember-data', 'ember', 'ember-riak-explorer/config/environment'], function (exports, _emberData, _ember, _emberRiakExplorerConfigEnvironment) {

  var SearchIndexAdapter = _emberData['default'].RESTAdapter.extend({
    buildURL: function buildURL(modelName, id, snapshot, requestType, query) {
      return _emberRiakExplorerConfigEnvironment['default'].baseURL + 'riak/clusters/' + query.clusterId + '/search/index';
    },

    query: function query(store, type, _query) {
      var url = this.buildURL(type.modelName, null, null, 'query', _query);

      var promise = this.ajax(url, 'GET').then(function (indexes) {
        indexes.forEach(function (index) {
          index.id = _query.clusterId + '/' + index.name;
        });

        return indexes;
      });

      return promise;
    }
  });

  exports['default'] = SearchIndexAdapter;
});
define('ember-riak-explorer/app', ['exports', 'ember', 'ember/resolver', 'ember/load-initializers', 'ember-riak-explorer/config/environment'], function (exports, _ember, _emberResolver, _emberLoadInitializers, _emberRiakExplorerConfigEnvironment) {

  var App = undefined;

  _ember['default'].MODEL_FACTORY_INJECTIONS = true;

  App = _ember['default'].Application.extend({
    modulePrefix: _emberRiakExplorerConfigEnvironment['default'].modulePrefix,
    podModulePrefix: _emberRiakExplorerConfigEnvironment['default'].podModulePrefix,
    Resolver: _emberResolver['default']
  });

  (0, _emberLoadInitializers['default'])(App, _emberRiakExplorerConfigEnvironment['default'].modulePrefix);

  exports['default'] = App;
});
define('ember-riak-explorer/components/app-version', ['exports', 'ember-cli-app-version/components/app-version', 'ember-riak-explorer/config/environment'], function (exports, _emberCliAppVersionComponentsAppVersion, _emberRiakExplorerConfigEnvironment) {

  var name = _emberRiakExplorerConfigEnvironment['default'].APP.name;
  var version = _emberRiakExplorerConfigEnvironment['default'].APP.version;

  exports['default'] = _emberCliAppVersionComponentsAppVersion['default'].extend({
    version: version,
    name: name
  });
});
define('ember-riak-explorer/components/basic-dropdown', ['exports', 'ember-basic-dropdown/components/basic-dropdown'], function (exports, _emberBasicDropdownComponentsBasicDropdown) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberBasicDropdownComponentsBasicDropdown['default'];
    }
  });
});
define('ember-riak-explorer/components/breadcrumb-component', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    classNames: ['breadcrumb-container'],

    subSection: null,

    isClusterData: (function () {
      return this.get('subSection') === 'data';
    }).property('subSection'),

    isClusterOps: (function () {
      return this.get('subSection') === 'ops';
    }).property('ops'),

    isClusterQuery: (function () {
      return this.get('subSection') === 'query';
    }).property('subSection')
  });
});
define('ember-riak-explorer/components/bucket-properties-list', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/bucket-properties', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/bucket-types', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/button/delete-object', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span',

    actions: {
      deleteObject: function deleteObject(object) {
        // Send its primary action to riak-object controller
        this.sendAction('action', object);
      }
    }
  });
});
define('ember-riak-explorer/components/button/edit-object', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span'
  });
});
define('ember-riak-explorer/components/button/object-view-raw', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span'
  });
});
define('ember-riak-explorer/components/button/refresh-buckets', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    actions: {
      refreshBuckets: function refreshBuckets(bucketList) {
        // Send its primary action to parent controller
        this.sendAction('action', bucketList);
      }
    }
  });
});
define('ember-riak-explorer/components/button/refresh-keys', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    actions: {
      refreshKeys: function refreshKeys(keyList) {
        // Send its primary action to parent controller
        this.sendAction('action', keyList);
      }
    }
  });
});
define('ember-riak-explorer/components/button/set-element-remove', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span',

    actions: {
      removeElement: function removeElement(model, item) {
        // Send its action to parent controller
        this.sendAction('removeElement', model, item);
      }
    }
  });
});
define('ember-riak-explorer/components/code-highlighter', ['exports', 'ember'], function (exports, _ember) {
  /* global hljs */

  exports['default'] = _ember['default'].Component.extend({
    tagName: 'pre',

    classNames: ['code-highlighter'],

    didRender: function didRender() {
      var codeBlock = this.$().find('code')[0];

      hljs.highlightBlock(codeBlock);

      // Highlight JS is pre-pending whitespace for some reason. This removes it.
      _ember['default'].$('.hljs').html(_ember['default'].$.trim(_ember['default'].$('.hljs').html()));
    }
  });
});
define('ember-riak-explorer/components/content-editable', ['exports', 'ember-content-editable/components/content-editable'], function (exports, _emberContentEditableComponentsContentEditable) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberContentEditableComponentsContentEditable['default'];
    }
  });
});
define('ember-riak-explorer/components/dashboard-module', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/em-tab-list', ['exports', 'ember-idx-tabs/tab-list'], function (exports, _emberIdxTabsTabList) {
  exports['default'] = _emberIdxTabsTabList['default'];
});
define('ember-riak-explorer/components/em-tab-panel', ['exports', 'ember-idx-tabs/tab-panel'], function (exports, _emberIdxTabsTabPanel) {
  exports['default'] = _emberIdxTabsTabPanel['default'];
});
define('ember-riak-explorer/components/em-tab', ['exports', 'ember-idx-tabs/tab'], function (exports, _emberIdxTabsTab) {
  exports['default'] = _emberIdxTabsTab['default'];
});
define('ember-riak-explorer/components/em-tabs', ['exports', 'ember-idx-tabs/tabs'], function (exports, _emberIdxTabsTabs) {
  exports['default'] = _emberIdxTabsTabs['default'];
});
define('ember-riak-explorer/components/ember-wormhole', ['exports', 'ember-wormhole/components/ember-wormhole'], function (exports, _emberWormholeComponentsEmberWormhole) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberWormholeComponentsEmberWormhole['default'];
    }
  });
});
define('ember-riak-explorer/components/link/bucket-type', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span'
  });
});
define('ember-riak-explorer/components/link/link-bucket', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span'
  });
});
define('ember-riak-explorer/components/link/link-cluster', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span'
  });
});
define('ember-riak-explorer/components/link/link-index', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span'
  });
});
define('ember-riak-explorer/components/link/link-object', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'span'
  });
});
define('ember-riak-explorer/components/loading-spinner', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/nav-cluster-link', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'li'
  });
});
define('ember-riak-explorer/components/object-actions', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    actions: {
      deleteObject: function deleteObject(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
      }
    }
  });
});
define('ember-riak-explorer/components/object-contents-counter', ['exports', 'ember'], function (exports, _ember) {

  var ObjectContentsCounterComponent = _ember['default'].Component.extend({
    actions: {
      decrementCounter: function decrementCounter(object) {
        // Send action to parent controller
        this.sendAction('decrementCounter', object);
      },

      deleteObject: function deleteObject(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
      },

      incrementCounter: function incrementCounter(object) {
        // Send action to parent controller
        this.sendAction('incrementCounter', object);
      }
    }
  });
  exports['default'] = ObjectContentsCounterComponent;
});
define('ember-riak-explorer/components/object-contents-counters-embedded', ['exports', 'ember-riak-explorer/components/object-contents-counter'], function (exports, _emberRiakExplorerComponentsObjectContentsCounter) {

  var ObjectContentsCountersEmbeddedComponent = _emberRiakExplorerComponentsObjectContentsCounter['default'].extend({
    actions: {
      /**
       * The user has clicked on the Delete Counter button.
       * Forward the `removeField` action to parent controller.
       *
       * @event removeField
       * @param model {RiakObjectMap} Current map
       * @param field {RiakObjectMapField} Counter to be removed
       */
      removeField: function removeField(model, field) {
        this.sendAction('removeField', model, 'counter', field);
      }
    }
  });
  exports['default'] = ObjectContentsCountersEmbeddedComponent;
});
define('ember-riak-explorer/components/object-contents-flags', ['exports', 'ember'], function (exports, _ember) {

  var ObjectContentsFlagsComponent = _ember['default'].Component.extend({
    actions: {
      /**
       * The user has clicked on the Delete Flag button.
       * Forward the `removeField` action to parent controller.
       *
       * @event removeField
       * @param model {RiakObjectMap} Current map
       * @param field {RiakObjectMapField} Flag to be removed
       */
      removeField: function removeField(model, field) {
        this.sendAction('removeField', model, 'flag', field);
      }
    }
  });
  exports['default'] = ObjectContentsFlagsComponent;
});
define('ember-riak-explorer/components/object-contents-map', ['exports', 'ember'], function (exports, _ember) {

  /**
   * Displays and manages a list of all field types in a Map.
   * @see RiakObjectMap
   *
   * @class ObjectContentsMapComponent
   * @extends Ember.Component
   * @constructor
   */
  var ObjectContentsMapComponent = _ember['default'].Component.extend({
    explorer: _ember['default'].inject.service('explorer'),
    store: _ember['default'].inject.service('store'),

    actions: {
      /**
       * The user has added an element to a nested Set field.
       *
       * @event addElement
       * @param setField {RiakObjectMapField} A nested Set field
       * @param newElement {String} New element to be added
       */
      addElement: function addElement(setField, newElement) {
        this.get('explorer').updateDataType(setField, 'addElement', newElement);
        setField.addElement(newElement);
      },

      /**
       * The user has clicked on the 'Add Field' button.
       * Creates a new field object and adds it to this map's contents.
       *
       * @event addField
       * @param model {RiakObjectMap} Parent map model
       * @param fieldType {String} Field type ('register', 'flag' or 'counter')
       * @param newFieldName {String} Name of the register to be added
       * @param newFieldValue {String} Value for the register to be added
       */
      addField: function addField(model, fieldType, newFieldName, newFieldValue) {
        if (!newFieldName || !newFieldValue) {
          return; // Fields must have non-empty names and values
        }
        var newField = this.get('store').createRecord('riak-object.map-field', {
          fieldType: fieldType,
          name: newFieldName,
          parentMap: model,
          rootMap: model.get('rootMap'),
          value: newFieldValue
        });
        newField.normalizeName();
        this.get('explorer').updateDataType(model, 'addField', newField);
        model.addField(fieldType, newField);
      },

      deleteObject: function deleteObject(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
      },

      editField: function editField(model, fieldType, field) {
        console.log('Editing field in %O, type: %s, field: %O', model, fieldType, field);
      },

      /**
       * The user has clicked on a Remove Field button.
       * Removes the specified nested field from this map.
       *
       * @event removeCounter
       * @param model {RiakObjectMap} Current map
       * @param fieldType {String}
       * @param field {RiakObjectMapField} Field to be removed
       */
      removeField: function removeField(model, fieldType, field) {
        this.get('explorer').updateDataType(model, 'removeField', field);
        model.removeField(fieldType, field);
      },

      /**
       * The user has removed an element from the nested Set field.
       *
       * @event removeElement
       * @param setField {RiakObjectMapField}
       * @param element {String}
       */
      removeElement: function removeElement(setField, element) {
        this.get('explorer').updateDataType(setField, 'removeElement', element);
        setField.removeElement(element);
      }
    }
  });
  exports['default'] = ObjectContentsMapComponent;
});
define('ember-riak-explorer/components/object-contents-maps-embedded', ['exports', 'ember-riak-explorer/components/object-contents-set'], function (exports, _emberRiakExplorerComponentsObjectContentsSet) {

  var ObjectContentsMapsEmbeddedComponent = _emberRiakExplorerComponentsObjectContentsSet['default'].extend({
    actions: {
      addField: function addField(model, fieldType, newName, newValue) {
        this.sendAction('addField', model, fieldType, newName, newValue);
      },

      /**
       * The user has clicked on a Remove Nested Field button.
       * Forward the `removeField` action to parent controller.
       *
       * @event removeField
       * @param model {RiakObjectMap} Current map
       * @param field {RiakObjectMapField} Field to be removed
       */
      removeField: function removeField(model, fieldType, field) {
        this.sendAction('removeField', model, fieldType, field);
      }
    }
  });
  exports['default'] = ObjectContentsMapsEmbeddedComponent;
});
define('ember-riak-explorer/components/object-contents-registers', ['exports', 'ember'], function (exports, _ember) {

  /**
   * Displays a list of Register type fields in a Map Data Type.
   *
   * @class ObjectContentsRegistersComponent
   * @extends Ember.Component
   * @constructor
   */
  var ObjectContentsRegistersComponent = _ember['default'].Component.extend({
    /**
     * @property fieldToAddName
     * @type {String}
     */
    fieldToAddName: null,

    /**
     * @property fieldToAddValue
     * @type {String}
     */
    fieldToAddValue: null,

    actions: {
      /**
       * The user has clicked on the 'Add Register' button.
       * Forward the `addField` action to parent controller:
       * @see ObjectContentsMapComponent
       *
       * @event addField
       * @param model {RiakObjectMap}
       */
      addField: function addField(model) {
        var newName = this.get('fieldToAddName');
        var newValue = this.get('fieldToAddValue');
        if (!newName || !newValue) {
          return; // Registers must have non-empty names and values
        }
        this.sendAction('addField', model, 'register', newName, newValue);

        // Reset the UI fields
        this.set('fieldToAddName', null);
        this.set('fieldToAddValue', null);
      },

      /**
       * The user has clicked on the Edit Register button.
       * Forward the `editRegister` action to parent controller.
       *
       * @event editField
       * @param model {RiakObjectMap} Current map
       * @param register {RiakObjectMapField} Register to be removed
       */
      editField: function editField(model, register) {
        this.sendAction('editField', model, 'register', register);
      },

      /**
       * The user has clicked on the Delete Register button.
       * Forward the `removeField` action to parent controller.
       *
       * @event removeField
       * @param model {RiakObjectMap} Current map
       * @param register {RiakObjectMapField} Register to be removed
       */
      removeField: function removeField(model, register) {
        this.sendAction('removeField', model, 'register', register);
      }
    }
  });
  exports['default'] = ObjectContentsRegistersComponent;
});
define('ember-riak-explorer/components/object-contents-set-elements', ['exports', 'ember'], function (exports, _ember) {

  /**
   * Displays and manages a set's elements (either a Set field embedded in a Map,
   * or a standalone Set object).
   * @see RiakObjectSet
   * @see RiakObjectMapField
   *
   * @class ObjectContentsSetElementsComponent
   * @extends Ember.Component
   * @constructor
   */
  var ObjectContentsSetElementsComponent = _ember['default'].Component.extend({
    elementToAdd: null,

    actions: {
      /**
       * The user has added an element to the Set.
       * Forward the `addElement` action to parent controller, which is
       * one of:
       * @see ObjectContentsSetComponent
       * @see ObjectContentsSetsEmbeddedComponent
       *
       * @event addElement
       * @param set {RiakObjectSet|RiakObjectMapField}
       */
      addElement: function addElement(set) {
        this.sendAction('addElement', set, this.get('elementToAdd'));
        this.set('elementToAdd', null); // Reset text box
      },

      deleteObject: function deleteObject(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
      },

      /**
       * The user has removed an element to the Set.
       * Forward the `removeElement` action to parent controller, which is
       * one of:
       * @see ObjectContentsSetComponent
       * @see ObjectContentsSetsEmbeddedComponent
       *
       * @event removeElement
       * @param set {RiakObjectSet|RiakObjectMapField}
       * @param element {String}
       */
      removeElement: function removeElement(set, element) {
        this.sendAction('removeElement', set, element);
      }
    }
  });
  exports['default'] = ObjectContentsSetElementsComponent;
});
define('ember-riak-explorer/components/object-contents-set', ['exports', 'ember'], function (exports, _ember) {

  /**
   * Displays and manages a list of all elements in a Riak server-side Set.
   * @see RiakObjectSet
   *
   * @class ObjectContentsSetComponent
   * @extends Ember.Component
   * @constructor
   */
  var ObjectContentsSetComponent = _ember['default'].Component.extend({
    actions: {
      /**
       * The user has added an element to the Set.
       * Forward the `addElement` action to parent controller.
       * @see RiakObjectSetController
       *
       * @event addElement
       * @param set {RiakObjectSet}
       * @param element {String}
       */
      addElement: function addElement(set, element) {
        this.sendAction('addElement', set, element);
      },

      /**
       * The user has clicked the Delete Set (deletes the whole set object)
       * Forward the `deleteObject` action to parent controller.
       * @see RiakObjectSetController
       *
       * @event deleteObject
       * @param object {RiakObjectSet}
       */
      deleteObject: function deleteObject(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
      },

      /**
       * The user has removed an element from the Set.
       * Forward the `removeElement` action to parent controller.
       * @see RiakObjectSetController
       *
       * @event removeElement
       * @param set {RiakObjectSet}
       * @param element {String}
       */
      removeElement: function removeElement(set, element) {
        // Send its action to parent controller
        this.sendAction('removeElement', set, element);
      }
    }
  });
  exports['default'] = ObjectContentsSetComponent;
});
define('ember-riak-explorer/components/object-contents-sets-embedded', ['exports', 'ember-riak-explorer/components/object-contents-set'], function (exports, _emberRiakExplorerComponentsObjectContentsSet) {

  /**
   * Displays and manages a list of all elements in an embedded Set field
   * @see RiakObjectMap
   * @see RiakObjectMapField
   *
   * @class ObjectContentsSetsEmbeddedComponent
   * @extends Ember.Component
   * @constructor
   */
  var ObjectContentsSetsEmbeddedComponent = _emberRiakExplorerComponentsObjectContentsSet['default'].extend({
    actions: {
      /**
       * The user has added an element to the nested Set field.
       * Forward the `addElement` action to parent controller.
       * @see ObjectContentsMapComponent
       *
       * @event addElement
       * @param setField {RiakObjectMapField}
       * @param element {String}
       */
      addElement: function addElement(setField, element) {
        this.sendAction('addElement', setField, element);
      },

      /**
       * The user has removed an element from the nested Set field.
       * Forward the `removeElement` action to parent controller.
       *
       * @event removeElement
       * @param setField {RiakObjectMapField}
       * @param element {String}
       */
      removeElement: function removeElement(setField, element) {
        // Send its action to parent controller
        this.sendAction('removeElement', setField, element);
      },

      /**
       * The user has clicked 'Remove Set' button, to delete the field from
       * its parent map.
       * Forward the `removeField` action to parent controller.
       *
       * @event removeField
       * @param model {RiakObjectMap}
       * @param setField {RiakObjectMapField}
       */
      removeField: function removeField(model, setField) {
        this.sendAction('removeField', model, 'set', setField);
      }
    }
  });
  exports['default'] = ObjectContentsSetsEmbeddedComponent;
});
define('ember-riak-explorer/components/object-contents', ['exports', 'ember'], function (exports, _ember) {

  var ObjectContentsComponent = _ember['default'].Component.extend({
    actions: {
      deleteObject: function deleteObject(object) {
        // Send action to riak-object controller
        this.sendAction('deleteObject', object);
      },

      saveObject: function saveObject(object) {
        // Send its primary action to riak-object-edit controller
        this.sendAction('saveObject', object);
      }
    }
  });
  exports['default'] = ObjectContentsComponent;
});
define('ember-riak-explorer/components/object-headers-edit', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/object-headers', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/object-location', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    actions: {
      editCancel: function editCancel(object) {
        // Send its primary action to parent controller
        this.sendAction('action', object);
      }
    }
  });
});
define('ember-riak-explorer/components/object-metadata', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/object-version', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/pagination-component', ['exports', 'ember'], function (exports, _ember) {

  /**
   * A server side pagination UI component. Determines how many links to show, handling click actions on those links,
   * sending data "up" to be acted upon, and updating selected state.
   *
   * @class pagination-component
   * @extends Ember.Component
   */
  exports['default'] = _ember['default'].Component.extend({
    /**
     * Sets the class name of the component
     *
     * @property classNames
     * @type Array
     */
    classNames: ['pagination-component-container'],

    /**
     * Stores the number of pagination links the UI could potentially display
     *
     * @property numberLinksCount
     * @type Integer
     * @default 0
     */
    numberLinksCount: 0,

    /**
     * Stores the current page or chunk that the UI is displaying
     *
     * @property currentChunk
     * @type Integer
     * @default 1
     */
    currentChunk: 1,

    /**
     * Stores the current page or chunk size
     *
     * @property chunkSize
     * @type Integer
     * @default 0
     */
    chunkSize: 0,

    /**
     * An array of sequential integers starting at 1. i.e. [1,2,3,4,5,6]
     * This is used to create the links in the UI as handlebars does not have a "times" helper by default
     *
     * @property numberLinks
     * @type Array
     * @default []
     */
    numberLinks: [],

    /**
     * Stores the total length of the items on which are being paginated
     *
     * @property totalSize
     * @type Integer
     * @default 0
     */
    totalSize: 0,

    /**
     * All actions that the pagination component handles. Upon receiving an action, it updates the state of the component
     * and sends the event "up" for higher level work that it is not aware of.
     *
     * @property actions
     * @type Object
     */
    actions: {
      numberLinkClick: function numberLinkClick(link) {
        var chunk = link;
        var requestedRange = this.calculateRequestedRange(chunk);

        this.set('currentChunk', chunk);
        this.sendAction('sectionRequest', requestedRange.low);
      },

      prevLinkClick: function prevLinkClick() {
        if (!this.get('shouldPrevBeDisabled')) {
          var currentChunk = this.get('currentChunk');
          var newChunk = currentChunk - 1;
          var requestedRange = this.calculateRequestedRange(newChunk);

          this.set('currentChunk', newChunk);
          this.sendAction('sectionRequest', requestedRange.low);
        }
      },

      nextLinkClick: function nextLinkClick() {
        if (!this.get('shouldNextBeDisabled')) {
          var currentChunk = this.get('currentChunk');
          var newChunk = currentChunk + 1;
          var requestedRange = this.calculateRequestedRange(newChunk);

          this.set('currentChunk', newChunk);
          this.sendAction('sectionRequest', requestedRange.low);
        }
      }
    },

    /**
     * Lifecycle method. This is called only once upon instantiation and is not called when data has changed forcing a component
     * re-render. Because we are using a cached list, it only has to calculate the amount of potential pagination links once.
     *
     * @method init
     */
    init: function init() {
      this._super();
      this.createPaginationLinks();
    },

    /**
     * Lifecycle method. This is called every time new data is fed into the component.
     * Current chunk is set on various actions, data is fetched as a result of that action, the component receives new data,
     * and this method is invoked.
     *
     * @method didRender
     */
    didRender: function didRender() {
      this.updateSelectedClass();
    },

    /**
     * Figures out what the item range for a given chunk based on the chunk size.
     * If current chunk is 3 and paginating every ten items, the object returns the range 31-40
     *
     * @method calculateRequestedRange
     * @private
     * @param chunk {String}
     * @return {Object} Contains low and high properties. i.e. { low: 31, high: 40 }
     */
    calculateRequestedRange: function calculateRequestedRange(chunk) {
      var chunkSize = this.get('chunkSize');

      return {
        low: chunk * chunkSize - chunkSize + 1,
        high: chunk * chunkSize
      };
    },

    /**
     * Determines the total number of links needed to be created given the total length and chunk size.
     *
     * @method calculateNumberLinksCount
     * @private
     * @return {Integer}
     */
    calculateNumberLinksCount: function calculateNumberLinksCount() {
      var linkCount = Math.ceil(this.get('totalSize') / this.get('chunkSize'));

      return this.set('numberLinksCount', linkCount);
    },

    /**
     * Operational method that hydrates the numberLinks array.
     *
     * @method createPaginationLinks
     * @private
     */
    createPaginationLinks: function createPaginationLinks() {
      this.calculateNumberLinksCount();

      if (this.get('shouldShowPaginationLinks')) {
        // reset numberLinks array
        this.set('numberLinks', []);

        // We want the loop to be 1 indexed, not 0
        for (var i = 1; i < this.get('numberLinksCount') + 1; i++) {
          this.numberLinks.push(i);
        }
      }
    },

    /**
     * Sets the selected class on the current pagination link item in the DOM. Using jQuery to mutate state is not ideal,
     * but since this state is contained within the component, and because we don't have conditional logic in handlebars,
     * this is the cleanest way to handle this.
     *
     * @method updateSelectedClass
     * @private
     */
    updateSelectedClass: function updateSelectedClass() {
      var self = this;
      var numberLinks = this.$().find('.pagination-link.number-link');

      // Remove the selected class
      numberLinks.removeClass('selected');

      // Add selected to the correct link
      numberLinks.filter(function (index) {
        var oneBasedIndex = index + 1;

        return oneBasedIndex === self.get('currentChunk');
      }).addClass('selected');
    },

    /**
     * Determines whether or not the UI should show pagination links. This is used by the handlebars templates.
     * returns false if there is only one page and pagination is not needed.
     *
     * @method shouldShowPaginationLinks
     * @return {Boolean}
     */
    shouldShowPaginationLinks: (function () {
      return this.get('numberLinksCount') > 1;
    }).property('numberLinksCount'),

    /**
     * Determines whether or not the previous button should disabled. This is used by the handlebars templates.
     * Returns true if the current page is 1, because there is not previous page at that point.
     *
     * @method shouldPrevBeDisabled
     * @return {Boolean}
     */
    shouldPrevBeDisabled: (function () {
      return this.get('currentChunk') <= 1;
    }).property('currentChunk'),

    /**
     * Determines whether or not the next button should disabled. This is used by the handlebars templates.
     * Returns true if the current page is the last item in the list, because there is not previous page at that point.
     *
     * @method shouldNextBeDisabled
     * @return {Boolean}
     */
    shouldNextBeDisabled: (function () {
      return this.numberLinks.length === this.get('currentChunk');
    }).property('currentChunk')
  });
});
define('ember-riak-explorer/components/power-select/before-options', ['exports', 'ember-power-select/components/power-select/before-options'], function (exports, _emberPowerSelectComponentsPowerSelectBeforeOptions) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectComponentsPowerSelectBeforeOptions['default'];
    }
  });
});
define('ember-riak-explorer/components/power-select/multiple/selected', ['exports', 'ember-power-select/components/power-select/multiple/selected'], function (exports, _emberPowerSelectComponentsPowerSelectMultipleSelected) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectComponentsPowerSelectMultipleSelected['default'];
    }
  });
});
define('ember-riak-explorer/components/power-select/multiple', ['exports', 'ember-power-select/components/power-select/multiple'], function (exports, _emberPowerSelectComponentsPowerSelectMultiple) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectComponentsPowerSelectMultiple['default'];
    }
  });
});
define('ember-riak-explorer/components/power-select/options', ['exports', 'ember-power-select/components/power-select/options'], function (exports, _emberPowerSelectComponentsPowerSelectOptions) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectComponentsPowerSelectOptions['default'];
    }
  });
});
define('ember-riak-explorer/components/power-select/single/selected', ['exports', 'ember-power-select/components/power-select/single/selected'], function (exports, _emberPowerSelectComponentsPowerSelectSingleSelected) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectComponentsPowerSelectSingleSelected['default'];
    }
  });
});
define('ember-riak-explorer/components/power-select/single', ['exports', 'ember-power-select/components/power-select/single'], function (exports, _emberPowerSelectComponentsPowerSelectSingle) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectComponentsPowerSelectSingle['default'];
    }
  });
});
define('ember-riak-explorer/components/power-select', ['exports', 'ember-power-select/components/power-select'], function (exports, _emberPowerSelectComponentsPowerSelect) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectComponentsPowerSelect['default'];
    }
  });
});
define('ember-riak-explorer/components/riak-buckets', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    actions: {
      deleteBucket: function deleteBucket(bucket) {
        // Send the action to parent controller
        this.sendAction('deleteBucketAction', bucket);
      }
    }
  });
});
define('ember-riak-explorer/components/riak-keys', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/riak-node', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    tagName: 'tr'
  });
});
define('ember-riak-explorer/components/search-indexes', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/components/view-label', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({
    classNames: ['view-label']
  });
});
define('ember-riak-explorer/components/wrapper-panel', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Component.extend({});
});
define('ember-riak-explorer/controllers/application', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller.extend({
    currentCluster: null,

    /**
     * Observes routes and makes sure that if on the index route, to set the current cluster state
     *  to null. Since all routes are nested under the index route, this is the only way we can
     *  reliably ensure that this method is called every time the route is visited.
     *
     * @method setSidebarState
     */
    setSidebarState: (function () {
      switch (this.get('currentPath')) {
        case 'index':
          this.set('currentCluster', null);
          break;
      }
    }).observes('currentPath'),

    actions: {
      clusterSelected: function clusterSelected(cluster) {
        this.set('currentCluster', cluster);
        // Default to data view since no cluster overview view
        this.transitionToRoute('cluster.data', cluster.get('id'));
      }
    }
  });
});
define('ember-riak-explorer/controllers/array', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller;
});
define('ember-riak-explorer/controllers/error/cluster-not-found', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller.extend({
    queryParams: ['cluster_id'],
    cluster_id: null
  });
});
define('ember-riak-explorer/controllers/error/object-not-found', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller.extend({
    queryParams: ['clusterId', 'bucketTypeId', 'bucketId', 'key'],
    clusterId: null,
    bucketTypeId: null,
    bucketId: null,
    key: null
  });
});
define('ember-riak-explorer/controllers/error/unknown', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller.extend({});
});
define('ember-riak-explorer/controllers/explorer-api', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller.extend({
    pageTitle: 'Riak Explorer API'
  });
});
define('ember-riak-explorer/controllers/object', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller;
});
define('ember-riak-explorer/helpers/ember-power-select-option-classes', ['exports', 'ember-power-select/helpers/ember-power-select-option-classes'], function (exports, _emberPowerSelectHelpersEmberPowerSelectOptionClasses) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectHelpersEmberPowerSelectOptionClasses['default'];
    }
  });
  Object.defineProperty(exports, 'emberPowerSelectOptionClasses', {
    enumerable: true,
    get: function get() {
      return _emberPowerSelectHelpersEmberPowerSelectOptionClasses.emberPowerSelectOptionClasses;
    }
  });
});
define('ember-riak-explorer/helpers/hash', ['exports', 'ember-hash-helper-polyfill/helpers/hash'], function (exports, _emberHashHelperPolyfillHelpersHash) {
  Object.defineProperty(exports, 'default', {
    enumerable: true,
    get: function get() {
      return _emberHashHelperPolyfillHelpersHash['default'];
    }
  });
  Object.defineProperty(exports, 'hash', {
    enumerable: true,
    get: function get() {
      return _emberHashHelperPolyfillHelpersHash.hash;
    }
  });
});
define('ember-riak-explorer/initialize', ['exports', 'ember', 'ember-idx-utils/config'], function (exports, _ember, _emberIdxUtilsConfig) {
  exports['default'] = {
    name: 'ember-idx-utils',
    initialize: function initialize() {
      if (!_ember['default'].IdxConfig) {
        _ember['default'].IdxConfig = _emberIdxUtilsConfig['default'].create();
      }
    }
  };
});
define('ember-riak-explorer/initializers/app-version', ['exports', 'ember-cli-app-version/initializer-factory', 'ember-riak-explorer/config/environment'], function (exports, _emberCliAppVersionInitializerFactory, _emberRiakExplorerConfigEnvironment) {
  exports['default'] = {
    name: 'App Version',
    initialize: (0, _emberCliAppVersionInitializerFactory['default'])(_emberRiakExplorerConfigEnvironment['default'].APP.name, _emberRiakExplorerConfigEnvironment['default'].APP.version)
  };
});
define('ember-riak-explorer/initializers/explorer', ['exports'], function (exports) {
  exports.initialize = initialize;

  function initialize(container, app) {
    // inject explorer service into all routes
    app.inject('route', 'explorer', 'service:explorer');
    // inject the store into the explorer service
    app.inject('service:explorer', 'store', 'store:main');
  }

  exports['default'] = {
    name: 'explorer',
    initialize: initialize
  };
});
define('ember-riak-explorer/initializers/export-application-global', ['exports', 'ember', 'ember-riak-explorer/config/environment'], function (exports, _ember, _emberRiakExplorerConfigEnvironment) {
  exports.initialize = initialize;

  function initialize() {
    var application = arguments[1] || arguments[0];
    if (_emberRiakExplorerConfigEnvironment['default'].exportApplicationGlobal !== false) {
      var value = _emberRiakExplorerConfigEnvironment['default'].exportApplicationGlobal;
      var globalName;

      if (typeof value === 'string') {
        globalName = value;
      } else {
        globalName = _ember['default'].String.classify(_emberRiakExplorerConfigEnvironment['default'].modulePrefix);
      }

      if (!window[globalName]) {
        window[globalName] = application;

        application.reopen({
          willDestroy: function willDestroy() {
            this._super.apply(this, arguments);
            delete window[globalName];
          }
        });
      }
    }
  }

  exports['default'] = {
    name: 'export-application-global',

    initialize: initialize
  };
});
define('ember-riak-explorer/initializers/idx-tabs', ['exports', 'ember', 'ember-idx-utils/config'], function (exports, _ember, _emberIdxUtilsConfig) {
    exports['default'] = {
        name: 'ember-idx-tabs',
        initialize: function initialize() {
            var Config = _ember['default'].IdxConfig = _ember['default'].IdxConfig ? _ember['default'].IdxConfig : _emberIdxUtilsConfig['default'].create();

            var defaultConfig = Config.getConfig('default');
            if (!defaultConfig) {
                Config.addConfig('default');
                defaultConfig = Config.getConfig('default');
            }

            defaultConfig['tabs'] = {
                tabsTag: ['div'],
                tabTag: ['li'],
                tabListTag: ['ul'],
                tabsClasses: ['em-tabs'],
                tabClasses: ['em-tab'],
                tabListClasses: ['em-tab-list'],
                tabPanelClasses: ['em-tab-panel']
            };

            //Bootstrap
            var bsConfig = Config.getConfig('bs');
            if (!bsConfig) {
                Config.addConfig('bs');
                bsConfig = Config.getConfig('bs');
            }
            bsConfig['tabs'] = {
                tabTag: ['li'],
                tabListTag: ['ul'],
                tabListClasses: ['nav', 'nav-tabs'],
                tabPanelClasses: ['em-tab-panel'],
                tabSelectedClasses: ['active']
            };
        }
    };
});
define('ember-riak-explorer/mixins/sidebar-select', ['exports', 'ember'], function (exports, _ember) {

  /**
   * Mixin to allow routes to set sidebar state.
   *
   * @mixin SidebarSelect
   */
  exports['default'] = _ember['default'].Mixin.create({
    setSidebarCluster: function setSidebarCluster(cluster) {
      this.controllerFor('application').set('currentCluster', cluster);
    }
  });
});
define('ember-riak-explorer/models/bucket-list', ['exports', 'ember-data', 'ember-riak-explorer/models/cached-list'], function (exports, _emberData, _emberRiakExplorerModelsCachedList) {

  /**
   * Represents a list of buckets in the current bucket type,
   * cached by the Explorer API.
   *
   * @class BucketList
   * @extends CachedList
   * @constructor
   * @uses BucketType
   * @uses Bucket
   * @uses Cluster
   */
  var BucketList = _emberRiakExplorerModelsCachedList['default'].extend({
    /**
     * The bucket type that owns this bucket list.
     * @property bucketType
     * @type BucketType
     */
    bucketType: _emberData['default'].belongsTo('bucket-type'),

    /**
     * The cluster in which this bucket type resides.
     * @property cluster
     * @type Cluster
     */
    cluster: _emberData['default'].belongsTo('cluster'),

    /**
     * List of Bucket model instances (loaded from the server)
     * @property buckets
     * @type Array<Bucket>
     * @default []
     */
    buckets: _emberData['default'].attr(null, { defaultValue: [] }),

    /**
     * @property bucketTypeId
     * @type String
     */
    bucketTypeId: (function () {
      return this.get('bucketType').get('bucketTypeId');
    }).property('bucketType'),

    /**
     * @property clusterId
     * @type String
     */
    clusterId: (function () {
      return this.get('cluster').get('clusterId');
    }).property('cluster')
  });

  exports['default'] = BucketList;
});
define('ember-riak-explorer/models/bucket-props', ['exports', 'ember-data', 'ember', 'ember-riak-explorer/utils/riak-util'], function (exports, _emberData, _ember, _emberRiakExplorerUtilsRiakUtil) {

  /**
   * Represents a Bucket's or a Bucket Type's properties.
   * Since a Bucket inherits all of its parent Bucket Type's properties, both
   *    models use this object to store their properties.
   *
   * @see http://docs.basho.com/riak/latest/theory/concepts/Buckets/
   * @see http://docs.basho.com/riak/latest/dev/references/http/set-bucket-props/
   * @see http://docs.basho.com/riak/latest/dev/advanced/bucket-types/
   * @see http://docs.basho.com/riak/latest/dev/advanced/replication-properties/
   *
   * @class BucketProps
   * @extends DS.Model
   * @constructor
   */
  var BucketProps = _emberData['default'].Model.extend({
    /**
     * Hash of key/value pairs, obtained as a result of
     *    an HTTP GET Bucket Properties API call,
     *    or a GET Bucket Type Properties API call
     *
     * @property props
     * @type Hash
     * @example
     *     { "allow_mult":false, "basic_quorum":false, "write_once": false, ... }
     */
    props: _emberData['default'].attr(),

    /**
     * Returns a capitalized name of the Riak Data Type stored in this bucket
     *    or bucket type (if this is a CRDT type bucket).
     * @see http://docs.basho.com/riak/latest/dev/using/data-types/
     * @see http://docs.basho.com/riak/latest/theory/concepts/crdts/
     *
     * @method dataTypeName
     * @return {String|Null} One of: [ 'Map', 'Set', 'Counter', null ]
     */
    dataTypeName: (function () {
      var name;
      if (this.get('isCRDT')) {
        name = this.get('props').datatype;
      }
      if (name) {
        return name.capitalize();
      }
    }).property('props'),

    /**
     * Does this bucket or bucket type have custom pre-commit or post-commit
     *     hooks enabled?
     * @see http://docs.basho.com/riak/latest/dev/using/commit-hooks/
     *
     * @method hasCommitHooks
     * @return {Boolean}
     */
    hasCommitHooks: (function () {
      var hasPrecommit = !_ember['default'].isEmpty(this.get('props').precommit);
      var hasPostcommit = !_ember['default'].isEmpty(this.get('props').postcommit);

      return hasPrecommit || hasPostcommit;
    }).property('props'),

    /**
     * Have Siblings been enabled for this Bucket or Bucket Type?
     * Returns `false` by default if this is a bucket within the `default`
     * Bucket Type.
     * Otherwise (for any user-defined type) returns `true` by default.
     * @see http://docs.basho.com/riak/latest/dev/using/conflict-resolution/#Siblings
     *
     * @method hasSiblings
     * @return {Boolean}
     */
    hasSiblings: (function () {
      return this.get('props').allow_mult;
    }).property('props'),

    /**
     * Has this Bucket Type been activated via `riak-admin bucket-types activate`?
     * (Buckets inherit this setting from their parent bucket types.)
     *
     * @property isActive
     * @type Boolean
     */
    isActive: (function () {
      return this.get('props').active;
    }).property('props'),

    /**
     * Does this bucket store Counter data type objects?
     *
     * @method isCounter
     * @return {Boolean}
     */
    isCounter: (function () {
      return this.get('dataTypeName') === 'Counter';
    }).property('dataTypeName'),

    /**
     * Does this bucket type store Riak Data Type objects?
     * @see http://docs.basho.com/riak/latest/dev/using/data-types/
     * @see http://docs.basho.com/riak/latest/theory/concepts/crdts/
     *
     * @method isCRDT
     * @return {Boolean}
     */
    isCRDT: (function () {
      return this.get('props').datatype;
    }).property('props'),

    /**
     * Has the 'Last Write Wins' optimization been turned on for this bucket?
     * @see http://docs.basho.com/riak/latest/dev/using/conflict-resolution/#last-write-wins
     *
     * @method isLWW
     * @return {Boolean}
     */
    isLWW: (function () {
      return this.get('props').last_write_wins;
    }).property('props'),

    /**
     * Does this bucket store Map data type objects?
     *
     * @method isMap
     * @return {Boolean}
     */
    isMap: (function () {
      return this.get('dataTypeName') === 'Map';
    }).property('dataTypeName'),

    /**
     * Has a Riak Search index been associated with this bucket type?
     *
     * @method isSearchIndexed
     * @return {Boolean}
     */
    isSearchIndexed: (function () {
      return !!this.get('props').search_index;
    }).property('searchIndexName'),

    /**
     * Does this bucket store Set data type objects?
     *
     * @method isSet
     * @return {Boolean}
     */
    isSet: (function () {
      return this.get('dataTypeName') === 'Set';
    }).property('dataTypeName'),

    /**
     * Has Strong Consistency been enabled for this bucket type?
     * @see http://docs.basho.com/riak/latest/dev/advanced/strong-consistency/
     *
     * @method isStronglyConsistent
     * @return {Boolean}
     */
    isStronglyConsistent: (function () {
      return this.get('props').consistent;
    }).property('props'),

    /**
     * Has the 'Write Once' setting been enabled for this bucket type?
     * (This feature was introduced in Riak 2.1)
     * @see http://docs.basho.com/riak/latest/dev/advanced/write-once/
     *
     * @method isWriteOnce
     * @return {Boolean}
     */
    isWriteOnce: (function () {
      return this.get('props').write_once;
    }).property('props'),

    /**
     * Returns the N value (number of object replicas) setting for this bucket type.
     * (Default is 3).
     * @see http://docs.basho.com/riak/latest/dev/advanced/replication-properties/
     *
     * @property nVal
     * @type Number
     */
    nVal: (function () {
      return this.get('props').n_val;
    }).property('props'),

    objectType: (function () {
      var type = [];

      if (this.get('isCRDT')) {
        type.push(this.get('dataTypeName'));
      } else {
        type.push('Default');
      }

      if (this.get('isSearchIndexed')) {
        type.push('Search Indexed');
      }

      return type.join(', ');
    }).property('props'),

    /**
     * Returns a flat list of properties, used for display on a View Properties
     *     page.
     *
     * @method propsList
     * @return {Array<Hash>} List of key/value pairs
     */
    propsList: (function () {
      if (!this.get('props')) {
        return [];
      }
      return (0, _emberRiakExplorerUtilsRiakUtil['default'])(this.get('props'));
    }).property('props'),

    /**
     * Returns a hash containing quorum-related settings.
     * @see http://docs.basho.com/riak/latest/dev/advanced/replication-properties/
     *
     * @method quorum
     * @return {Hash}
     */
    quorum: (function () {
      return {
        r: this.get('props').r, // Read quorum
        w: this.get('props').r, // Write Quorum
        pr: this.get('props').pr, // Primary Read
        pw: this.get('props').pw, // Primary Write
        dw: this.get('props').dw, // Durable Write
        basic_quorum: this.get('props').basic_quorum,
        notfound_ok: this.get('props').notfound_ok
      };
    }).property('props'),

    /**
     * Returns true if this is an Eventually Consistent object type
     *    (versus Strongly Consistent type or a CRDT), and therefore the notion
     *    of 'Quorum' applies.
     *
     * @method quorumRelevant
     * @return {Boolean}
     */
    quorumRelevant: (function () {
      return !this.get('isStronglyConsistent') && !this.get('isCRDT');
    }).property('props'),

    /**
     * Returns a human-readable description of the conflict resolution strategy
     *   for this bucket type or bucket.
     *
     * @method resolutionStrategy
     * @return {String}
     */
    resolutionStrategy: (function () {
      var strategy = null;

      switch (true) {
        case this.get('isStronglyConsistent'):
          strategy = 'Strongly Consistent';
          break;
        case this.get('isCounter'):
          strategy = 'Convergent, Pairwise Maximum Wins';
          break;
        case this.get('isMap'):
          strategy = 'Convergent, Add/Update Wins Over Remove';
          break;
        case this.get('isSet'):
          strategy = 'Convergent, Add Wins Over Remove';
          break;
        case this.get('hasSiblings'):
          strategy = 'Causal Context (Siblings Enabled)';
          break;
        case this.get('isWriteOnce'):
          strategy = 'n/a (Write-Once Optimized)';
          break;
        case this.get('isLWW'):
          strategy = 'Wall Clock (LastWriteWins enabled)';
          break;
        default:
          strategy = 'Causal Context (Siblings Off, fallback to Wall Clock)';
      }

      return strategy;
    }).property('props'),

    /**
     * Returns the name of the Search Index set on this bucket type or bucket
     * @see http://docs.basho.com/riak/latest/dev/using/search/
     *
     * @method searchIndexName
     * @return {String|Null}
     */
    searchIndexName: (function () {
      return this.get('props').search_index;
    }).property('props'),

    /**
     * Returns human-readable warnings related to this bucket's settings.
     *
     * @method warnings
     * @return {Array<String>}
     */
    warnings: (function () {
      var warnings = [];

      if (this.get('isStronglyConsistent')) {
        if (this.get('nVal') < 5) {
          warnings.push('Using Strong Consistency, but n_val < 5!');
        }
        if (this.get('isSearchIndexed')) {
          warnings.push('Combining Strong Consistency with Search. Use cation!');
        }
        if (this.get('hasCommitHooks')) {
          warnings.push('Using commit hooks, but those are ignored for Strongly Consistent data!');
        }
      }
      if (this.get('hasSiblings')) {
        // Siblings enabled
        if (!this.get('props').dvv_enabled) {
          warnings.push('Dotted Version Vectors (dvv_enabled) should be enabled when Siblings are enabled.');
        }
      }
      return warnings;
    }).property('props')
  });

  exports['default'] = BucketProps;
});
define('ember-riak-explorer/models/cached-list', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Represents a key list or a bucket list that's cached on disk by Explorer API.
   *
   * @class CachedList
   * @extends DS.Model
   * @constructor
   */
  var CachedList = _emberData['default'].Model.extend({
    /**
     * Is an API-side cache present, for this list?
     * In Development Mode, if a cache is not present, the client will
     * automatically kick off a cache refresh (from a streaming list keys, for
     * example).
     * In Production Mode, if there is no cache, an appropriate message will
     * be displayed.
     * @property cachePresent
     * @type Boolean
     * @default true
     */
    cachePresent: _emberData['default'].attr('boolean', { defaultValue: true }),

    /**
     * Number of items displayed on the current page of the list
     * @property count
     * @type Number
     * @default 0
     */
    count: _emberData['default'].attr('number', { defaultValue: 0 }),

    /**
     * Timestamp of when the cached list was generated on the server side
     * @property created
     * @type String
     */
    created: _emberData['default'].attr(),

    /**
     * Is the List operation waiting for a cache to be generated?
     * @property isLoaded
     * @type Boolean
     * @default false
     */
    isLoaded: _emberData['default'].attr('boolean', { defaultValue: false }),

    /**
     * The index of the first item in the current page, in relation to the entire list
     * @property firstItemIndex
     * @type Integer
     */
    firstItemIndex: _emberData['default'].attr('number', { defaultValue: 1 }),

    /**
     * The number of items per page
     * @property pageSize
     * @type Integer
     */
    pageSize: _emberData['default'].attr('number', { defaultValue: 0 }),

    /**
     * Status message to display to the user. Relevant for long-running
     * server operations such as loading large lists or refreshing cached lists.
     * Sample messages:
     *
     *  - 'Requesting cached list...'
     *  - 'Cache not found. Refreshing from a streaming list keys/buckets call...'
     * @property statusMessage
     * @type String
     */
    statusMessage: _emberData['default'].attr('string', { defaultValue: 'Requesting cached list...' }),

    /**
     * Total number of items in the cached list on the API side.
     * @property total
     * @type Number
     * @default 0
     */
    total: _emberData['default'].attr('number', { defaultValue: 0 }),

    /**
     * The index of the last item in the current page, in relation to the entire list
     *
     * @method lastItemIndex
     * @returns Integer
     */
    lastItemIndex: (function () {
      return this.get('firstItemIndex') + this.get('count') - 1;
    }).property('firstItemIndex', 'count'),

    /**
     * Whether or not the current page has more than 1 item in it
     *
     * @method lastItemIndex
     * @returns Boolean
     */
    multipleListItems: (function () {
      return this.get('count') > 1;
    }).property('count')
  });

  exports['default'] = CachedList;
});
define('ember-riak-explorer/models/key-list', ['exports', 'ember-data', 'ember-riak-explorer/models/cached-list'], function (exports, _emberData, _emberRiakExplorerModelsCachedList) {

  /**
   * Represents a bucket's key list, cached by the Explorer API.
   *
   * @class KeyList
   * @extends CachedList
   * @constructor
   * @uses Bucket
   * @uses Cluster
   */
  var KeyList = _emberRiakExplorerModelsCachedList['default'].extend({
    /**
     * Bucket for which this key list was generated.
     * @property bucket
     * @type Bucket
     */
    bucket: _emberData['default'].attr(),

    /**
     * Cluster in which the bucket resides.
     * @property cluster
     * @type Cluster
     */
    cluster: _emberData['default'].attr(),

    /**
     * List of keys (actually, RiakObject instances) for this page
     * @property keys
     * @type Array<RiakObject>
     * @default []
     */
    keys: _emberData['default'].attr(null, { defaultValue: [] }),

    /**
     * @method bucketId
     * @type String
     */
    bucketId: (function () {
      return this.get('bucket').get('bucketId');
    }).property('bucket'),

    /**
     * @method bucketTypeId
     * @type String
     */
    bucketTypeId: (function () {
      return this.get('bucket').get('bucketTypeId');
    }).property('bucket'),

    /**
     * @method clusterId
     * @type String
     */
    clusterId: (function () {
      return this.get('cluster').get('clusterId');
    }).property('cluster'),

    /**
     * Returns true if this list has a nonzero key count.
     * @method hasKeys
     * @return {Boolean}
     */
    hasKeys: (function () {
      return this.get('count') > 0;
    }).property('count'),

    /**
     * Returns whether or not the 'Delete All Keys in Bucket' button
     *    should be displayed to the user.
     * @method showDeleteKeys
     * @return {Boolean}
     */
    showDeleteKeys: (function () {
      return this.get('cluster').get('developmentMode') && this.get('hasKeys');
    }).property('cluster', 'count')
  });

  exports['default'] = KeyList;
});
define('ember-riak-explorer/models/link', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Represents JSON-API style 'links' (for example, 'self').
   * Not currently used for much.
   *
   * @class Link
   * @extends DS.Model
   * @constructor
   */
  exports['default'] = _emberData['default'].Model.extend({
    related: _emberData['default'].attr(),
    self: _emberData['default'].attr()
  });
});
define('ember-riak-explorer/models/object-metadata', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Riak Object metadata and custom headers.
   * @see RiakObject
   *
   * @class ObjectMetadata
   * @extends DS.Model
   * @constructor
   */
  var ObjectMetadata = _emberData['default'].Model.extend({
    /**
     * Hash containing an object's metadata/headers.
     * Divided into three categories:
     *   1. 'custom' - user-defined custom headers
     *        (in the HTTP API, these start with `x-riak-meta-`).
     *        Stored as an array of headers (simple key/value hashes) for
     *        easy listing on the Edit Object screen.
     *   2. 'indexes' - Secondary Indexes
     *        Stored as an array of headers (simple key/value hashes) for
     *        easy listing on the Edit Object screen.
     *   3. 'other' - Standard object metadata (x-riak-vclock, etag, etc).
     *        Stored as a hash of keys/values (not a list, since these are fixed)
     *
     * @see http://docs.basho.com/riak/latest/dev/references/http/fetch-object/
     * @see http://docs.basho.com/riak/latest/dev/references/http/store-object/
     * @see http://docs.basho.com/riak/latest/dev/references/http/secondary-indexes/
     *
     * @property headers
     * @type Hash
     * @default { custom:[], indexes:[], other:{} }
     */
    headers: _emberData['default'].attr(null, {
      defaultValue: {
        custom: [], // x-riak-meta-*
        indexes: [], // x-riak-index-*
        other: {} // everything else
      }
    }),

    /**
     * Causal context header, used for server-side conflict resolution.
     * This is opaque to the client; the important thing is to load it
     * by reading first, before any sort of edit operation to an object.
     * @see http://docs.basho.com/riak/latest/dev/using/conflict-resolution/#Causal-Context
     *
     * @property causalContext
     * @readOnly
     * @type String
     */
    causalContext: (function () {
      return this.get('headers').other['x-riak-vclock'];
    }).property('headers'),

    /**
     * HTTP Content-Type of the object (see section 14.17 of RFC 2616),
     * specified by the user when writing the object.
     * @property contentType
     * @type String
     */
    contentType: (function () {
      return this.get('headers').other['content-type'];
    }).property('headers'),

    /**
     * Last-Modified timestamp.
     * Useful for conditional GET operations and caching.
     * @property contentType
     * @readOnly
     * @type String
     */
    dateLastModified: (function () {
      return this.get('headers').other['last-modified'];
    }).property('headers'),

    /**
     * Date on which this object was loaded from Riak (via an HTTP request).
     * Used to give the user a sense of when the 'View Object' page was last
     * refreshed.
     * @property dateLoaded
     * @readOnly
     * @type String
     */
    dateLoaded: (function () {
      return this.get('headers').other['date'];
    }).property('headers'),

    /**
     * HTTP Etag (entity tag). Unique identifier for this object and contents.
     * Useful for conditional GET operations and validation-based caching.
     * @property dateLoaded
     * @readOnly
     * @type String
     */
    etag: (function () {
      return this.get('headers').other['etag'];
    }).property('headers'),

    /**
     * List of custom (user-specified) headers.
     * Mainly useful to "Tag" binary objects and enable Search to index them.
     * @property headersCustom
     * @type Array<Hash>
     * @example
     *     [ { "x-riak-meta-user_id": "user123" }]
     */
    headersCustom: (function () {
      return this.get('headers').custom;
    }).property('headers'),

    /**
     * Re-assembles relevant object headers, such as the causal context and
     * any user-edited headers like secondary indexes or custom metadata.
     * Used when saving/updating an object.
     * @see http://docs.basho.com/riak/latest/dev/references/http/store-object/
     * @see http://docs.basho.com/riak/latest/dev/references/http/secondary-indexes/
     *
     * @method headersForUpdate
     * @return {Hash} Headers object suitable for a jQuery AJAX PUT request
     */
    headersForUpdate: (function () {
      // Start with the causal context
      var headers = {
        'X-Riak-Vclock': this.get('causalContext')
      };
      var header;
      var i;
      // Add the 2i indexes, if applicable
      var indexes = this.get('headersIndexes');
      for (i = 0; i < indexes.length; i++) {
        header = indexes[i];
        headers[header.key] = header.value;
      }
      // Add the user-defined custom headers
      var customHeaders = this.get('headersCustom');
      for (i = 0; i < customHeaders.length; i++) {
        header = customHeaders[i];
        headers[header.key] = header.value;
      }
      return headers;
    }).property('headers'),

    /**
     * List of user-defined Secondary Indexes for this object.
     * @see http://docs.basho.com/riak/latest/dev/references/http/secondary-indexes/
     * @property headersIndexes
     * @type Array<Hash>
     */
    headersIndexes: (function () {
      return this.get('headers').indexes;
    }).property('headers'),

    /**
     * Has this object been deleted, cluster-side?
     * Generally only encountered if `delete_mode` is set to 'keep',
     *   or if a tombstone is one of the object's siblings.
     * @see http://docs.basho.com/riak/latest/ops/advanced/deletion/
     * @see http://docs.basho.com/riak/latest/dev/references/http/delete-object/
     *
     * @property isDeleted
     * @type String
     * @readOnly
     */
    isDeleted: (function () {
      return this.get('headers').other['x-riak-deleted'];
    }).property('headers')
  });

  exports['default'] = ObjectMetadata;
});
define('ember-riak-explorer/models/route', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Represents Explorer API endpoints/routes.
   *
   * @class Route
   * @extends DS.Model
   * @constructor
   */
  exports['default'] = _emberData['default'].Model.extend({
    links: _emberData['default'].attr(),
    resources: _emberData['default'].attr()
  });
});
define('ember-riak-explorer/pods/bucket/controller', ['exports', 'ember'], function (exports, _ember) {

  var BucketController = _ember['default'].Controller.extend({
    explorer: _ember['default'].inject.service('explorer'),

    /**
     * Kicks off a model refresh after the specified delay.
     * Initially called by +BucketRoute.setupController+.
     *
     * @method pollForModel
     * @param bucket {Bucket}
     * @param delay {Number} Milliseconds to wait before refreshing the model
     *                       (to see if the key list has loaded)
     */
    pollForModel: function pollForModel(bucket, delay) {
      var self = this;
      _ember['default'].run.later(function () {
        self.refreshModel(bucket);
      }, delay);
    },

    /**
     * Reloads the model (a bucket, its properties and key list) from the server.
     * If the key list is not ready, try again after a delay.
     *
     * @method refreshModel
     * @param bucket {Bucket}
     */
    refreshModel: function refreshModel(bucket) {
      var self = this;
      var cluster = bucket.get('cluster');

      self.get('explorer').getKeyList(bucket).then(function (updatedKeyList) {
        // The key list could be either loaded or empty at this point
        bucket.set('keyList', updatedKeyList);
        // If the key list is empty/not-loaded, poll for it again
        if (!bucket.get('isKeyListLoaded') && updatedKeyList.get('cachePresent')) {
          self.pollForModel(bucket, 3000);
        }
      });
    },

    actions: {
      retrieveRequestedKeys: function retrieveRequestedKeys(startIndex) {
        var service = this.get('explorer');
        var bucket = this.get('model');

        return service.getBucketWithKeyList(bucket, startIndex);
      },

      deleteBucket: function deleteBucket(bucket) {
        bucket.set('isKeyListLoaded', false);
        this.get('explorer').deleteBucket(bucket);

        // Reload the model after the delete, triggers a cache refresh
        this.pollForModel(bucket, 5000);

        // Reload the second time
        this.pollForModel(bucket, 10000);
      },

      refreshKeys: function refreshKeys(bucket) {
        var clusterId = bucket.get('clusterId');
        var bucketTypeId = bucket.get('bucketTypeId');
        var bucketId = bucket.get('bucketId');

        bucket.set('isKeyListLoaded', false);
        this.get('explorer').keyCacheRefresh(clusterId, bucketTypeId, bucketId);
        this.pollForModel(bucket, 3000);
      }
    }
  });

  exports['default'] = BucketController;
});
define('ember-riak-explorer/pods/bucket/model', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Represents a Riak Bucket
   *
   * @class Bucket
   * @extends DS.Model
   * @constructor
   * @uses BucketType
   * @uses Cluster
   * @uses BucketProps
   * @uses KeyList
   */
  var Bucket = _emberData['default'].Model.extend({
    /**
     * Initializes a new Bucket instance by setting up an empty
     * KeyList.
     * @method init
     */
    init: function init() {
      this._super();
      var emptyList = this.store.createRecord('key-list', {
        cluster: this.get('cluster'),
        keys: []
      });
      this.set('keyList', emptyList);
    },

    /**
     * Riak Bucket Type in which this bucket lives.
     *
     * @property bucketType
     * @type BucketType
     * @writeOnce
     */
    bucketType: _emberData['default'].belongsTo('bucket-type'),

    /**
     * Riak cluster in which this bucket lives.
     *
     * @property cluster
     * @type Cluster
     * @writeOnce
     */
    cluster: _emberData['default'].belongsTo('cluster'),

    /**
     * Contains the results of cached key lists for this bucket,
     * fetched from the API.
     *
     * @property key-list
     * @type KeyList
     */
    keyList: _emberData['default'].belongsTo('key-list'),

    /**
     * Bucket Properties object. Note: Bucket Types and Buckets share the
     *    same Properties format.
     * When not specified, buckets inherit their properties from the Bucket Type
     *
     * @property props
     * @type BucketProps
     */
    props: _emberData['default'].belongsTo('bucket-props'),

    /**
     * Has the keyList been loaded from the server?
     *
     * @property isKeyListLoaded
     * @type Boolean
     * @default false
     */
    isKeyListLoaded: _emberData['default'].attr('boolean', { defaultValue: false }),

    /**
     * Bucket name (unique within a cluster and bucket type)
     *
     * @property name
     * @type String
     */
    name: _emberData['default'].attr('string'),

    /**
     * Returns the bucket name (this is an alias/helper function)
     *
     * @property bucketId
     * @type String
     */
    bucketId: (function () {
      return this.get('name');
    }).property('name'),

    /**
     * Returns the bucket type's name
     *
     * @property bucketTypeId
     * @type String
     */
    bucketTypeId: (function () {
      return this.get('bucketType').get('bucketTypeId');
    }).property('cluster'),

    /**
     * Returns the name of the cluster in which this bucket resides.
     * (As specified in the `riak_explorer.conf` file)
     *
     * @property clusterId
     * @type String
     */
    clusterId: (function () {
      return this.get('cluster').get('clusterId');
    }).property('cluster'),

    /**
     * Returns the name of the Search Index associated with this bucket
     * (or its parent bucket type), if applicable.
     *
     * @property index
     * @type String
     */
    index: (function () {
      return this.get('cluster').get('searchIndexes').findBy('name', this.get('props').get('searchIndexName'));
    }).property('cluster'),

    /**
     * Has this bucket type been activated?
     *
     * @property isActive
     * @type Boolean
     */
    isActive: (function () {
      if (this.get('bucketTypeId') === 'default') {
        // Buckets in the Default type don't have the 'active' attribute
        // in the props, but are actually active.
        return true;
      }
      return this.get('props').get('isActive');
    }).property('props'),

    /**
     * Returns the Ember.js/Ember Data model name of the objects stored within
     *     this bucket.
     *
     * @property objectModelName
     * @type String
     * @readOnly
     * @default 'riak-object'
     */
    objectModelName: (function () {
      var modelType = null;

      switch (true) {
        case this.get('props').get('isCounter'):
          modelType = 'riak-object.counter';
          break;
        case this.get('props').get('isSet'):
          modelType = 'riak-object.set';
          break;
        case this.get('props').get('isMap'):
          modelType = 'riak-object.map';
          break;
        default:
          modelType = 'riak-object';
          break;
      }

      return modelType;
    }).property('props'),

    /**
     * Returns this bucket's properties as an array of key/value tuples.
     * Used for displaying and editing the properties.
     *
     * @method propsList
     * @return {Array<Hash>}
     */
    propsList: (function () {
      if (!this.get('props')) {
        return [];
      }
      return this.get('props').get('propsList');
    }).property('props')
  });

  exports['default'] = Bucket;
});
define('ember-riak-explorer/pods/bucket/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    model: function model(params) {
      var explorer = this.explorer;

      return explorer.getBucket(params.clusterId, params.bucketTypeId, params.bucketId).then(function (bucket) {
        return explorer.getBucketWithKeyList(bucket);
      });
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));
    },

    /**
     * @method setupController
     * @param controller {BucketController}
     * @param model {Bucket}
     */
    setupController: function setupController(controller, model) {
      this._super(controller, model);
      // When user follows a bucket link from the Bucket Type view,
      //   the props are not yet initialized. Also, the model()
      //   function, above, is not called. Handle this case.
      if (_ember['default'].isEmpty(model.get('props'))) {
        this.explorer.getBucketProps(model.get('clusterId'), model.get('bucketTypeId'), model.get('bucketId')).then(function (bucketProps) {
          model.set('props', bucketProps);
        });
      }
      // Start fetching the key list
      if (!model.get('isKeyListLoaded')) {
        controller.pollForModel(model, 3000);
      }
    }
  });
});
define("ember-riak-explorer/pods/bucket/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          var child0 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 18,
                      "column": 10
                    },
                    "end": {
                      "line": 18,
                      "column": 29
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("Overview");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            var child1 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 19,
                      "column": 10
                    },
                    "end": {
                      "line": 19,
                      "column": 40
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("Advanced Properties");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 17,
                    "column": 8
                  },
                  "end": {
                    "line": 20,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createComment("");
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createComment("");
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(2);
                morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
                morphs[1] = dom.createMorphAt(fragment, 3, 3, contextualElement);
                return morphs;
              },
              statements: [["block", "em-tab", [], [], 0, null, ["loc", [null, [18, 10], [18, 40]]]], ["block", "em-tab", [], [], 1, null, ["loc", [null, [19, 10], [19, 51]]]]],
              locals: [],
              templates: [child0, child1]
            };
          })();
          var child1 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 22,
                    "column": 8
                  },
                  "end": {
                    "line": 24,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createComment("");
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
                return morphs;
              },
              statements: [["inline", "bucket-properties", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [23, 36], [23, 41]]]]], [], []]], ["loc", [null, [23, 10], [23, 43]]]]],
              locals: [],
              templates: []
            };
          })();
          var child2 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 28,
                      "column": 12
                    },
                    "end": {
                      "line": 33,
                      "column": 12
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
                },
                arity: 1,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("              ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("tr");
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2, "class", "key");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2, "class", "value");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n              ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var element6 = dom.childAt(fragment, [1]);
                  var morphs = new Array(2);
                  morphs[0] = dom.createMorphAt(dom.childAt(element6, [1]), 0, 0);
                  morphs[1] = dom.createMorphAt(dom.childAt(element6, [3]), 0, 0);
                  return morphs;
                },
                statements: [["content", "prop.key", ["loc", [null, [30, 32], [30, 44]]]], ["content", "prop.value", ["loc", [null, [31, 34], [31, 48]]]]],
                locals: ["prop"],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 26,
                    "column": 8
                  },
                  "end": {
                    "line": 35,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("table");
                dom.setAttribute(el1, "class", "key-value-table");
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("          ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                return morphs;
              },
              statements: [["block", "each", [["get", "model.props.propsList", ["loc", [null, [28, 20], [28, 41]]]]], [], 0, null, ["loc", [null, [28, 12], [33, 21]]]]],
              locals: [],
              templates: [child0]
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 16,
                  "column": 6
                },
                "end": {
                  "line": 36,
                  "column": 6
                }
              },
              "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(3);
              morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
              morphs[1] = dom.createMorphAt(fragment, 2, 2, contextualElement);
              morphs[2] = dom.createMorphAt(fragment, 4, 4, contextualElement);
              dom.insertBoundary(fragment, 0);
              dom.insertBoundary(fragment, null);
              return morphs;
            },
            statements: [["block", "em-tab-list", [], [], 0, null, ["loc", [null, [17, 8], [20, 24]]]], ["block", "em-tab-panel", [], [], 1, null, ["loc", [null, [22, 8], [24, 25]]]], ["block", "em-tab-panel", [], [], 2, null, ["loc", [null, [26, 8], [35, 25]]]]],
            locals: [],
            templates: [child0, child1, child2]
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 15,
                "column": 4
              },
              "end": {
                "line": 37,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [["block", "em-tabs", [], ["class", "half-width"], 0, null, ["loc", [null, [16, 6], [36, 18]]]]],
          locals: [],
          templates: [child0]
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 37,
                "column": 4
              },
              "end": {
                "line": 39,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      Properties not loaded\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 2
            },
            "end": {
              "line": 40,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.props", ["loc", [null, [15, 10], [15, 21]]]]], [], 0, 1, ["loc", [null, [15, 4], [39, 11]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          var child0 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 55,
                    "column": 16
                  },
                  "end": {
                    "line": 59,
                    "column": 16
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                  ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("li");
                var el2 = dom.createTextNode("\n                    ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n                  ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                return morphs;
              },
              statements: [["inline", "button.refresh-keys", [], ["action", "refreshKeys", "bucket", ["subexpr", "@mut", [["get", "model", ["loc", [null, [57, 70], [57, 75]]]]], [], []]], ["loc", [null, [57, 20], [57, 77]]]]],
              locals: [],
              templates: []
            };
          })();
          var child1 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 60,
                    "column": 16
                  },
                  "end": {
                    "line": 67,
                    "column": 16
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                  ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("li");
                var el2 = dom.createTextNode("\n                    ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("button");
                dom.setAttribute(el2, "type", "button");
                dom.setAttribute(el2, "class", "btn btn-xs btn-danger");
                var el3 = dom.createTextNode("\n                      ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("span");
                dom.setAttribute(el3, "class", "glyphicon glyphicon-trash");
                dom.setAttribute(el3, "aria-hidden", "true");
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n                      Delete All Keys\n                    ");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n                  ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element2 = dom.childAt(fragment, [1, 1]);
                var morphs = new Array(1);
                morphs[0] = dom.createElementMorph(element2);
                return morphs;
              },
              statements: [["element", "action", ["deleteBucket", ["get", "model", ["loc", [null, [62, 96], [62, 101]]]]], [], ["loc", [null, [62, 72], [62, 103]]]]],
              locals: [],
              templates: []
            };
          })();
          var child2 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 74,
                    "column": 14
                  },
                  "end": {
                    "line": 77,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("p");
                var el2 = dom.createTextNode("Displaying keys ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("-");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode(" out\n                  of ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element1 = dom.childAt(fragment, [1]);
                var morphs = new Array(3);
                morphs[0] = dom.createMorphAt(element1, 1, 1);
                morphs[1] = dom.createMorphAt(element1, 3, 3);
                morphs[2] = dom.createMorphAt(element1, 5, 5);
                return morphs;
              },
              statements: [["content", "model.keyList.firstItemIndex", ["loc", [null, [75, 35], [75, 67]]]], ["content", "model.keyList.lastItemIndex", ["loc", [null, [75, 68], [75, 99]]]], ["content", "model.keyList.total", ["loc", [null, [76, 21], [76, 44]]]]],
              locals: [],
              templates: []
            };
          })();
          var child3 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 77,
                    "column": 14
                  },
                  "end": {
                    "line": 79,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("p");
                var el2 = dom.createTextNode("Displaying key ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode(" out of ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element0 = dom.childAt(fragment, [1]);
                var morphs = new Array(2);
                morphs[0] = dom.createMorphAt(element0, 1, 1);
                morphs[1] = dom.createMorphAt(element0, 3, 3);
                return morphs;
              },
              statements: [["content", "model.keyList.firstItemIndex", ["loc", [null, [78, 34], [78, 66]]]], ["content", "model.keyList.total", ["loc", [null, [78, 74], [78, 97]]]]],
              locals: [],
              templates: []
            };
          })();
          var child4 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 86,
                      "column": 18
                    },
                    "end": {
                      "line": 90,
                      "column": 18
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
                },
                arity: 1,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                    ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("li");
                  var el2 = dom.createTextNode("\n                      ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createComment("");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                    ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                  return morphs;
                },
                statements: [["inline", "link.link-object", [], ["obj", ["subexpr", "@mut", [["get", "obj", ["loc", [null, [88, 45], [88, 48]]]]], [], []]], ["loc", [null, [88, 22], [88, 50]]]]],
                locals: ["obj"],
                templates: []
              };
            })();
            var child1 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 90,
                      "column": 18
                    },
                    "end": {
                      "line": 92,
                      "column": 18
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                    No keys found\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 81,
                    "column": 14
                  },
                  "end": {
                    "line": 94,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("ul");
                dom.setAttribute(el1, "class", "button-list small");
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("                ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                return morphs;
              },
              statements: [["block", "each", [["get", "model.keyList.keys", ["loc", [null, [86, 26], [86, 44]]]]], [], 0, 1, ["loc", [null, [86, 18], [92, 27]]]]],
              locals: [],
              templates: [child0, child1]
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 44,
                  "column": 6
                },
                "end": {
                  "line": 99,
                  "column": 6
                }
              },
              "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("        ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("table");
              dom.setAttribute(el1, "class", "key-value-table");
              var el2 = dom.createTextNode("\n          ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("tbody");
              var el3 = dom.createTextNode("\n          ");
              dom.appendChild(el2, el3);
              var el3 = dom.createElement("tr");
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "key");
              var el5 = dom.createTextNode("Key list cache created:");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "value");
              var el5 = dom.createComment("");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n          ");
              dom.appendChild(el3, el4);
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("\n          ");
              dom.appendChild(el2, el3);
              var el3 = dom.createElement("tr");
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "key");
              var el5 = dom.createTextNode("Available actions:");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "value");
              var el5 = dom.createTextNode("\n              ");
              dom.appendChild(el4, el5);
              var el5 = dom.createElement("ul");
              dom.setAttribute(el5, "class", "button-list");
              var el6 = dom.createTextNode("\n");
              dom.appendChild(el5, el6);
              var el6 = dom.createComment("");
              dom.appendChild(el5, el6);
              var el6 = dom.createComment("");
              dom.appendChild(el5, el6);
              var el6 = dom.createTextNode("              ");
              dom.appendChild(el5, el6);
              dom.appendChild(el4, el5);
              var el5 = dom.createTextNode("\n            ");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n          ");
              dom.appendChild(el3, el4);
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("\n          ");
              dom.appendChild(el2, el3);
              var el3 = dom.createElement("tr");
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "key");
              var el5 = dom.createTextNode("Cached Key List:");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "value");
              var el5 = dom.createTextNode("\n");
              dom.appendChild(el4, el5);
              var el5 = dom.createComment("");
              dom.appendChild(el4, el5);
              var el5 = dom.createTextNode("\n");
              dom.appendChild(el4, el5);
              var el5 = dom.createComment("");
              dom.appendChild(el4, el5);
              var el5 = dom.createTextNode("            ");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n          ");
              dom.appendChild(el3, el4);
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("\n          ");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n        ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var element3 = dom.childAt(fragment, [1, 1]);
              var element4 = dom.childAt(element3, [3, 3, 1]);
              var element5 = dom.childAt(element3, [5, 3]);
              var morphs = new Array(5);
              morphs[0] = dom.createMorphAt(dom.childAt(element3, [1, 3]), 0, 0);
              morphs[1] = dom.createMorphAt(element4, 1, 1);
              morphs[2] = dom.createMorphAt(element4, 2, 2);
              morphs[3] = dom.createMorphAt(element5, 1, 1);
              morphs[4] = dom.createMorphAt(element5, 3, 3);
              return morphs;
            },
            statements: [["content", "model.keyList.created", ["loc", [null, [49, 30], [49, 55]]]], ["block", "if", [["get", "model.cluster.developmentMode", ["loc", [null, [55, 22], [55, 51]]]]], [], 0, null, ["loc", [null, [55, 16], [59, 23]]]], ["block", "if", [["get", "model.keyList.showDeleteKeys", ["loc", [null, [60, 22], [60, 50]]]]], [], 1, null, ["loc", [null, [60, 16], [67, 23]]]], ["block", "if", [["get", "model.keyList.multipleListItems", ["loc", [null, [74, 20], [74, 51]]]]], [], 2, 3, ["loc", [null, [74, 14], [79, 21]]]], ["block", "pagination-component", [], ["chunkSize", ["subexpr", "@mut", [["get", "model.keyList.pageSize", ["loc", [null, [82, 24], [82, 46]]]]], [], []], "totalSize", ["subexpr", "@mut", [["get", "model.keyList.total", ["loc", [null, [83, 24], [83, 43]]]]], [], []], "sectionRequest", "retrieveRequestedKeys"], 4, null, ["loc", [null, [81, 14], [94, 39]]]]],
            locals: [],
            templates: [child0, child1, child2, child3, child4]
          };
        })();
        var child1 = (function () {
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 99,
                  "column": 6
                },
                "end": {
                  "line": 103,
                  "column": 6
                }
              },
              "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("        ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("p");
              var el2 = dom.createTextNode("Cached list not available.\n          This cluster is in Production mode, and so no cache refresh is\n          possible.");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes() {
              return [];
            },
            statements: [],
            locals: [],
            templates: []
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 43,
                "column": 4
              },
              "end": {
                "line": 104,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [["block", "if", [["get", "model.keyList.cachePresent", ["loc", [null, [44, 12], [44, 38]]]]], [], 0, 1, ["loc", [null, [44, 6], [103, 13]]]]],
          locals: [],
          templates: [child0, child1]
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 104,
                "column": 4
              },
              "end": {
                "line": 106,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "loading-spinner", [], ["statusMessage", ["subexpr", "@mut", [["get", "model.keyList.statusMessage", ["loc", [null, [105, 38], [105, 65]]]]], [], []]], ["loc", [null, [105, 6], [105, 67]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 42,
              "column": 2
            },
            "end": {
              "line": 107,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.isKeyListLoaded", ["loc", [null, [43, 10], [43, 31]]]]], [], 0, 1, ["loc", [null, [43, 4], [106, 11]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 109,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "bucket-types-container");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element7 = dom.childAt(fragment, [0]);
        var element8 = dom.childAt(fragment, [2]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(element7, 1, 1);
        morphs[1] = dom.createMorphAt(element7, 3, 3);
        morphs[2] = dom.createMorphAt(element8, 1, 1);
        morphs[3] = dom.createMorphAt(element8, 3, 3);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "bucketTypeId", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [5, 15], [5, 33]]]]], [], []], "bucketId", ["subexpr", "@mut", [["get", "model.bucketId", ["loc", [null, [6, 11], [6, 25]]]]], [], []]], ["loc", [null, [2, 2], [7, 4]]]], ["inline", "view-label", [], ["pre-label", "Bucket", "label", ["subexpr", "@mut", [["get", "model.bucketId", ["loc", [null, [10, 8], [10, 22]]]]], [], []]], ["loc", [null, [8, 2], [10, 24]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [14, 2], [40, 23]]]], ["block", "dashboard-module", [], ["label", "Keys"], 1, null, ["loc", [null, [42, 2], [107, 23]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define('ember-riak-explorer/pods/bucket-type/controller', ['exports', 'ember'], function (exports, _ember) {

  var BucketTypeController = _ember['default'].Controller.extend({
    explorer: _ember['default'].inject.service('explorer'),

    /**
     * Kicks off a model refresh after the specified delay.
     * Initially called by +BucketTypeRoute.setupController+.
     *
     * @method pollForModel
     * @param bucketType {BucketType}
     * @param delay {Number} Milliseconds to wait before refreshing the model
     *                       (to see if the bucket list has loaded)
     */
    pollForModel: function pollForModel(bucketType, delay) {
      var self = this;

      _ember['default'].run.later(function () {
        // console.log('controller: scheduling to refreshModel');
        self.refreshModel(bucketType);
      }, delay);
    },

    /**
     * Reloads the model (bucket type, its properties and bucket list)
     * from the server.
     * If the bucket list is not ready, try again after a delay.
     *
     * @method refreshModel
     * @param bucketType {BucketType}
     */
    refreshModel: function refreshModel(bucketType) {
      var self = this;

      // console.log("Refreshing model %O", bucketType);
      var cluster = bucketType.get('cluster');

      self.get('explorer').getBucketList(cluster, bucketType).then(function (updatedBucketList) {
        // console.log('loaded bucket list: %O', updatedBucketList);
        var model = self.get('model');

        model.set('bucketList', updatedBucketList);

        if (!model.get('isBucketListLoaded') && updatedBucketList.get('cachePresent')) {

          // Only continue polling in development mode
          self.pollForModel(model, 3000);
        }
      });
    },

    actions: {
      retrieveRequestedBuckets: function retrieveRequestedBuckets(startIndex) {
        var service = this.get('explorer');
        var bucketType = this.get('model');
        var cluster = bucketType.get('cluster');

        return service.getBucketTypeWithBucketList(bucketType, cluster, startIndex);
      },

      refreshBuckets: function refreshBuckets(bucketType) {
        var clusterId = bucketType.get('clusterId');
        var bucketTypeId = bucketType.get('bucketTypeId');

        this.get('model').set('isBucketListLoaded', false);
        this.get('explorer').bucketCacheRefresh(clusterId, bucketTypeId);
        this.pollForModel(this.get('model'), 3000);
      }
    }
  });

  exports['default'] = BucketTypeController;
});
define('ember-riak-explorer/pods/bucket-type/model', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Represents a Riak Bucket Type
   *
   * @class BucketType
   * @extends DS.Model
   * @constructor
   * @uses Cluster
   * @uses BucketProps
   * @uses BucketList
   */
  var BucketType = _emberData['default'].Model.extend({
    /**
     * Initializes a new BucketType instance by setting up an empty
     * BucketList.
     * @method init
     */
    init: function init() {
      this._super();
      var emptyList = this.store.createRecord('bucket-list', {
        cluster: this.get('cluster'),
        buckets: []
      });
      this.set('bucketList', emptyList);
    },

    /**
     * Contains the results of cached bucket lists for this bucket type,
     * fetched from the API.
     * @property bucket-list
     * @type BucketList
     */
    bucketList: _emberData['default'].belongsTo('bucket-list'),

    /**
     * Riak cluster in which this bucket type lives.
     * @property cluster
     * @type Cluster
     * @writeOnce
     */
    cluster: _emberData['default'].belongsTo('cluster'),

    /**
     * Bucket Type Properties object.
     * Note: Bucket Types and Buckets share the same Properties format.
     * When not specified, buckets inherit their properties from the Bucket Type
     * @property props
     * @type BucketProps
     */
    props: _emberData['default'].belongsTo('bucket-props'),

    /**
     * Has the bucketList been loaded from the server?
     * @property isBucketListLoaded
     * @type Boolean
     * @default false
     */
    isBucketListLoaded: _emberData['default'].attr('boolean', { defaultValue: false }),

    /**
     * Bucket Type name (unique per cluster),
     *    as appears on `riak-admin bucket-type list`
     * @property originalId
     * @type String
     */
    originalId: _emberData['default'].attr('string'),

    bucketTypeId: (function () {
      return this.get('originalId');
    }).property('originalId'),

    /**
     * Returns the name of the cluster in which this bucket type resides.
     * (As specified in the `riak_explorer.conf` file)
     * @property clusterId
     * @type String
     */
    clusterId: (function () {
      return this.get('cluster').get('clusterId');
    }).property('cluster'),

    /**
     * Returns the Search Index associated with this bucket type,
     *     if applicable.
     * @property index
     * @type String
     */
    index: (function () {
      return this.get('cluster').get('searchIndexes').findBy('name', this.get('props').get('searchIndexName'));
    }).property('cluster', 'props'),

    /**
     * Returns true if this Bucket Type has been activated.
     *
     * @property isActive
     * @type Boolean
     */
    isActive: (function () {
      return this.get('props').get('isActive');
    }).property('props'),

    /**
     * Returns true if this Bucket Type has not yet been activated.
     *
     * @property isInactive
     * @type Boolean
     */
    isInactive: (function () {
      return !this.get('isActive');
    }).property('props'),

    /**
     * Alias for the record's `id`, which is a composite ID in the form of
     *     `'<clusterId>/<bucketName>'`.
     * @see ExplorerResourceAdapter.normalizeId
     *
     * @property name
     * @type String
     * @example
     *    'dev-cluster/users'
     */
    name: (function () {
      return this.get('id');
    }).property('id')
  });

  exports['default'] = BucketType;
});
define('ember-riak-explorer/pods/bucket-type/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    model: function model(params) {
      var clusterId = params.clusterId;
      var bucketTypeId = params.bucketTypeId;
      var explorer = this.explorer;

      return this.explorer.getBucketType(clusterId, bucketTypeId).then(function (bucketType) {
        return explorer.getBucketTypeWithBucketList(bucketType, bucketType.get('cluster'));
      });
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));
    },

    setupController: function setupController(controller, model) {
      this._super(controller, model);

      if (!model.get('isBucketListLoaded')) {
        controller.pollForModel(model, 3000);
      }
    }
  });
});
define("ember-riak-explorer/pods/bucket-type/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          var child0 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 17,
                      "column": 10
                    },
                    "end": {
                      "line": 17,
                      "column": 29
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("Overview");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            var child1 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 18,
                      "column": 10
                    },
                    "end": {
                      "line": 18,
                      "column": 40
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("Advanced Properties");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 16,
                    "column": 8
                  },
                  "end": {
                    "line": 19,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createComment("");
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createComment("");
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(2);
                morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
                morphs[1] = dom.createMorphAt(fragment, 3, 3, contextualElement);
                return morphs;
              },
              statements: [["block", "em-tab", [], [], 0, null, ["loc", [null, [17, 10], [17, 40]]]], ["block", "em-tab", [], [], 1, null, ["loc", [null, [18, 10], [18, 51]]]]],
              locals: [],
              templates: [child0, child1]
            };
          })();
          var child1 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 27,
                      "column": 16
                    },
                    "end": {
                      "line": 29,
                      "column": 16
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                  ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("span");
                  dom.setAttribute(el1, "class", "label label-success");
                  var el2 = dom.createTextNode("Active");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            var child1 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 29,
                      "column": 16
                    },
                    "end": {
                      "line": 31,
                      "column": 16
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                  ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("span");
                  dom.setAttribute(el1, "class", "label label-default");
                  var el2 = dom.createTextNode("Inactive");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            var child2 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 42,
                      "column": 16
                    },
                    "end": {
                      "line": 52,
                      "column": 16
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                  ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("br");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n                  R: ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode(", W: ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode(",\n                  PR: ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode(", PW: ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode(",\n                  DW: ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n                  ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("br");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n                  ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("small");
                  var el2 = dom.createTextNode("\n                    (basic_quorum: ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createComment("");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode(",\n                    notfound_ok: ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createComment("");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode(")\n                  ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var element5 = dom.childAt(fragment, [15]);
                  var morphs = new Array(7);
                  morphs[0] = dom.createMorphAt(fragment, 3, 3, contextualElement);
                  morphs[1] = dom.createMorphAt(fragment, 5, 5, contextualElement);
                  morphs[2] = dom.createMorphAt(fragment, 7, 7, contextualElement);
                  morphs[3] = dom.createMorphAt(fragment, 9, 9, contextualElement);
                  morphs[4] = dom.createMorphAt(fragment, 11, 11, contextualElement);
                  morphs[5] = dom.createMorphAt(element5, 1, 1);
                  morphs[6] = dom.createMorphAt(element5, 3, 3);
                  return morphs;
                },
                statements: [["content", "model.props.quorum.r", ["loc", [null, [44, 21], [44, 45]]]], ["content", "model.props.quorum.w", ["loc", [null, [44, 50], [44, 74]]]], ["content", "model.props.quorum.pr", ["loc", [null, [45, 22], [45, 47]]]], ["content", "model.props.quorum.pw", ["loc", [null, [45, 53], [45, 78]]]], ["content", "model.props.quorum.dw", ["loc", [null, [46, 22], [46, 47]]]], ["content", "model.props.quorum.basic_quorum", ["loc", [null, [49, 35], [49, 70]]]], ["content", "model.props.quorum.basic_quorum", ["loc", [null, [50, 33], [50, 68]]]]],
                locals: [],
                templates: []
              };
            })();
            var child3 = (function () {
              var child0 = (function () {
                return {
                  meta: {
                    "revision": "Ember@1.13.11",
                    "loc": {
                      "source": null,
                      "start": {
                        "line": 59,
                        "column": 18
                      },
                      "end": {
                        "line": 61,
                        "column": 18
                      }
                    },
                    "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                  },
                  arity: 0,
                  cachedFragment: null,
                  hasRendered: false,
                  buildFragment: function buildFragment(dom) {
                    var el0 = dom.createDocumentFragment();
                    var el1 = dom.createTextNode("                    ");
                    dom.appendChild(el0, el1);
                    var el1 = dom.createComment("");
                    dom.appendChild(el0, el1);
                    var el1 = dom.createTextNode("\n");
                    dom.appendChild(el0, el1);
                    return el0;
                  },
                  buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                    var morphs = new Array(1);
                    morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
                    return morphs;
                  },
                  statements: [["content", "model.index.name", ["loc", [null, [60, 20], [60, 40]]]]],
                  locals: [],
                  templates: []
                };
              })();
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 58,
                      "column": 16
                    },
                    "end": {
                      "line": 62,
                      "column": 16
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
                  dom.insertBoundary(fragment, 0);
                  dom.insertBoundary(fragment, null);
                  return morphs;
                },
                statements: [["block", "link-to", ["search-index", ["get", "model.index.cluster.clusterId", ["loc", [null, [59, 44], [59, 73]]]], ["get", "model.index.name", ["loc", [null, [59, 74], [59, 90]]]]], [], 0, null, ["loc", [null, [59, 18], [61, 30]]]]],
                locals: [],
                templates: [child0]
              };
            })();
            var child4 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 62,
                      "column": 16
                    },
                    "end": {
                      "line": 64,
                      "column": 16
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                  n/a (not being indexed)\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            var child5 = (function () {
              var child0 = (function () {
                return {
                  meta: {
                    "revision": "Ember@1.13.11",
                    "loc": {
                      "source": null,
                      "start": {
                        "line": 72,
                        "column": 20
                      },
                      "end": {
                        "line": 74,
                        "column": 20
                      }
                    },
                    "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                  },
                  arity: 1,
                  cachedFragment: null,
                  hasRendered: false,
                  buildFragment: function buildFragment(dom) {
                    var el0 = dom.createDocumentFragment();
                    var el1 = dom.createTextNode("                      ");
                    dom.appendChild(el0, el1);
                    var el1 = dom.createElement("li");
                    var el2 = dom.createComment("");
                    dom.appendChild(el1, el2);
                    dom.appendChild(el0, el1);
                    var el1 = dom.createTextNode("\n");
                    dom.appendChild(el0, el1);
                    return el0;
                  },
                  buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                    var morphs = new Array(1);
                    morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
                    return morphs;
                  },
                  statements: [["content", "warning", ["loc", [null, [73, 26], [73, 37]]]]],
                  locals: ["warning"],
                  templates: []
                };
              })();
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 67,
                      "column": 12
                    },
                    "end": {
                      "line": 78,
                      "column": 12
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("              ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("tr");
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2, "class", "key");
                  var el3 = dom.createTextNode("Warnings:");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2, "class", "value");
                  var el3 = dom.createTextNode("\n                  ");
                  dom.appendChild(el2, el3);
                  var el3 = dom.createElement("ul");
                  var el4 = dom.createTextNode("\n");
                  dom.appendChild(el3, el4);
                  var el4 = dom.createComment("");
                  dom.appendChild(el3, el4);
                  var el4 = dom.createTextNode("                  ");
                  dom.appendChild(el3, el4);
                  dom.appendChild(el2, el3);
                  var el3 = dom.createTextNode("\n                ");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n              ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1]), 1, 1);
                  return morphs;
                },
                statements: [["block", "each", [["get", "model.props.warnings", ["loc", [null, [72, 28], [72, 48]]]]], [], 0, null, ["loc", [null, [72, 20], [74, 29]]]]],
                locals: [],
                templates: [child0]
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 21,
                    "column": 8
                  },
                  "end": {
                    "line": 80,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("table");
                dom.setAttribute(el1, "class", "key-value-table");
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("tr");
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "key");
                var el4 = dom.createTextNode("Object type:");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "value");
                var el4 = dom.createTextNode("\n                ");
                dom.appendChild(el3, el4);
                var el4 = dom.createComment("");
                dom.appendChild(el3, el4);
                var el4 = dom.createTextNode("\n");
                dom.appendChild(el3, el4);
                var el4 = dom.createComment("");
                dom.appendChild(el3, el4);
                var el4 = dom.createTextNode("              ");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n            ");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("tr");
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "key");
                var el4 = dom.createTextNode("Conflict Res. Strategy:");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "value");
                var el4 = dom.createComment("");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n            ");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("tr");
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "key");
                var el4 = dom.createTextNode("Quorum:");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "value");
                var el4 = dom.createTextNode("\n                N_Val: ");
                dom.appendChild(el3, el4);
                var el4 = dom.createComment("");
                dom.appendChild(el3, el4);
                var el4 = dom.createTextNode("\n");
                dom.appendChild(el3, el4);
                var el4 = dom.createComment("");
                dom.appendChild(el3, el4);
                var el4 = dom.createTextNode("              ");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n            ");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("tr");
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "key");
                var el4 = dom.createTextNode("Search Index:");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3, "class", "value");
                var el4 = dom.createTextNode("\n");
                dom.appendChild(el3, el4);
                var el4 = dom.createComment("");
                dom.appendChild(el3, el4);
                var el4 = dom.createTextNode("              ");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n            ");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("          ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element6 = dom.childAt(fragment, [1]);
                var element7 = dom.childAt(element6, [1, 3]);
                var element8 = dom.childAt(element6, [5, 3]);
                var morphs = new Array(7);
                morphs[0] = dom.createMorphAt(element7, 1, 1);
                morphs[1] = dom.createMorphAt(element7, 3, 3);
                morphs[2] = dom.createMorphAt(dom.childAt(element6, [3, 3]), 0, 0);
                morphs[3] = dom.createMorphAt(element8, 1, 1);
                morphs[4] = dom.createMorphAt(element8, 3, 3);
                morphs[5] = dom.createMorphAt(dom.childAt(element6, [7, 3]), 1, 1);
                morphs[6] = dom.createMorphAt(element6, 9, 9);
                return morphs;
              },
              statements: [["content", "model.props.objectType", ["loc", [null, [26, 16], [26, 42]]]], ["block", "if", [["get", "model.isActive", ["loc", [null, [27, 22], [27, 36]]]]], [], 0, 1, ["loc", [null, [27, 16], [31, 23]]]], ["content", "model.props.resolutionStrategy", ["loc", [null, [36, 32], [36, 66]]]], ["content", "model.props.nVal", ["loc", [null, [41, 23], [41, 43]]]], ["block", "if", [["get", "model.props.quorumRelevant", ["loc", [null, [42, 22], [42, 48]]]]], [], 2, null, ["loc", [null, [42, 16], [52, 23]]]], ["block", "if", [["get", "model.index", ["loc", [null, [58, 22], [58, 33]]]]], [], 3, 4, ["loc", [null, [58, 16], [64, 23]]]], ["block", "if", [["get", "model.props.warnings", ["loc", [null, [67, 18], [67, 38]]]]], [], 5, null, ["loc", [null, [67, 12], [78, 19]]]]],
              locals: [],
              templates: [child0, child1, child2, child3, child4, child5]
            };
          })();
          var child2 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 84,
                      "column": 12
                    },
                    "end": {
                      "line": 89,
                      "column": 12
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 1,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("              ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("tr");
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2, "class", "key");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2, "class", "value");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n              ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var element4 = dom.childAt(fragment, [1]);
                  var morphs = new Array(2);
                  morphs[0] = dom.createMorphAt(dom.childAt(element4, [1]), 0, 0);
                  morphs[1] = dom.createMorphAt(dom.childAt(element4, [3]), 0, 0);
                  return morphs;
                },
                statements: [["content", "prop.key", ["loc", [null, [86, 32], [86, 44]]]], ["content", "prop.value", ["loc", [null, [87, 34], [87, 48]]]]],
                locals: ["prop"],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 82,
                    "column": 8
                  },
                  "end": {
                    "line": 91,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("table");
                dom.setAttribute(el1, "class", "key-value-table");
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("          ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                return morphs;
              },
              statements: [["block", "each", [["get", "model.props.propsList", ["loc", [null, [84, 20], [84, 41]]]]], [], 0, null, ["loc", [null, [84, 12], [89, 21]]]]],
              locals: [],
              templates: [child0]
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 15,
                  "column": 6
                },
                "end": {
                  "line": 92,
                  "column": 6
                }
              },
              "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(3);
              morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
              morphs[1] = dom.createMorphAt(fragment, 2, 2, contextualElement);
              morphs[2] = dom.createMorphAt(fragment, 4, 4, contextualElement);
              dom.insertBoundary(fragment, 0);
              dom.insertBoundary(fragment, null);
              return morphs;
            },
            statements: [["block", "em-tab-list", [], [], 0, null, ["loc", [null, [16, 8], [19, 24]]]], ["block", "em-tab-panel", [], [], 1, null, ["loc", [null, [21, 8], [80, 25]]]], ["block", "em-tab-panel", [], [], 2, null, ["loc", [null, [82, 8], [91, 25]]]]],
            locals: [],
            templates: [child0, child1, child2]
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 14,
                "column": 4
              },
              "end": {
                "line": 93,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [["block", "em-tabs", [], ["class", "half-width"], 0, null, ["loc", [null, [15, 6], [92, 18]]]]],
          locals: [],
          templates: [child0]
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 93,
                "column": 4
              },
              "end": {
                "line": 95,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      Properties not loaded\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 13,
              "column": 2
            },
            "end": {
              "line": 96,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.props", ["loc", [null, [14, 10], [14, 21]]]]], [], 0, 1, ["loc", [null, [14, 4], [95, 11]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          var child0 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 107,
                    "column": 10
                  },
                  "end": {
                    "line": 120,
                    "column": 10
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("            ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("tr");
                var el2 = dom.createTextNode("\n              ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("td");
                dom.setAttribute(el2, "class", "key");
                var el3 = dom.createTextNode("Available actions:");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n              ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("td");
                dom.setAttribute(el2, "class", "value");
                var el3 = dom.createTextNode("\n                ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("ul");
                dom.setAttribute(el3, "class", "button-list");
                var el4 = dom.createTextNode("\n                  ");
                dom.appendChild(el3, el4);
                var el4 = dom.createElement("li");
                var el5 = dom.createTextNode("\n                    ");
                dom.appendChild(el4, el5);
                var el5 = dom.createComment("");
                dom.appendChild(el4, el5);
                var el5 = dom.createTextNode("\n                  ");
                dom.appendChild(el4, el5);
                dom.appendChild(el3, el4);
                var el4 = dom.createTextNode("\n                ");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1, 1]), 1, 1);
                return morphs;
              },
              statements: [["inline", "button.refresh-buckets", [], ["action", "refreshBuckets", "bucketType", ["subexpr", "@mut", [["get", "model", ["loc", [null, [115, 31], [115, 36]]]]], [], []]], ["loc", [null, [113, 20], [115, 38]]]]],
              locals: [],
              templates: []
            };
          })();
          var child1 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 124,
                    "column": 14
                  },
                  "end": {
                    "line": 127,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("p");
                var el2 = dom.createTextNode("Displaying buckets ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("-");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode(" out\n                  of ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element1 = dom.childAt(fragment, [1]);
                var morphs = new Array(3);
                morphs[0] = dom.createMorphAt(element1, 1, 1);
                morphs[1] = dom.createMorphAt(element1, 3, 3);
                morphs[2] = dom.createMorphAt(element1, 5, 5);
                return morphs;
              },
              statements: [["content", "model.bucketList.firstItemIndex", ["loc", [null, [125, 38], [125, 73]]]], ["content", "model.bucketList.lastItemIndex", ["loc", [null, [125, 74], [125, 108]]]], ["content", "model.bucketList.total", ["loc", [null, [126, 21], [126, 47]]]]],
              locals: [],
              templates: []
            };
          })();
          var child2 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 127,
                    "column": 14
                  },
                  "end": {
                    "line": 129,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("p");
                var el2 = dom.createTextNode("Displaying bucket ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode(" out of ");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element0 = dom.childAt(fragment, [1]);
                var morphs = new Array(2);
                morphs[0] = dom.createMorphAt(element0, 1, 1);
                morphs[1] = dom.createMorphAt(element0, 3, 3);
                return morphs;
              },
              statements: [["content", "model.bucketList.firstItemIndex", ["loc", [null, [128, 37], [128, 72]]]], ["content", "model.bucketList.total", ["loc", [null, [128, 80], [128, 106]]]]],
              locals: [],
              templates: []
            };
          })();
          var child3 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 136,
                      "column": 18
                    },
                    "end": {
                      "line": 140,
                      "column": 18
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 1,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                    ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("li");
                  var el2 = dom.createTextNode("\n                      ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createComment("");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                    ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                  return morphs;
                },
                statements: [["inline", "link.link-bucket", [], ["bucket", ["subexpr", "@mut", [["get", "bucket", ["loc", [null, [138, 48], [138, 54]]]]], [], []]], ["loc", [null, [138, 22], [138, 56]]]]],
                locals: ["bucket"],
                templates: []
              };
            })();
            var child1 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 140,
                      "column": 18
                    },
                    "end": {
                      "line": 142,
                      "column": 18
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                    No buckets found\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes() {
                  return [];
                },
                statements: [],
                locals: [],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 131,
                    "column": 14
                  },
                  "end": {
                    "line": 144,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("ul");
                dom.setAttribute(el1, "class", "button-list small");
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("                ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                return morphs;
              },
              statements: [["block", "each", [["get", "model.bucketList.buckets", ["loc", [null, [136, 26], [136, 50]]]]], [], 0, 1, ["loc", [null, [136, 18], [142, 27]]]]],
              locals: [],
              templates: [child0, child1]
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 100,
                  "column": 6
                },
                "end": {
                  "line": 149,
                  "column": 6
                }
              },
              "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("        ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("table");
              dom.setAttribute(el1, "class", "key-value-table");
              var el2 = dom.createTextNode("\n          ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("tbody");
              var el3 = dom.createTextNode("\n          ");
              dom.appendChild(el2, el3);
              var el3 = dom.createElement("tr");
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "key");
              var el5 = dom.createTextNode("Bucket List cache created:");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "value");
              var el5 = dom.createComment("");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n          ");
              dom.appendChild(el3, el4);
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("\n");
              dom.appendChild(el2, el3);
              var el3 = dom.createComment("");
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("          ");
              dom.appendChild(el2, el3);
              var el3 = dom.createElement("tr");
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "key");
              var el5 = dom.createTextNode("Cached Bucket List");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n            ");
              dom.appendChild(el3, el4);
              var el4 = dom.createElement("td");
              dom.setAttribute(el4, "class", "value");
              var el5 = dom.createTextNode("\n");
              dom.appendChild(el4, el5);
              var el5 = dom.createComment("");
              dom.appendChild(el4, el5);
              var el5 = dom.createTextNode("\n");
              dom.appendChild(el4, el5);
              var el5 = dom.createComment("");
              dom.appendChild(el4, el5);
              var el5 = dom.createTextNode("            ");
              dom.appendChild(el4, el5);
              dom.appendChild(el3, el4);
              var el4 = dom.createTextNode("\n          ");
              dom.appendChild(el3, el4);
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("\n          ");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n        ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var element2 = dom.childAt(fragment, [1, 1]);
              var element3 = dom.childAt(element2, [5, 3]);
              var morphs = new Array(4);
              morphs[0] = dom.createMorphAt(dom.childAt(element2, [1, 3]), 0, 0);
              morphs[1] = dom.createMorphAt(element2, 3, 3);
              morphs[2] = dom.createMorphAt(element3, 1, 1);
              morphs[3] = dom.createMorphAt(element3, 3, 3);
              return morphs;
            },
            statements: [["content", "model.bucketList.created", ["loc", [null, [105, 30], [105, 58]]]], ["block", "if", [["get", "model.cluster.developmentMode", ["loc", [null, [107, 16], [107, 45]]]]], [], 0, null, ["loc", [null, [107, 10], [120, 17]]]], ["block", "if", [["get", "model.bucketList.multipleListItems", ["loc", [null, [124, 20], [124, 54]]]]], [], 1, 2, ["loc", [null, [124, 14], [129, 21]]]], ["block", "pagination-component", [], ["chunkSize", ["subexpr", "@mut", [["get", "model.bucketList.pageSize", ["loc", [null, [132, 24], [132, 49]]]]], [], []], "totalSize", ["subexpr", "@mut", [["get", "model.bucketList.total", ["loc", [null, [133, 24], [133, 46]]]]], [], []], "sectionRequest", "retrieveRequestedBuckets"], 3, null, ["loc", [null, [131, 14], [144, 39]]]]],
            locals: [],
            templates: [child0, child1, child2, child3]
          };
        })();
        var child1 = (function () {
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 149,
                  "column": 6
                },
                "end": {
                  "line": 153,
                  "column": 6
                }
              },
              "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("        ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("p");
              var el2 = dom.createTextNode("Cached list not available.\n          This cluster is in Production mode, and so no cache refresh is\n          possible.");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes() {
              return [];
            },
            statements: [],
            locals: [],
            templates: []
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 99,
                "column": 4
              },
              "end": {
                "line": 154,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [["block", "if", [["get", "model.bucketList.cachePresent", ["loc", [null, [100, 12], [100, 41]]]]], [], 0, 1, ["loc", [null, [100, 6], [153, 13]]]]],
          locals: [],
          templates: [child0, child1]
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 154,
                "column": 4
              },
              "end": {
                "line": 156,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "loading-spinner", [], ["statusMessage", ["subexpr", "@mut", [["get", "model.bucketList.statusMessage", ["loc", [null, [155, 38], [155, 68]]]]], [], []]], ["loc", [null, [155, 6], [155, 70]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 98,
              "column": 2
            },
            "end": {
              "line": 157,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.isBucketListLoaded", ["loc", [null, [99, 10], [99, 34]]]]], [], 0, 1, ["loc", [null, [99, 4], [156, 11]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 159,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "bucket-types-container");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element9 = dom.childAt(fragment, [0]);
        var element10 = dom.childAt(fragment, [2]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(element9, 1, 1);
        morphs[1] = dom.createMorphAt(element9, 3, 3);
        morphs[2] = dom.createMorphAt(element10, 1, 1);
        morphs[3] = dom.createMorphAt(element10, 3, 3);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "bucketTypeId", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [5, 15], [5, 33]]]]], [], []]], ["loc", [null, [2, 2], [6, 4]]]], ["inline", "view-label", [], ["pre-label", "Bucket-Type", "label", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [9, 8], [9, 26]]]]], [], []]], ["loc", [null, [7, 2], [9, 28]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [13, 2], [96, 23]]]], ["block", "dashboard-module", [], ["label", "Buckets"], 1, null, ["loc", [null, [98, 2], [157, 23]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define('ember-riak-explorer/pods/cluster/data/model', ['exports', 'ember-riak-explorer/pods/cluster/model'], function (exports, _emberRiakExplorerPodsClusterModel) {
  exports['default'] = _emberRiakExplorerPodsClusterModel['default'].extend({});
});
define('ember-riak-explorer/pods/cluster/data/route', ['exports', 'ember-riak-explorer/pods/cluster/route'], function (exports, _emberRiakExplorerPodsClusterRoute) {
  exports['default'] = _emberRiakExplorerPodsClusterRoute['default'].extend({});
});
define("ember-riak-explorer/pods/cluster/data/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 17,
                "column": 10
              },
              "end": {
                "line": 19,
                "column": 10
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("            On\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 19,
                "column": 10
              },
              "end": {
                "line": 21,
                "column": 10
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("            Off\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 27,
                "column": 10
              },
              "end": {
                "line": 29,
                "column": 10
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("            Enterprise\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child3 = (function () {
        var child0 = (function () {
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 30,
                  "column": 12
                },
                "end": {
                  "line": 32,
                  "column": 12
                }
              },
              "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("              Open Source\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes() {
              return [];
            },
            statements: [],
            locals: [],
            templates: []
          };
        })();
        var child1 = (function () {
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 32,
                  "column": 12
                },
                "end": {
                  "line": 34,
                  "column": 12
                }
              },
              "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("              ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
              return morphs;
            },
            statements: [["content", "model.riakType", ["loc", [null, [33, 14], [33, 32]]]]],
            locals: [],
            templates: []
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 29,
                "column": 10
              },
              "end": {
                "line": 35,
                "column": 10
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [["block", "if", [["get", "model.isOpenSourceEdition", ["loc", [null, [30, 18], [30, 43]]]]], [], 0, 1, ["loc", [null, [30, 12], [34, 19]]]]],
          locals: [],
          templates: [child0, child1]
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 2
            },
            "end": {
              "line": 44,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1, "class", "key-value-table");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tbody");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "key");
          var el5 = dom.createTextNode("Development Mode");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "value");
          var el5 = dom.createTextNode("\n");
          dom.appendChild(el4, el5);
          var el5 = dom.createComment("");
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("        ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "key");
          var el5 = dom.createTextNode("Edition Type");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "value");
          var el5 = dom.createTextNode("\n");
          dom.appendChild(el4, el5);
          var el5 = dom.createComment("");
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("        ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "key");
          var el5 = dom.createTextNode("Status");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "value");
          var el5 = dom.createElement("span");
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [5, 3, 0]);
          var morphs = new Array(4);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]), 1, 1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]), 1, 1);
          morphs[2] = dom.createAttrMorph(element1, 'class');
          morphs[3] = dom.createMorphAt(element1, 0, 0);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.developmentMode", ["loc", [null, [17, 16], [17, 37]]]]], [], 0, 1, ["loc", [null, [17, 10], [21, 17]]]], ["block", "if", [["get", "model.isEnterpriseEdition", ["loc", [null, [27, 16], [27, 41]]]]], [], 2, 3, ["loc", [null, [27, 10], [35, 17]]]], ["attribute", "class", ["concat", ["cluster-status cluster-status-label ", ["get", "model.status", ["loc", [null, [40, 77], [40, 89]]]]]]], ["content", "model.status", ["loc", [null, [40, 93], [40, 109]]]]],
        locals: [],
        templates: [child0, child1, child2, child3]
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 47,
                "column": 4
              },
              "end": {
                "line": 51,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "bucket-types", [], ["clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [49, 16], [49, 31]]]]], [], []], "bucketTypes", ["subexpr", "@mut", [["get", "model.activeBucketTypes", ["loc", [null, [50, 18], [50, 41]]]]], [], []]], ["loc", [null, [48, 6], [50, 43]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 51,
                "column": 4
              },
              "end": {
                "line": 53,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("p");
            var el2 = dom.createTextNode("No bucket types have been activated");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 46,
              "column": 2
            },
            "end": {
              "line": 54,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.activeBucketTypes", ["loc", [null, [47, 10], [47, 33]]]]], [], 0, 1, ["loc", [null, [47, 4], [53, 11]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    var child2 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 57,
                "column": 4
              },
              "end": {
                "line": 61,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "bucket-types", [], ["clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [59, 16], [59, 31]]]]], [], []], "bucketTypes", ["subexpr", "@mut", [["get", "model.inactiveBucketTypes", ["loc", [null, [60, 18], [60, 43]]]]], [], []]], ["loc", [null, [58, 6], [60, 45]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 61,
                "column": 4
              },
              "end": {
                "line": 63,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("p");
            var el2 = dom.createTextNode("No inactive buckets");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 56,
              "column": 2
            },
            "end": {
              "line": 64,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.inactiveBucketTypes", ["loc", [null, [57, 10], [57, 35]]]]], [], 0, 1, ["loc", [null, [57, 4], [63, 11]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 68,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/cluster/data/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "cluster-information-container");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var element3 = dom.childAt(fragment, [2]);
        var morphs = new Array(5);
        morphs[0] = dom.createMorphAt(element2, 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        morphs[2] = dom.createMorphAt(element3, 1, 1);
        morphs[3] = dom.createMorphAt(element3, 3, 3);
        morphs[4] = dom.createMorphAt(element3, 5, 5);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [4, 12], [4, 20]]]]], [], []]], ["loc", [null, [2, 2], [4, 22]]]], ["inline", "view-label", [], ["pre-label", "Cluster Data", "label", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [7, 8], [7, 16]]]]], [], []]], ["loc", [null, [5, 2], [7, 18]]]], ["block", "dashboard-module", [], ["label", "Cluster Properties"], 0, null, ["loc", [null, [11, 2], [44, 23]]]], ["block", "dashboard-module", [], ["label", "Active Bucket Types"], 1, null, ["loc", [null, [46, 2], [54, 23]]]], ["block", "dashboard-module", [], ["label", "Inactive Bucket Types"], 2, null, ["loc", [null, [56, 2], [64, 23]]]]],
      locals: [],
      templates: [child0, child1, child2]
    };
  })());
});
define('ember-riak-explorer/pods/cluster/model', ['exports', 'ember-data', 'ember-riak-explorer/config/environment'], function (exports, _emberData, _emberRiakExplorerConfigEnvironment) {

  /**
   * Represents a Riak cluster as a whole.
   *
   * @class Cluster
   * @extends DS.Model
   * @constructor
   * @uses BucketType
   */
  var Cluster = _emberData['default'].Model.extend({
    /**
     * Bucket types created on the cluster
     * @property bucketTypes
     * @type Array<BucketType>
     */
    bucketTypes: _emberData['default'].hasMany('bucket-type'),

    /**
     * Riak nodes assigned to the cluster
     * @property nodes
     * @type Array<BucketType>
     */
    nodes: _emberData['default'].hasMany('node', { async: true }),

    /**
     * Search indexes created on the cluster
     * @property searchIndexes
     * @type Array<BucketType>
     */
    searchIndexes: _emberData['default'].hasMany('search-index', { async: true }),

    /**
     * Search schemas created on the cluster
     * @property searchSchemas
     * @type Array<BucketType>
     */
    searchSchemas: _emberData['default'].hasMany('search-schema', { async: true }),

    /**
     * Is this cluster in Dev Mode? Set in the Explorer config file.
     * Dev mode allows expensive operations like list keys, delete bucket, etc.
     * @property developmentMode
     * @type Boolean
     * @default false
     */
    developmentMode: _emberData['default'].attr('boolean', { defaultValue: false }),

    /**
     * The Riak Type: either Open Source (oss), Enterprise Edition (ee), or "unavailable"
     * @property riakType
     * @type String
     */
    riakType: _emberData['default'].attr('string', { defaultValue: 'oss' }),

    /**
     * Riak Version
     * @property riakVersion
     * @type String
     */
    riakVersion: _emberData['default'].attr('string'),

    /**
     * Returns a list of currently activated bucket types.
     *
     * @method activeBucketTypes
     * @return {Array<BucketType>}
     */
    activeBucketTypes: (function () {
      return this.get('bucketTypes').filterBy('isActive');
    }).property('bucketTypes'),

    /**
     * Returns the name of the cluster
     * (As specified in the `riak_explorer.conf` file)
     * Note: Currently unrelated to the source/datacenter name used by MDC Repl
     * @method clusterId
     * @type String
     */
    clusterId: (function () {
      return this.get('id');
    }).property('id'),

    /**
     * Boolean check to see if the cluster has a Riak version number associated with it
     *
     * @method hasVersion
     * @returns Boolean
     */
    hasVersion: (function () {
      return this.get('riakVersion') && this.get('riakVersion') !== "unavailable";
    }).property('riakVersion'),

    /**
     * Boolean check to see if the cluster has a Riak type associated with it
     *
     * @method hasType
     * @returns Boolean
     */
    hasType: (function () {
      return this.get('riakType') && this.get('riakType') !== "unavailable";
    }).property('riakType'),

    /**
     * Returns a list of un-activated bucket types.
     *
     * @method inactiveBucketTypes
     * @return {Array<BucketType>}
     */
    inactiveBucketTypes: (function () {
      return this.get('bucketTypes').filterBy('isInactive');
    }).property('bucketTypes'),

    /**
     * Boolean test on if the riakType is the open source edition
     *
     * @method isOpenSourceEdition
     * @return Boolean
     */
    isOpenSourceEdition: (function () {
      return this.get('riakType') === 'oss';
    }).property('riakType'),

    /**
     * Boolean test on if the riakType is the enterprise edition
     *
     * @method isEnterpriseEdition
     * @return Boolean
     */
    isEnterpriseEdition: (function () {
      return this.get('riakType') === 'ee';
    }).property('riakType'),

    /**
     * Returns true if this cluster is in production mode (development_mode=off)
     * @method productionMode
     * @type Boolean
     */
    productionMode: (function () {
      return !this.get('developmentMode');
    }).property('developmentMode'),

    /**
     * Returns the URL which Explorer uses to forward requests to the cluster.
     * Used to link to Search schemas, on the Cluster view.
     * Having the config and url here is hacky, but no good alternatives.
     * @method proxyUrl
     * @return {String} URL
     */
    proxyUrl: (function () {
      return _emberRiakExplorerConfigEnvironment['default'].baseURL + 'riak/clusters/' + this.get('id');
    }).property('id'),

    /**
     * Calculates cluster status based on node health. If all child nodes are valid and
     *  available, status is "ok". If some child nodes are unavailable or invalid, status is
     *  "warning". If all child nodes are unavailable or invalid, status is "down".
     *
     * @method status
     * @return {String} Status
     */
    status: (function () {
      var nodes = this.get('nodes');
      var totalNodes = nodes.get('length');
      var totalHealthyNodes = 0;
      var totalUnhealthyNodes = 0;
      var status = null;

      // Calculate how many nodes are healthy/unhealthy
      nodes.forEach(function (node) {
        if (node.get('isHealthy')) {
          totalHealthyNodes++;
        } else {
          totalUnhealthyNodes++;
        }
      });

      if (totalUnhealthyNodes === totalNodes || totalNodes < 1) {
        status = 'down';
      } else if (totalHealthyNodes === totalNodes) {
        status = 'ok';
      } else {
        status = 'warning';
      }

      return status;
    }).property('nodes.@each.isHealthy')
  });

  exports['default'] = Cluster;
});
define('ember-riak-explorer/pods/cluster/ops/model', ['exports', 'ember-riak-explorer/pods/cluster/model'], function (exports, _emberRiakExplorerPodsClusterModel) {
  exports['default'] = _emberRiakExplorerPodsClusterModel['default'].extend({});
});
define('ember-riak-explorer/pods/cluster/ops/route', ['exports', 'ember-riak-explorer/pods/cluster/route'], function (exports, _emberRiakExplorerPodsClusterRoute) {
  exports['default'] = _emberRiakExplorerPodsClusterRoute['default'].extend({});
});
define("ember-riak-explorer/pods/cluster/ops/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          var child0 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 17,
                      "column": 14
                    },
                    "end": {
                      "line": 20,
                      "column": 14
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("span");
                  dom.setAttribute(el1, "class", "glyphicon glyphicon-ok-circle");
                  dom.setAttribute(el1, "aria-hidden", "true");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n                ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(fragment, 3, 3, contextualElement);
                  return morphs;
                },
                statements: [["content", "node.id", ["loc", [null, [19, 16], [19, 27]]]]],
                locals: [],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 16,
                    "column": 12
                  },
                  "end": {
                    "line": 21,
                    "column": 12
                  }
                },
                "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createComment("");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
                dom.insertBoundary(fragment, 0);
                dom.insertBoundary(fragment, null);
                return morphs;
              },
              statements: [["block", "link-to", ["node", ["get", "model.id", ["loc", [null, [17, 32], [17, 40]]]], ["get", "node.id", ["loc", [null, [17, 41], [17, 48]]]]], [], 0, null, ["loc", [null, [17, 14], [20, 26]]]]],
              locals: [],
              templates: [child0]
            };
          })();
          var child1 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 22,
                      "column": 14
                    },
                    "end": {
                      "line": 25,
                      "column": 14
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
                },
                arity: 0,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("                ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("span");
                  dom.setAttribute(el1, "class", "glyphicon glyphicon-ban-circle");
                  dom.setAttribute(el1, "aria-hidden", "true");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n                ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(fragment, 3, 3, contextualElement);
                  return morphs;
                },
                statements: [["content", "node.id", ["loc", [null, [24, 16], [24, 27]]]]],
                locals: [],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 21,
                    "column": 12
                  },
                  "end": {
                    "line": 26,
                    "column": 12
                  }
                },
                "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createComment("");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
                dom.insertBoundary(fragment, 0);
                dom.insertBoundary(fragment, null);
                return morphs;
              },
              statements: [["block", "link-to", ["node", ["get", "model.id", ["loc", [null, [22, 32], [22, 40]]]], ["get", "node.id", ["loc", [null, [22, 41], [22, 48]]]]], ["class", "danger"], 0, null, ["loc", [null, [22, 14], [25, 26]]]]],
              locals: [],
              templates: [child0]
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 14,
                  "column": 8
                },
                "end": {
                  "line": 28,
                  "column": 8
                }
              },
              "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
            },
            arity: 1,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("          ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("li");
              var el2 = dom.createTextNode("\n");
              dom.appendChild(el1, el2);
              var el2 = dom.createComment("");
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("          ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
              return morphs;
            },
            statements: [["block", "if", [["get", "node.available", ["loc", [null, [16, 18], [16, 32]]]]], [], 0, 1, ["loc", [null, [16, 12], [26, 19]]]]],
            locals: ["node"],
            templates: [child0, child1]
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 12,
                "column": 4
              },
              "end": {
                "line": 30,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("ul");
            dom.setAttribute(el1, "class", "button-list");
            var el2 = dom.createTextNode("\n");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("      ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
            return morphs;
          },
          statements: [["block", "each", [["get", "model.nodes", ["loc", [null, [14, 16], [14, 27]]]]], [], 0, null, ["loc", [null, [14, 8], [28, 17]]]]],
          locals: [],
          templates: [child0]
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 30,
                "column": 4
              },
              "end": {
                "line": 32,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("p");
            var el2 = dom.createTextNode("No nodes detected");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 2
            },
            "end": {
              "line": 33,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.nodes", ["loc", [null, [12, 10], [12, 21]]]]], [], 0, 1, ["loc", [null, [12, 4], [32, 11]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 37,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/cluster/ops/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "cluster-information-container");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 1, 1);
        morphs[1] = dom.createMorphAt(element0, 3, 3);
        morphs[2] = dom.createMorphAt(dom.childAt(fragment, [2]), 1, 1);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "ops", "clusterId", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [4, 12], [4, 20]]]]], [], []]], ["loc", [null, [2, 2], [4, 22]]]], ["inline", "view-label", [], ["pre-label", "Cluster Ops", "label", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [7, 8], [7, 16]]]]], [], []]], ["loc", [null, [5, 2], [7, 18]]]], ["block", "dashboard-module", [], ["label", "Nodes"], 0, null, ["loc", [null, [11, 2], [33, 23]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/cluster/query/model', ['exports', 'ember-riak-explorer/pods/cluster/model'], function (exports, _emberRiakExplorerPodsClusterModel) {
  exports['default'] = _emberRiakExplorerPodsClusterModel['default'].extend({});
});
define('ember-riak-explorer/pods/cluster/query/route', ['exports', 'ember-riak-explorer/pods/cluster/route'], function (exports, _emberRiakExplorerPodsClusterRoute) {
  exports['default'] = _emberRiakExplorerPodsClusterRoute['default'].extend({});
});
define("ember-riak-explorer/pods/cluster/query/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 12,
                "column": 4
              },
              "end": {
                "line": 15,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/query/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "glyphicon glyphicon-plus");
            dom.setAttribute(el1, "aria-hidden", "true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      Create new search schema\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 17,
                "column": 4
              },
              "end": {
                "line": 19,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/query/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "search-indexes", [], ["indexes", ["subexpr", "@mut", [["get", "model.searchIndexes", ["loc", [null, [18, 31], [18, 50]]]]], [], []]], ["loc", [null, [18, 6], [18, 52]]]]],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 19,
                "column": 4
              },
              "end": {
                "line": 21,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/query/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("p");
            var el2 = dom.createTextNode("No search indexes found");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 2
            },
            "end": {
              "line": 22,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/query/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          morphs[1] = dom.createMorphAt(fragment, 2, 2, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "link-to", ["search-schema.create", ["get", "model.id", ["loc", [null, [12, 38], [12, 46]]]]], ["class", "btn btn-small btn-primary"], 0, null, ["loc", [null, [12, 4], [15, 16]]]], ["block", "if", [["get", "model.searchIndexes", ["loc", [null, [17, 10], [17, 29]]]]], [], 1, 2, ["loc", [null, [17, 4], [21, 11]]]]],
        locals: [],
        templates: [child0, child1, child2]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 26,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/cluster/query/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "cluster-information-container");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 1, 1);
        morphs[1] = dom.createMorphAt(element0, 3, 3);
        morphs[2] = dom.createMorphAt(dom.childAt(fragment, [2]), 1, 1);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "query", "clusterId", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [4, 12], [4, 20]]]]], [], []]], ["loc", [null, [2, 2], [4, 22]]]], ["inline", "view-label", [], ["pre-label", "Cluster Query", "label", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [7, 8], [7, 16]]]]], [], []]], ["loc", [null, [5, 2], [7, 18]]]], ["block", "dashboard-module", [], ["label", "Search Overview"], 0, null, ["loc", [null, [11, 2], [22, 23]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/cluster/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    actions: {
      error: function error(errors, transition) {
        var error = errors.errors[0];

        if (error && error.status === "404") {
          this.transitionTo('error.cluster-not-found', { queryParams: { cluster_id: transition.params.cluster_id } });
        } else {
          // Unknown error, bubble error event up to routes/application.js
          return true;
        }
      }
    },

    model: function model(params) {
      return this.explorer.getCluster(params.clusterId);
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model);
    }
  });
});
define('ember-riak-explorer/pods/config-file/model', ['exports', 'ember-data'], function (exports, _emberData) {
  exports['default'] = _emberData['default'].Model.extend({
    /**
     * Node the config file belongs to.
     * @property node
     * @type DS.Model
     */
    node: _emberData['default'].belongsTo('node', { async: true }),

    /**
     * Contents of the config file so they can be displayed in the ui.
     *
     * @property content
     * @type String
     */
    content: _emberData['default'].attr('string'),

    /**
     * The name of the file. The actual id is a composite id (i.e. cluster1/node1/file1.log).
     *
     * @property fileId
     * @type String
     */
    fileId: _emberData['default'].attr('string')
  });
});
define('ember-riak-explorer/pods/config-file/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {

    model: function model(params) {
      return this.explorer.getConfigFile(params.clusterId, params.nodeId, params.configId);
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('node').get('cluster'));
    }
  });
});
define("ember-riak-explorer/pods/config-file/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 17,
                "column": 2
              },
              "end": {
                "line": 19,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/config-file/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "model.content", ["loc", [null, [18, 4], [18, 21]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 12,
              "column": 0
            },
            "end": {
              "line": 20,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/config-file/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("header");
          dom.setAttribute(el1, "class", "module-header");
          var el2 = dom.createTextNode("\n    File output\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "code-highlighter", [], ["language-type", "js"], 0, null, ["loc", [null, [17, 2], [19, 23]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 21,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/config-file/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 1, 1);
        morphs[1] = dom.createMorphAt(element0, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "ops", "clusterId", ["subexpr", "@mut", [["get", "model.node.cluster.id", ["loc", [null, [4, 12], [4, 33]]]]], [], []], "nodeId", ["subexpr", "@mut", [["get", "model.node.id", ["loc", [null, [5, 9], [5, 22]]]]], [], []], "configId", ["subexpr", "@mut", [["get", "model.fileId", ["loc", [null, [6, 11], [6, 23]]]]], [], []]], ["loc", [null, [2, 2], [6, 25]]]], ["inline", "view-label", [], ["pre-label", "Config Detail", "label", ["subexpr", "@mut", [["get", "model.fileId", ["loc", [null, [9, 8], [9, 20]]]]], [], []]], ["loc", [null, [7, 2], [9, 22]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [12, 0], [20, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/log-file/controller', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller.extend({
    isRefreshing: false
  });
});
define('ember-riak-explorer/pods/log-file/model', ['exports', 'ember-data'], function (exports, _emberData) {
  exports['default'] = _emberData['default'].Model.extend({
    /**
     * Node the log file belongs to.
     * @property node
     * @type DS.Model
     */
    node: _emberData['default'].belongsTo('node', { async: true }),

    /**
     * Contents of the node file so they can be displayed in the ui. will be limited to the amount of
     *  lines specified in this.get('pageSize').
     *
     * @property content
     * @type String
     */
    content: _emberData['default'].attr('string'),

    /**
     * The name of the file. The actual id is a composite id (i.e. cluster1/node1/file1.log).
     *
     * @property fileId
     * @type String
     */
    fileId: _emberData['default'].attr('string'),

    /**
     * Tracks the state of whether the model is refetching this.get('content').
     *
     * @property isRefreshing
     * @type Boolean
     */
    isRefreshing: _emberData['default'].attr('boolean', { 'default': false }),

    /**
     * The max number of lines fetched in the request for this.get('content').
     *
     * @property pageSize
     * @type Integer
     */
    pageSize: _emberData['default'].attr('number', { 'default': 0 }),

    /**
     * The total number of lines in the file.
     *
     * @property totalLines
     * @type Integer
     */
    totalLines: _emberData['default'].attr('number', { 'default': 0 }),

    /**
     * Calculates whether there are more lines than the requested page size. Returns a
     * Boolean Value.
     *
     * @method moreLinesThanPageSize
     * @returns Boolean
     */
    moreLinesThanPageSize: (function () {
      return this.get('pageSize') < this.get('totalLines');
    }).property('pageSize', 'totalLines')
  });
});
define('ember-riak-explorer/pods/log-file/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {

    model: function model(params) {
      return this.explorer.getLogFile(params.clusterId, params.nodeId, params.logId);
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('node').get('cluster'));
    },

    actions: {
      refreshLogFile: function refreshLogFile(log) {
        var self = this;

        this.controllerFor('log-file').set('isRefreshing', true);

        // TODO: Add functionality to change the amount of lines the log file is tailing
        this.explorer.getLogFileContents(log).then(function () {
          // The response back from the server is very fast on a lot of these requests,
          //  so let the animation run for an extra second to give the feedback loop that
          //  the request has gone through.
          setTimeout(function () {
            return self.controllerFor('log-file').set('isRefreshing', false);
          }, 1000);
        });
      }
    }
  });
});
define("ember-riak-explorer/pods/log-file/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 14,
                "column": 4
              },
              "end": {
                "line": 18,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/log-file/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      File output from\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("code");
            var el2 = dom.createTextNode("tail -n ");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "mini-label");
            var el2 = dom.createTextNode("(");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode(" total lines)");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
            morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3]), 1, 1);
            return morphs;
          },
          statements: [["content", "model.pageSize", ["loc", [null, [16, 20], [16, 38]]]], ["content", "model.totalLines", ["loc", [null, [17, 32], [17, 52]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 18,
                "column": 4
              },
              "end": {
                "line": 21,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/pods/log-file/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      File output\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "mini-label");
            var el2 = dom.createTextNode("(");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode(" total lines)");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
            return morphs;
          },
          statements: [["content", "model.totalLines", ["loc", [null, [20, 32], [20, 52]]]]],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 31,
                "column": 2
              },
              "end": {
                "line": 33,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/log-file/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "model.content", ["loc", [null, [32, 4], [32, 21]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 12,
              "column": 0
            },
            "end": {
              "line": 34,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/log-file/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("header");
          dom.setAttribute(el1, "class", "module-header");
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "action-buttons");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("span");
          dom.setAttribute(el3, "class", "refresh action-button");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        Refresh ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(element0, [3, 1]);
          var element2 = dom.childAt(element1, [1]);
          var morphs = new Array(5);
          morphs[0] = dom.createMorphAt(element0, 1, 1);
          morphs[1] = dom.createElementMorph(element1);
          morphs[2] = dom.createAttrMorph(element2, 'class');
          morphs[3] = dom.createMorphAt(element1, 3, 3);
          morphs[4] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.moreLinesThanPageSize", ["loc", [null, [14, 10], [14, 37]]]]], [], 0, 1, ["loc", [null, [14, 4], [21, 11]]]], ["element", "action", ["refreshLogFile", ["get", "model", ["loc", [null, [24, 68], [24, 73]]]]], [], ["loc", [null, [24, 42], [24, 75]]]], ["attribute", "class", ["subexpr", "if", [["get", "isRefreshing", ["loc", [null, [25, 25], [25, 37]]]], "glyphicon glyphicon-refresh glyphicon-spin", "glyphicon glyphicon-refresh"], [], ["loc", [null, [25, 20], [25, 114]]]]], ["content", "model.fileId", ["loc", [null, [26, 16], [26, 32]]]], ["block", "code-highlighter", [], ["language-type", "js"], 2, null, ["loc", [null, [31, 2], [33, 23]]]]],
        locals: [],
        templates: [child0, child1, child2]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 37,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/log-file/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element3 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element3, 1, 1);
        morphs[1] = dom.createMorphAt(element3, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "ops", "clusterId", ["subexpr", "@mut", [["get", "model.node.cluster.id", ["loc", [null, [4, 12], [4, 33]]]]], [], []], "nodeId", ["subexpr", "@mut", [["get", "model.node.id", ["loc", [null, [5, 9], [5, 22]]]]], [], []], "logId", ["subexpr", "@mut", [["get", "model.fileId", ["loc", [null, [6, 8], [6, 20]]]]], [], []]], ["loc", [null, [2, 2], [6, 22]]]], ["inline", "view-label", [], ["pre-label", "Log Detail", "label", ["subexpr", "@mut", [["get", "model.fileId", ["loc", [null, [9, 8], [9, 20]]]]], [], []]], ["loc", [null, [7, 2], [9, 22]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [12, 0], [34, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/node/model', ['exports', 'ember-data', 'ember-riak-explorer/utils/riak-util'], function (exports, _emberData, _emberRiakExplorerUtilsRiakUtil) {
  exports['default'] = _emberData['default'].Model.extend({
    /**
     * Cluster the node belongs to.
     * @property cluster
     * @type DS.Model
     */
    cluster: _emberData['default'].belongsTo('cluster', { async: true }),

    /**
     * The nodes various configuration files
     * @property configFiles
     * @type DS.Model
     */
    configFiles: _emberData['default'].hasMany('config-file', { async: true }),

    /**
     * The nodes various log files
     * @property logFiles
     * @type DS.Model
     */
    logFiles: _emberData['default'].hasMany('log-file', { async: true }),

    /**
     * Whether or not the node is available when pinged.
     *
     * @property available
     * @type Boolean
     */
    available: _emberData['default'].attr('boolean', { 'default': false }),

    /**
     * All the nodes advanced configuration. Stored as an Array of strings.
     *  i.e ["{riak_core,[{cluster_mgr,{"127.0.0.1",8098}}]}", "{riak_repl,[{data_root,"/var/db/riak/riak_repl/"}]}"]
     *
     * @property config
     * @type Object
     */
    advancedConfig: _emberData['default'].attr(),

    /**
     * All the nodes configuration settings. Stored as an Object hashmap.
     *
     * @property config
     * @type Object
     */
    config: _emberData['default'].attr(),

    /**
     * All the nodes statistics. Stored as an Object hashmap.
     *
     * @property stats
     * @type Object
     */
    stats: _emberData['default'].attr(),

    /**
     * Whether or not the node's ring file is "valid" or "invalid".
     *
     * @property status
     * @type String
     */
    status: _emberData['default'].attr('string', { defaultValue: 'invalid' }),

    /**
     * All of the nodes configuration settings, stored as an array of key-value objects.
     *  ex. [{key: 'anti_entropy', value: 'active'}, {key: 'anti_entropy.bloomfilter', value: 'on'}...]
     *
     * @method configList
     * @return {Array<Object<Config>>}
     */
    configList: (function () {
      var configList = [];

      if (this.get('config')) {
        configList = (0, _emberRiakExplorerUtilsRiakUtil['default'])(this.get('config'));
      }

      return configList;
    }).property('config'),

    /**
     * Node health is determined by whether or not the node is available and if it's
     *  status is valid
     *
     * @method isHealthy
     * @return Boolean
     */
    isHealthy: (function () {
      return !!(this.get('available') && this.get('status') === 'valid');
    }).property('available', 'status'),

    /**
     * All of the nodes statistics, stored as an array of key-value objects.
     *  ex. [{key: 'asn1_version', value: '2.0.3'}, {key: 'basho_stats_version', value: '1.0.3'}...]
     *
     * @method statsList
     * @return {Array<Object<Stats>>}
     */
    statsList: (function () {
      var statsList = [];

      if (this.get('stats')) {
        statsList = (0, _emberRiakExplorerUtilsRiakUtil['default'])(this.get('stats'));
      }

      return statsList;
    }).property('stats')
  });
});
define('ember-riak-explorer/pods/node/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    model: function model(params) {
      return this.explorer.getNode(params.clusterId, params.nodeId);
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));
    }
  });
});
define("ember-riak-explorer/pods/node/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          var child0 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 14,
                    "column": 6
                  },
                  "end": {
                    "line": 14,
                    "column": 27
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("Statistics");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes() {
                return [];
              },
              statements: [],
              locals: [],
              templates: []
            };
          })();
          var child1 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 15,
                    "column": 6
                  },
                  "end": {
                    "line": 15,
                    "column": 30
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("Configuration");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes() {
                return [];
              },
              statements: [],
              locals: [],
              templates: []
            };
          })();
          var child2 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 16,
                    "column": 6
                  },
                  "end": {
                    "line": 16,
                    "column": 26
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("Log Files");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes() {
                return [];
              },
              statements: [],
              locals: [],
              templates: []
            };
          })();
          var child3 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 17,
                    "column": 6
                  },
                  "end": {
                    "line": 17,
                    "column": 29
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("Config Files");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes() {
                return [];
              },
              statements: [],
              locals: [],
              templates: []
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 13,
                  "column": 4
                },
                "end": {
                  "line": 18,
                  "column": 4
                }
              },
              "moduleName": "ember-riak-explorer/pods/node/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("      ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n      ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n      ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n      ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(4);
              morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
              morphs[1] = dom.createMorphAt(fragment, 3, 3, contextualElement);
              morphs[2] = dom.createMorphAt(fragment, 5, 5, contextualElement);
              morphs[3] = dom.createMorphAt(fragment, 7, 7, contextualElement);
              return morphs;
            },
            statements: [["block", "em-tab", [], [], 0, null, ["loc", [null, [14, 6], [14, 38]]]], ["block", "em-tab", [], [], 1, null, ["loc", [null, [15, 6], [15, 41]]]], ["block", "em-tab", [], [], 2, null, ["loc", [null, [16, 6], [16, 37]]]], ["block", "em-tab", [], [], 3, null, ["loc", [null, [17, 6], [17, 40]]]]],
            locals: [],
            templates: [child0, child1, child2, child3]
          };
        })();
        var child1 = (function () {
          var child0 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 23,
                    "column": 8
                  },
                  "end": {
                    "line": 28,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 1,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("tr");
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("td");
                dom.setAttribute(el2, "class", "key");
                var el3 = dom.createComment("");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("td");
                dom.setAttribute(el2, "class", "value");
                var el3 = dom.createComment("");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n          ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element1 = dom.childAt(fragment, [1]);
                var morphs = new Array(2);
                morphs[0] = dom.createMorphAt(dom.childAt(element1, [1]), 0, 0);
                morphs[1] = dom.createMorphAt(dom.childAt(element1, [3]), 0, 0);
                return morphs;
              },
              statements: [["content", "stat.key", ["loc", [null, [25, 28], [25, 40]]]], ["content", "stat.value", ["loc", [null, [26, 30], [26, 44]]]]],
              locals: ["stat"],
              templates: []
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 20,
                  "column": 4
                },
                "end": {
                  "line": 31,
                  "column": 4
                }
              },
              "moduleName": "ember-riak-explorer/pods/node/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("      ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("table");
              dom.setAttribute(el1, "class", "key-value-table");
              var el2 = dom.createTextNode("\n        ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("tbody");
              var el3 = dom.createTextNode("\n");
              dom.appendChild(el2, el3);
              var el3 = dom.createComment("");
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("        ");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n      ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1]), 1, 1);
              return morphs;
            },
            statements: [["block", "each", [["get", "model.statsList", ["loc", [null, [23, 16], [23, 31]]]]], [], 0, null, ["loc", [null, [23, 8], [28, 17]]]]],
            locals: [],
            templates: [child0]
          };
        })();
        var child2 = (function () {
          var child0 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 37,
                      "column": 10
                    },
                    "end": {
                      "line": 39,
                      "column": 10
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/node/template.hbs"
                },
                arity: 1,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("            ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("li");
                  var el2 = dom.createElement("code");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 0]), 0, 0);
                  return morphs;
                },
                statements: [["content", "advancedItem", ["loc", [null, [38, 22], [38, 38]]]]],
                locals: ["advancedItem"],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 34,
                    "column": 6
                  },
                  "end": {
                    "line": 43,
                    "column": 6
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("        ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("h4");
                var el2 = dom.createTextNode("Advanced");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n        ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("ul");
                dom.setAttribute(el1, "class", "code-list");
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("        ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n\n        ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("h4");
                var el2 = dom.createTextNode("General");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [3]), 1, 1);
                return morphs;
              },
              statements: [["block", "each", [["get", "model.advancedConfig", ["loc", [null, [37, 18], [37, 38]]]]], [], 0, null, ["loc", [null, [37, 10], [39, 19]]]]],
              locals: [],
              templates: [child0]
            };
          })();
          var child1 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 47,
                    "column": 8
                  },
                  "end": {
                    "line": 52,
                    "column": 8
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 1,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("          ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("tr");
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("td");
                dom.setAttribute(el2, "class", "key");
                var el3 = dom.createComment("");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("td");
                dom.setAttribute(el2, "class", "value");
                var el3 = dom.createComment("");
                dom.appendChild(el2, el3);
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("\n          ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var element0 = dom.childAt(fragment, [1]);
                var morphs = new Array(2);
                morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 0, 0);
                morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 0, 0);
                return morphs;
              },
              statements: [["content", "config.key", ["loc", [null, [49, 28], [49, 42]]]], ["content", "config.value", ["loc", [null, [50, 30], [50, 46]]]]],
              locals: ["config"],
              templates: []
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 33,
                  "column": 4
                },
                "end": {
                  "line": 55,
                  "column": 4
                }
              },
              "moduleName": "ember-riak-explorer/pods/node/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n      ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("table");
              dom.setAttribute(el1, "class", "key-value-table");
              var el2 = dom.createTextNode("\n        ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("tbody");
              var el3 = dom.createTextNode("\n");
              dom.appendChild(el2, el3);
              var el3 = dom.createComment("");
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("        ");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n      ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(2);
              morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
              morphs[1] = dom.createMorphAt(dom.childAt(fragment, [2, 1]), 1, 1);
              dom.insertBoundary(fragment, 0);
              return morphs;
            },
            statements: [["block", "if", [["get", "model.advancedConfig", ["loc", [null, [34, 12], [34, 32]]]]], [], 0, null, ["loc", [null, [34, 6], [43, 13]]]], ["block", "each", [["get", "model.configList", ["loc", [null, [47, 16], [47, 32]]]]], [], 1, null, ["loc", [null, [47, 8], [52, 17]]]]],
            locals: [],
            templates: [child0, child1]
          };
        })();
        var child3 = (function () {
          var child0 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 60,
                      "column": 10
                    },
                    "end": {
                      "line": 64,
                      "column": 10
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/node/template.hbs"
                },
                arity: 1,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("            ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("li");
                  var el2 = dom.createTextNode("\n              ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createComment("");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n            ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                  return morphs;
                },
                statements: [["inline", "link-to", [["get", "logFile.fileId", ["loc", [null, [62, 24], [62, 38]]]], "log-file", ["get", "model.cluster.id", ["loc", [null, [62, 50], [62, 66]]]], ["get", "model.id", ["loc", [null, [62, 67], [62, 75]]]], ["get", "logFile.fileId", ["loc", [null, [62, 76], [62, 90]]]]], [], ["loc", [null, [62, 14], [62, 92]]]]],
                locals: ["logFile"],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 58,
                    "column": 6
                  },
                  "end": {
                    "line": 66,
                    "column": 6
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("        ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("ul");
                dom.setAttribute(el1, "class", "button-list");
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("        ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                return morphs;
              },
              statements: [["block", "each", [["get", "model.logFiles", ["loc", [null, [60, 18], [60, 32]]]]], [], 0, null, ["loc", [null, [60, 10], [64, 19]]]]],
              locals: [],
              templates: [child0]
            };
          })();
          var child1 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 66,
                    "column": 6
                  },
                  "end": {
                    "line": 68,
                    "column": 6
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("        No log files for this node\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes() {
                return [];
              },
              statements: [],
              locals: [],
              templates: []
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 57,
                  "column": 4
                },
                "end": {
                  "line": 69,
                  "column": 4
                }
              },
              "moduleName": "ember-riak-explorer/pods/node/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
              dom.insertBoundary(fragment, 0);
              dom.insertBoundary(fragment, null);
              return morphs;
            },
            statements: [["block", "if", [["get", "model.logFiles", ["loc", [null, [58, 12], [58, 26]]]]], [], 0, 1, ["loc", [null, [58, 6], [68, 13]]]]],
            locals: [],
            templates: [child0, child1]
          };
        })();
        var child4 = (function () {
          var child0 = (function () {
            var child0 = (function () {
              return {
                meta: {
                  "revision": "Ember@1.13.11",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 74,
                      "column": 10
                    },
                    "end": {
                      "line": 78,
                      "column": 10
                    }
                  },
                  "moduleName": "ember-riak-explorer/pods/node/template.hbs"
                },
                arity: 1,
                cachedFragment: null,
                hasRendered: false,
                buildFragment: function buildFragment(dom) {
                  var el0 = dom.createDocumentFragment();
                  var el1 = dom.createTextNode("            ");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createElement("li");
                  var el2 = dom.createTextNode("\n              ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createComment("");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n            ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(1);
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                  return morphs;
                },
                statements: [["inline", "link-to", [["get", "configFile.fileId", ["loc", [null, [76, 24], [76, 41]]]], "config-file", ["get", "model.cluster.id", ["loc", [null, [76, 56], [76, 72]]]], ["get", "model.id", ["loc", [null, [76, 73], [76, 81]]]], ["get", "configFile.fileId", ["loc", [null, [76, 82], [76, 99]]]]], [], ["loc", [null, [76, 14], [76, 101]]]]],
                locals: ["configFile"],
                templates: []
              };
            })();
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 72,
                    "column": 6
                  },
                  "end": {
                    "line": 80,
                    "column": 6
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("        ");
                dom.appendChild(el0, el1);
                var el1 = dom.createElement("ul");
                dom.setAttribute(el1, "class", "button-list");
                var el2 = dom.createTextNode("\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
                dom.appendChild(el1, el2);
                var el2 = dom.createTextNode("        ");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                var el1 = dom.createTextNode("\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                var morphs = new Array(1);
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
                return morphs;
              },
              statements: [["block", "each", [["get", "model.configFiles", ["loc", [null, [74, 18], [74, 35]]]]], [], 0, null, ["loc", [null, [74, 10], [78, 19]]]]],
              locals: [],
              templates: [child0]
            };
          })();
          var child1 = (function () {
            return {
              meta: {
                "revision": "Ember@1.13.11",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 80,
                    "column": 6
                  },
                  "end": {
                    "line": 82,
                    "column": 6
                  }
                },
                "moduleName": "ember-riak-explorer/pods/node/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("        No config files for this node\n");
                dom.appendChild(el0, el1);
                return el0;
              },
              buildRenderNodes: function buildRenderNodes() {
                return [];
              },
              statements: [],
              locals: [],
              templates: []
            };
          })();
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 71,
                  "column": 4
                },
                "end": {
                  "line": 83,
                  "column": 4
                }
              },
              "moduleName": "ember-riak-explorer/pods/node/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
              dom.insertBoundary(fragment, 0);
              dom.insertBoundary(fragment, null);
              return morphs;
            },
            statements: [["block", "if", [["get", "model.configFiles", ["loc", [null, [72, 12], [72, 29]]]]], [], 0, 1, ["loc", [null, [72, 6], [82, 13]]]]],
            locals: [],
            templates: [child0, child1]
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 12,
                "column": 2
              },
              "end": {
                "line": 85,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/node/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(5);
            morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
            morphs[1] = dom.createMorphAt(fragment, 2, 2, contextualElement);
            morphs[2] = dom.createMorphAt(fragment, 4, 4, contextualElement);
            morphs[3] = dom.createMorphAt(fragment, 6, 6, contextualElement);
            morphs[4] = dom.createMorphAt(fragment, 8, 8, contextualElement);
            dom.insertBoundary(fragment, 0);
            return morphs;
          },
          statements: [["block", "em-tab-list", [], [], 0, null, ["loc", [null, [13, 4], [18, 20]]]], ["block", "em-tab-panel", [], [], 1, null, ["loc", [null, [20, 4], [31, 21]]]], ["block", "em-tab-panel", [], [], 2, null, ["loc", [null, [33, 4], [55, 21]]]], ["block", "em-tab-panel", [], [], 3, null, ["loc", [null, [57, 4], [69, 21]]]], ["block", "em-tab-panel", [], [], 4, null, ["loc", [null, [71, 4], [83, 21]]]]],
          locals: [],
          templates: [child0, child1, child2, child3, child4]
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 0
            },
            "end": {
              "line": 86,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/node/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "em-tabs", [], ["class", "half-width"], 0, null, ["loc", [null, [12, 2], [85, 14]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 87,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/node/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element2, 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "ops", "clusterId", ["subexpr", "@mut", [["get", "model.cluster.id", ["loc", [null, [4, 12], [4, 28]]]]], [], []], "nodeId", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [5, 9], [5, 17]]]]], [], []]], ["loc", [null, [2, 2], [5, 19]]]], ["inline", "view-label", [], ["pre-label", "Node Detail", "label", ["subexpr", "@mut", [["get", "model.id", ["loc", [null, [8, 8], [8, 16]]]]], [], []]], ["loc", [null, [6, 2], [8, 18]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [11, 0], [86, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/riak-object/controller', ['exports', 'ember'], function (exports, _ember) {

  var RiakObjectController = _ember['default'].Controller.extend({
    explorer: _ember['default'].inject.service('explorer'),

    actions: {
      deleteObject: function deleteObject(object) {
        this.get('explorer').deleteObject(object);
        this.get('explorer').markDeletedKey(object);

        // Once the delete has been issued,
        // return to the bucket's Key List view.
        this.transitionToRoute('bucket', object.get('bucket'));
      }
    }
  });
  exports['default'] = RiakObjectController;
});
define('ember-riak-explorer/pods/riak-object/counter/controller', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/controller'], function (exports, _ember, _emberRiakExplorerPodsRiakObjectController) {

  var RiakObjectCounterController = _emberRiakExplorerPodsRiakObjectController['default'].extend({
    actions: {
      incrementCounter: function incrementCounter(object) {
        this.get('explorer').updateDataType(object, 'increment');

        object.increment(object.get('incrementBy'));
      },
      decrementCounter: function decrementCounter(object) {
        this.get('explorer').updateDataType(object, 'decrement');

        object.decrement(object.get('decrementBy'));
      },
      // delay in milliseconds
      pollForModel: function pollForModel(object, delay) {
        var self = this;
        _ember['default'].run.later(function () {
          self.refreshModel(object);
        }, delay);
      },

      refreshModel: function refreshModel(object) {
        var controller = this;
        controller.get('explorer').getRiakObject(object.get('bucket'), object.get('key'), controller.store).then(function (object) {
          controller.set('model', object);
        });
      }
    }
  });

  exports['default'] = RiakObjectCounterController;
});
define('ember-riak-explorer/pods/riak-object/counter/model', ['exports', 'ember-data', 'ember-riak-explorer/pods/riak-object/model'], function (exports, _emberData, _emberRiakExplorerPodsRiakObjectModel) {

  /**
   * Represents a Riak Counter data type. Can be used standalone, or as a field
   *    inside of a Map (`RiakObjectMap`).
   * @class RiakObjectCounter
   * @extends RiakObject
   * @constructor
   * @see RiakObjectMap
   */
  var RiakObjectCounter = _emberRiakExplorerPodsRiakObjectModel['default'].extend({
    /**
     * The amount to decrement the counter by.
     * @property decrementBy
     * @type {Number}
     * @private
     */
    decrementBy: _emberData['default'].attr('integer', { defaultValue: 1 }),

    /**
     * The amount to increment the counter by.
     * @property incrementBy
     * @type {Number}
     * @private
     */
    incrementBy: _emberData['default'].attr('integer', { defaultValue: 1 }),

    /**
     * Can this object type be edited directly, in a text box?
     * @property canBeEdited
     * @readOnly
     * @default false
     * @type {Boolean}
     */
    canBeEdited: (function () {
      return false;
    }).property(),

    /**
     * Can this object be viewed/downloaded directly from the browser?
     * @property canBeViewedRaw
     * @readOnly
     * @default false
     * @type {Boolean}
     */
    canBeViewedRaw: (function () {
      return false;
    }).property(),

    /**
     * The JSON string representation of the Counter value.
     * @method contentsForDisplay
     * @return {String}
     */
    contentsForDisplay: (function () {
      return this.get('contents').value;
    }).property('contents'),

    /**
     * Decrements the counter by the specified amount.
     * @method decrement
     * @param {Number} amount
     * @async
     */
    decrement: function decrement(amount) {
      var newValue = this.get('contents').value - amount;

      this.set('contents', { value: newValue });
    },

    /**
     * Increments the counter by the specified amount.
     * @method increment
     * @param {Number} amount
     * @async
     */
    increment: function increment(amount) {
      var newValue = this.get('contents').value + amount;

      this.set('contents', { value: newValue });
    }
  });

  exports['default'] = RiakObjectCounter;
});
define("ember-riak-explorer/pods/riak-object/counter/route", ["exports", "ember-riak-explorer/pods/riak-object/route"], function (exports, _emberRiakExplorerPodsRiakObjectRoute) {

  var RiakObjectCounterRoute = _emberRiakExplorerPodsRiakObjectRoute["default"].extend({});

  exports["default"] = RiakObjectCounterRoute;
});
define("ember-riak-explorer/pods/riak-object/counter/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 15,
                "column": 2
              },
              "end": {
                "line": 20,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/counter/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "object-contents-counter", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [17, 10], [17, 15]]]]], [], []], "deleteObject", "deleteObject", "incrementCounter", "incrementCounter", "decrementCounter", "decrementCounter"], ["loc", [null, [16, 4], [19, 41]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 20,
                "column": 2
              },
              "end": {
                "line": 22,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/counter/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "loading-spinner", ["loc", [null, [21, 4], [21, 23]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 0
            },
            "end": {
              "line": 23,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/counter/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.isLoaded", ["loc", [null, [15, 8], [15, 22]]]]], [], 0, 1, ["loc", [null, [15, 2], [22, 9]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 24,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/riak-object/counter/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 1, 1);
        morphs[1] = dom.createMorphAt(element0, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "bucketTypeId", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [5, 15], [5, 33]]]]], [], []], "bucketId", ["subexpr", "@mut", [["get", "model.bucketId", ["loc", [null, [6, 11], [6, 25]]]]], [], []], "keyId", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [7, 8], [7, 17]]]]], [], []]], ["loc", [null, [2, 2], [8, 4]]]], ["inline", "view-label", [], ["pre-label", "Set Object", "label", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [11, 8], [11, 17]]]]], [], []]], ["loc", [null, [9, 2], [11, 19]]]], ["block", "dashboard-module", [], ["label", "Counter info", "class", "object-counter-container"], 0, null, ["loc", [null, [14, 0], [23, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/riak-object/edit/controller', ['exports', 'ember'], function (exports, _ember) {

  var RiakObjectEditController = _ember['default'].Controller.extend({
    explorer: _ember['default'].inject.service('explorer'),

    actions: {
      saveObject: function saveObject(object) {
        this.get('explorer').saveObject(object);
        object.set('isLoaded', false);
        this.transitionToRoute('riak-object', object);
      }
    }
  });
  exports['default'] = RiakObjectEditController;
});
define('ember-riak-explorer/pods/riak-object/edit/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {

  var RiakObjectEditRoute = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    model: function model(params) {
      var explorer = this.explorer;

      return explorer.getBucket(params.clusterId, params.bucketTypeId, params.bucketId).then(function (bucket) {
        return explorer.getRiakObject(bucket, params.key);
      });
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));
    }
  });

  exports['default'] = RiakObjectEditRoute;
});
define("ember-riak-explorer/pods/riak-object/edit/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 15,
                "column": 2
              },
              "end": {
                "line": 20,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("div");
            dom.setAttribute(el1, "class", "riak-object");
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n    ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(element0, 1, 1);
            morphs[1] = dom.createMorphAt(element0, 3, 3);
            return morphs;
          },
          statements: [["inline", "object-metadata", [], ["metadata", ["subexpr", "@mut", [["get", "model.metadata", ["loc", [null, [17, 33], [17, 47]]]]], [], []], "isEditing", true], ["loc", [null, [17, 6], [17, 64]]]], ["inline", "object-contents", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [18, 30], [18, 35]]]]], [], []], "isEditing", true, "saveObject", "saveObject"], ["loc", [null, [18, 6], [18, 76]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 20,
                "column": 2
              },
              "end": {
                "line": 22,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "loading-spinner", ["loc", [null, [21, 4], [21, 23]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 0
            },
            "end": {
              "line": 23,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.isLoaded", ["loc", [null, [15, 8], [15, 22]]]]], [], 0, 1, ["loc", [null, [15, 2], [22, 9]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 26,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element1 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element1, 1, 1);
        morphs[1] = dom.createMorphAt(element1, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "bucketTypeId", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [5, 15], [5, 33]]]]], [], []], "bucketId", ["subexpr", "@mut", [["get", "model.bucketId", ["loc", [null, [6, 11], [6, 25]]]]], [], []], "keyId", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [7, 8], [7, 17]]]]], [], []]], ["loc", [null, [2, 2], [8, 4]]]], ["inline", "view-label", [], ["pre-label", "Riak Object", "label", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [11, 8], [11, 17]]]]], [], []]], ["loc", [null, [9, 2], [11, 19]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [14, 0], [23, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/riak-object/embedded-map/model', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/map-field/model'], function (exports, _ember, _emberRiakExplorerPodsRiakObjectMapFieldModel) {

  /**
   * Represents a nested map field that lives in a standalone Riak Map data type.
   * Implements much of the field management API of a standalone `RiakObjectMap`.
   * @see RiakObjectMap
   * DS.Model name: 'riak-object.embedded-map'
   *
   * @class RiakObjectEmbeddedMap
   * @extends RiakObjectMapField
   * @constructor
   * @param fieldType {String} Valid Riak Map field type (must be 'map')
   * @param name {String} Name of nested map. Must end in `_map`
   * @param rootMap {RiakObjectMap} Top-level Map in which these fields will live
   * @param parentMap {RiakObjectMap|RiakObjectEmbeddedMap} Standalone or
   *           nested map containing these fields. When a map is nested just
   *           one level deep, the parentMap is same as rootMap. For fields
   *           nested several levels deep, the parent map will be an embedded
   *           map field.
   * @param value {Object} Hashmap of nested map fields, by field type.
   *            Empty value:
   *            `{ counters: {}, flags: {}, registers: {}, sets: {}, maps: {} }`
   */
  var RiakObjectEmbeddedMap = _emberRiakExplorerPodsRiakObjectMapFieldModel['default'].extend({

    /**
     * Adds a field to the appropriate field collection for this nested Map field.
     *
     * @method fieldType
     * @param fieldType {String} Field type ('register', 'flag' or 'counter')
     * @param field {RiakObjectMapField|RiakObjectEmbeddedMap}
     */
    addField: function addField(fieldType, field) {
      var fieldsCollectionName = fieldType + 's'; // pluralize
      var fieldsCollection = this.get(fieldsCollectionName); // flags, etc
      fieldsCollection[field.get('name')] = field;
      this.set(fieldsCollectionName, fieldsCollection);
      this.notifyPropertyChange('value');
      this.get('rootMap').notifyNestedFieldChange();
    },

    /**
     * Returns a list of Counters for this map, sorted by field name.
     *
     * @method countersList
     * @return {Array<RiakObjectMapField>}
     */
    countersList: (function countersList() {
      return this.fieldList('counters');
    }).property('counters'),

    isTopLevel: (function isTopLevel() {
      return false;
    }).property(),

    /**
     * Returns a list of a given field type for this map, sorted by field name.
     *
     * @method fieldList
     * @param fieldsCollectionName {String} Field type plural (registers, flags,
     *                                      sets, maps, counters)
     * @return {Array<RiakObjectMapField>} List of field instances of the given type
     */
    fieldList: function fieldList(fieldsCollectionName) {
      var list = [];
      var fields = this.get(fieldsCollectionName);
      for (var fieldName in fields) {
        list.push(fields[fieldName]);
      }
      return list.sortBy('name');
    },

    /**
     * Returns a list of Flags for this map, sorted by field name.
     *
     * @method flagsList
     * @return {Array<RiakObjectMapField>}
     */
    flagsList: (function flagsList() {
      return this.fieldList('flags');
    }).property('flags'),

    /**
     * Returns a list of embedded Maps in this map, sorted by field name.
     *
     * @method mapsList
     * @return {Array<RiakObjectEmbeddedMap>}
     */
    mapsList: (function mapsList() {
      return this.fieldList('maps');
    }).property('maps'),

    /**
     * Returns a list of Registers for this map, sorted by field name.
     *
     * @method registersList
     * @return {Array<RiakObjectMapField>}
     */
    registersList: (function registersList() {
      return this.fieldList('registers');
    }).property('registers'),

    /**
     * Adds a field to the appropriate field collection for this nested map field.
     *
     * @method removeField
     * @param fieldType {String} Field type
     *        ('register', 'flag', 'counter', 'set' or 'map')
     * @param field {RiakObjectMapField|RiakObjectEmbeddedMap}
     */
    removeField: function removeField(fieldType, field) {
      var fieldsCollectionName = fieldType + 's'; // pluralize
      var fieldsCollection = this.get(fieldsCollectionName); // flags, etc
      if (field.get('name') in fieldsCollection) {
        delete fieldsCollection[field.get('name')];
      }
      this.set(fieldsCollectionName, fieldsCollection);
      this.notifyPropertyChange('value');
      this.get('rootMap').notifyNestedFieldChange();
    },

    /**
     * Returns a list of Sets for this map, sorted by field name.
     *
     * @method setsList
     * @return {Array<RiakObjectMapField>}
     */
    setsList: (function setsList() {
      return this.fieldList('sets');
    }).property('sets'),

    /**
     * Hashmap of counters (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property counters
     * @type {Object}
     */
    counters: _ember['default'].computed('value', {
      get: function get() {
        return this.get('value').counters;
      },
      set: function set(key, val) {
        var contents = this.get('value');
        contents.counters = val;
        this.set('value', contents);
      }
    }),

    /**
     * Hashmap of flags (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property flags
     * @type {Object}
     */
    flags: _ember['default'].computed('value', {
      get: function get() {
        return this.get('value').flags;
      },
      set: function set(key, val) {
        var contents = this.get('value');
        contents.flags = val;
        this.set('value', contents);
      }
    }),

    /**
     * Hashmap of embedded Maps (`RiakObjectEmbeddedMap` instances) in this map,
     * keyed by field name.
     *
     * @property maps
     * @type {Object}
     */
    maps: _ember['default'].computed('value', {
      get: function get() {
        return this.get('value').maps;
      },
      set: function set(key, val) {
        var contents = this.get('value');
        contents.maps = val;
        this.set('value', contents);
      }
    }),

    /**
     * Hashmap of registers (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property registers
     * @type {Object}
     */
    registers: _ember['default'].computed('value', {
      get: function get() {
        return this.get('value').registers;
      },
      set: function set(key, val) {
        var contents = this.get('value');
        contents.registers = val;
        this.set('value', contents);
      }
    }),

    /**
     * Hashmap of sets (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property sets
     * @type {Object}
     */
    sets: _ember['default'].computed('value', {
      get: function get() {
        return this.get('value').sets;
      },
      set: function set(key, val) {
        var contents = this.get('value');
        contents.sets = val;
        this.set('value', contents);
      }
    })
  });

  exports['default'] = RiakObjectEmbeddedMap;
});
define('ember-riak-explorer/pods/riak-object/map/controller', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/controller'], function (exports, _ember, _emberRiakExplorerPodsRiakObjectController) {

  /**
   * @class RiakObjectMapController
   * @extends RiakObjectController
   * @constructor
   */
  var RiakObjectMapController = _emberRiakExplorerPodsRiakObjectController['default'].extend({
    actions: {
      /**
       Polls the server to refresh the model
       (kicks off a delayed call to +refreshModel+)
       @method pollForModel
       @param {RiakMapObject} model
       @param {Integer} delay Delay in milliseconds
       */
      pollForModel: function pollForModel(model, delay) {
        var self = this;
        _ember['default'].run.later(function () {
          self.refreshModel(model);
        }, delay);
      },

      /**
       Reloads the model from the server, updates the controller with it.
       @method refreshModel
       @param {RiakMapObject} model
       */
      refreshModel: function refreshModel(model) {
        var controller = this;
        controller.get('explorer').getRiakObject(model.get('bucket'), model.get('key'), controller.store).then(function (model) {
          controller.set('model', model);
        });
      }
    }
  });
  exports['default'] = RiakObjectMapController;
});
define('ember-riak-explorer/pods/riak-object/map/model', ['exports', 'ember-riak-explorer/pods/riak-object/model', 'ember'], function (exports, _emberRiakExplorerPodsRiakObjectModel, _ember) {

  /**
   * Represents a Riak Map server-side Data Type.
   * This is a flexible data structure with Hashmap/Dictionary-like semantics.
   * A Map contains one or more fields, which can be of type:
   *  - Register (`*_register`, string fields with a Last-Write-Received-Wins
   *      semantics)
   *  - Flags (`*_flag`, boolean fields, Enabled-wins-over-Disabled)
   *  - Sets (`*_set`, embedded sets)
   *  - Counters (`*_counter`, embedded counters)
   *  - Maps (`*_map`, a map can contain embedded maps within itself)
   *
   * In the HTTP API, the field type is denoted by the name suffix (so, a
   * Register type field name must end in `_register`).
   *
   * @see https://docs.basho.com/riak/latest/dev/using/data-types/#Maps
   *
   * @class RiakObjectMap
   * @constructor
   * @extends RiakObject
   * @uses RiakObjectMapField
   * @uses RiakObjectEmbeddedMap
   * @param [key] {String}
   * @param [bucket] {Bucket}
   * @param [bucketType] {BucketType}
   * @param [cluster] {Cluster}
   * @param [metadata] {ObjectMetadata}
   * @param [isLoaded] {Boolean} Has this been loaded from server. Default: `false`
   * @param [rawUrl] {String}
   * @param [contents] {Object} Hashmap of map fields, by field type.
   *            Empty contents:
   *            `{ counters: {}, flags: {}, registers: {}, sets: {}, maps: {} }`
   */
  var RiakObjectMap = _emberRiakExplorerPodsRiakObjectModel['default'].extend({
    /**
     * Adds a field to the appropriate field collection for this Map.
     *
     * @method fieldType
     * @param fieldType {String} Field type ('register', 'flag' or 'counter')
     * @param field {RiakObjectMapField}
     */
    addField: function addField(fieldType, field) {
      var fieldsCollectionName = fieldType + 's'; // pluralize
      var fieldsCollection = this.get(fieldsCollectionName); // flags, etc
      fieldsCollection[field.get('name')] = field;
      this.set(fieldsCollectionName, fieldsCollection);
      this.notifyPropertyChange('contents');
    },

    /**
     * Can this object type be edited directly, in a text box?
     *
     * @property canBeEdited
     * @readOnly
     * @default false
     * @type {Boolean}
     */
    canBeEdited: (function () {
      return false;
    }).property(),

    /**
     * Can this object be viewed/downloaded directly from the browser?
     *
     * @property canBeViewedRaw
     * @readOnly
     * @default false
     * @type {Boolean}
     */
    canBeViewedRaw: (function () {
      return false;
    }).property(),

    /**
     * The JSON string representation of the Map contents.
     *
     * @method contentsForDisplay
     * @return {String}
     */
    // TODO: this is throwing "Converting circular structure to JSON" error
    //contentsForDisplay: function() {
    //    return JSON.stringify(this.get('contents'));
    //}.property('contents'),

    /**
     * Returns a list of Counters for this map, sorted by field name.
     *
     * @method countersList
     * @return {Array<RiakObjectMapField>}
     */
    countersList: (function countersList() {
      return this.fieldList('counters');
    }).property('counters'),

    /**
     * Returns a list of a given field type for this map, sorted by field name.
     *
     * @method fieldList
     * @param fieldsCollectionName {String} Field type plural (registers, flags,
     *                                      sets, maps, counters)
     * @return {Array<RiakObjectMapField>} List of field instances of the given type
     */
    fieldList: function fieldList(fieldsCollectionName) {
      var list = [];
      var fields = this.get(fieldsCollectionName);
      for (var fieldName in fields) {
        list.push(fields[fieldName]);
      }
      return list.sortBy('name');
    },

    /**
     * Returns a list of Flags for this map, sorted by field name.
     *
     * @method flagsList
     * @return {Array<RiakObjectMapField>}
     */
    flagsList: (function flagsList() {
      return this.fieldList('flags');
    }).property('flags'),

    isTopLevel: (function isTopLevel() {
      return true;
    }).property(),

    /**
     * Returns a list of embedded Maps in this map, sorted by field name.
     *
     * @method mapsList
     * @return {Array<RiakObjectEmbeddedMap>}
     */
    mapsList: (function mapsList() {
      return this.fieldList('maps');
    }).property('maps'),

    notifyNestedFieldChange: function notifyNestedFieldChange() {
      this.notifyPropertyChange('contents');
    },

    /**
     * Returns a list of Registers for this map, sorted by field name.
     *
     * @method registersList
     * @return {Array<RiakObjectMapField>}
     */
    registersList: (function registersList() {
      return this.fieldList('registers');
    }).property('registers'),

    /**
     * Adds a field to the appropriate field collection for this Map.
     *
     * @method removeField
     * @param fieldType {String} Field type
     *        ('register', 'flag', 'counter', 'set' or 'map')
     * @param field {RiakObjectMapField|RiakObjectEmbeddedMap}
     */
    removeField: function removeField(fieldType, field) {
      var fieldsCollectionName = fieldType + 's'; // pluralize
      var fieldsCollection = this.get(fieldsCollectionName); // flags, etc
      if (field.get('name') in fieldsCollection) {
        delete fieldsCollection[field.get('name')];
      }
      this.set(fieldsCollectionName, fieldsCollection);
      this.notifyPropertyChange('contents');
    },

    /**
     * Returns a list of Sets for this map, sorted by field name.
     *
     * @method setsList
     * @return {Array<RiakObjectMapField>}
     */
    setsList: (function setsList() {
      return this.fieldList('sets');
    }).property('sets'),

    /**
     * Hashmap of counters (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property counters
     * @type {Object}
     */
    counters: _ember['default'].computed('contents', {
      get: function get() {
        return this.get('contents').counters;
      },
      set: function set(key, value) {
        var contents = this.get('contents');
        contents.counters = value;
        this.set('contents', contents);
      }
    }),

    /**
     * Hashmap of flags (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property flags
     * @type {Object}
     */
    flags: _ember['default'].computed('contents', {
      get: function get() {
        return this.get('contents').flags;
      },
      set: function set(key, value) {
        var contents = this.get('contents');
        contents.flags = value;
        this.set('contents', contents);
      }
    }),

    /**
     * Hashmap of embedded Maps (`RiakObjectEmbeddedMap` instances) in this map,
     * keyed by field name.
     *
     * @property maps
     * @type {Object}
     */
    maps: _ember['default'].computed('contents', {
      get: function get() {
        return this.get('contents').maps;
      },
      set: function set(key, value) {
        var contents = this.get('contents');
        contents.maps = value;
        this.set('contents', contents);
      }
    }),

    /**
     * Hashmap of registers (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property registers
     * @type {Object}
     */
    registers: _ember['default'].computed('contents', {
      get: function get() {
        return this.get('contents').registers;
      },
      set: function set(key, value) {
        var contents = this.get('contents');
        contents.registers = value;
        this.set('contents', contents);
      }
    }),

    /**
     * Hashmap of sets (`RiakObjectMapField` instances) for this map,
     * keyed by field name.
     *
     * @property sets
     * @type {Object}
     */
    sets: _ember['default'].computed('contents', {
      get: function get() {
        return this.get('contents').sets;
      },
      set: function set(key, value) {
        var contents = this.get('contents');
        contents.sets = value;
        this.set('contents', contents);
      }
    })
  });

  exports['default'] = RiakObjectMap;
});
define("ember-riak-explorer/pods/riak-object/map/route", ["exports", "ember-riak-explorer/pods/riak-object/route"], function (exports, _emberRiakExplorerPodsRiakObjectRoute) {

  var RiakObjectMapRoute = _emberRiakExplorerPodsRiakObjectRoute["default"].extend({});

  exports["default"] = RiakObjectMapRoute;
});
define("ember-riak-explorer/pods/riak-object/map/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 16,
                "column": 2
              },
              "end": {
                "line": 18,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/map/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "object-contents-map", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [17, 32], [17, 37]]]]], [], []], "deleteObject", "deleteObject"], ["loc", [null, [17, 4], [17, 67]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 18,
                "column": 2
              },
              "end": {
                "line": 20,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/map/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "loading-spinner", ["loc", [null, [19, 4], [19, 23]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 15,
              "column": 0
            },
            "end": {
              "line": 21,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/map/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.isLoaded", ["loc", [null, [16, 8], [16, 22]]]]], [], 0, 1, ["loc", [null, [16, 2], [20, 9]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 22,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/riak-object/map/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 1, 1);
        morphs[1] = dom.createMorphAt(element0, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "bucketTypeId", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [5, 15], [5, 33]]]]], [], []], "bucketId", ["subexpr", "@mut", [["get", "model.bucketId", ["loc", [null, [6, 11], [6, 25]]]]], [], []], "keyId", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [7, 8], [7, 17]]]]], [], []]], ["loc", [null, [2, 2], [8, 4]]]], ["inline", "view-label", [], ["pre-label", "Map Object", "label", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [11, 8], [11, 17]]]]], [], []]], ["loc", [null, [9, 2], [11, 19]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [15, 0], [21, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/riak-object/map-field/model', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Represents an individual map field that lives in a Riak Map data type.
   * DS.Model name: 'riak-object.map-field'
   * @see RiakObjectMap
   *
   * @class RiakObjectMapField
   * @extends DS.Model
   * @constructor
   * @param fieldType {String} Valid Riak Map field type
   * @param name {String} Name of field. Must end in `_<field type>`
   * @param rootMap {RiakObjectMap} Top-level Map in which these fields will live
   * @param parentMap {RiakObjectMap|RiakObjectEmbeddedMap} Standalone or
   *           nested map containing these fields. When a map is nested just
   *           one level deep, the parentMap is same as rootMap. For fields
   *           nested several levels deep, the parent map will be an embedded
   *           map field.
   * @param value {Object} Value/contents of the field.
   */
  var RiakObjectMapField = _emberData['default'].Model.extend({
    /**
     * Field type (one of: `register`, `flag`, `map`, `counter`, `set`)
     *
     * @property fieldType
     * @type String
     * @readOnly
     */
    fieldType: _emberData['default'].attr('string'),

    /**
     * Name of the map field (has to end in `_<field type>`).
     *
     * @property name
     * @type String
     * @readOnly
     */
    name: _emberData['default'].attr('string'),

    /**
     * Parent map (embedded or top-level) in which this field resides.
     *
     * @property parent
     * @type (RiakObjectMap|RiakObjectMapField)
     */
    parent: _emberData['default'].attr(),

    /**
     * The actual map containing these fields (this may be a Standalone top-level
     * map, or a nested map field. When a map is nested just one level deep, the
     * parentMap is same as rootMap. For fields nested several levels deep, the
     * parent map will be an embedded map field.
     *
     * @property parentMap
     * @type (RiakObjectMap|RiakObjectEmbeddedMap)
     */
    parentMap: _emberData['default'].attr(),

    /**
     * Top-level standalone map in which this field lives.
     *
     * @property rootMap
     * @type RiakObjectMap
     */
    rootMap: _emberData['default'].attr(),

    /**
     * Value/contents of the map field.
     * String values for Registers, boolean values for Flags,
     * arrays for Sets, numbers for Counters, and Object for Maps.
     *
     * @property value
     * @type (String|Boolean|Array|Object)
     */
    value: _emberData['default'].attr(),

    /**
     * Adds an element to this nested Set field and notifies parent map
     * that contents have changed.
     *
     * @method addElement
     * @param setField {RiakObjectMapField}
     * @param newElement {String}
     */
    addElement: function addElement(newElement) {
      if (!newElement) {
        return;
      }
      var set = this.get('value');
      if (newElement in set) {
        return;
      }
      set.push(newElement);
      this.set('value', set);
      this.get('rootMap').notifyNestedFieldChange();
    },

    /**
     * Returns the root map object's bucket.
     * Implemented as a property for compatibility in the code of
     * `ExplorerService.dataTypeActionFor`
     *
     * @property bucket
     * @type Bucket
     */
    bucket: (function bucket() {
      return this.get('rootMap').get('bucket');
    }).property('rootMap'),

    fullName: (function fullName() {
      if (this.get('parentMap').get('isTopLevel')) {
        return this.get('name');
      } else {
        return this.get('parentMap').get('fullName') + ' > ' + this.get('name');
      }
    }).property('name', 'parentMap'),

    /**
     * Returns the root map object's key.
     *
     * @property bucket
     * @type String
     */
    key: (function key() {
      return this.get('rootMap').get('key');
    }).property('rootMap'),

    /**
     * Ensures that a user-provided field name ends in `_<field type>`
     * (as is required by the HTTP API)
     *
     * @method normalizeName
     */
    normalizeName: function normalizeName() {
      var name = this.get('name');
      var suffix = '_' + this.get('fieldType');
      if (!name.endsWith(suffix)) {
        this.set('name', name + suffix);
      }
    },

    /**
     * Removes a given element from the nested Set field's contents and notifies
     * parent map that the field has changed.
     *
     * @method removeElement
     * @param {String} item Element to be removed
     */
    removeElement: function removeElement(item) {
      var set = this.get('value');
      var index = set.indexOf(item);
      if (index > -1) {
        set.splice(index, 1); // Remove item
      }
      this.set('value', set);
      this.get('rootMap').notifyNestedFieldChange();
    },

    valueForDisplay: (function valueForDisplay() {
      return JSON.stringify(this.get('value'));
    }).property('value')
  });

  exports['default'] = RiakObjectMapField;
});
define('ember-riak-explorer/pods/riak-object/model', ['exports', 'ember-data'], function (exports, _emberData) {

  /**
   * Represents a plain (non Data Type) Riak Object.
   *
   * @class RiakObject
   * @extends DS.Model
   * @constructor
   * @uses Bucket
   * @uses BucketType
   * @uses Cluster
   * @uses ObjectMetadata
   * @param [key] {String}
   * @param [bucket] {Bucket}
   * @param [bucketType] {BucketType}
   * @param [cluster] {Cluster}
   * @param [metadata] {ObjectMetadata}
   * @param [isLoaded] {Boolean} Has this been loaded from server. Default: `false`
   * @param [rawUrl] {String}
   * @param [contents] {Object} Object value/payload
   */
  var RiakObject = _emberData['default'].Model.extend({
    /**
     * Riak Bucket in which this object lives.
     * @property bucket
     * @type Bucket
     * @writeOnce
     */
    bucket: _emberData['default'].belongsTo('bucket'),

    /**
     * Riak Bucket Type in which this object lives.
     * @property bucketType
     * @type BucketType
     * @writeOnce
     */
    bucketType: _emberData['default'].belongsTo('bucket-type'),

    /**
     * Riak cluster in which this object lives.
     * @property cluster
     * @type Cluster
     * @writeOnce
     */
    cluster: _emberData['default'].belongsTo('cluster'),

    /**
     * Riak object headers/metadata.
     * @property metadata
     * @type ObjectMetadata
     */
    metadata: _emberData['default'].belongsTo('object-metadata'),

    /**
     * The value/contents of the object.
     * @property contents
     * @type Object
     */
    contents: _emberData['default'].attr(),

    /**
     * Has the object been fully loaded from the server?
     * @property isLoaded
     * @type Boolean
     * @default false
     */
    isLoaded: _emberData['default'].attr('boolean', { defaultValue: false }),

    /**
     * The object's primary key.
     * @property key
     * @type String
     * @writeOnce
     */
    key: _emberData['default'].attr('string'),

    /**
     * Was this object marked as deleted by Explorer UI?
     * Note: Deleted objects may still show up in the API-side key list cache.
     * @property markedDeleted
     * @type Boolean
     * @default false
     */
    markedDeleted: _emberData['default'].attr('boolean', { defaultValue: false }),

    /**
     * The URL to fetch the raw contents of the object directly from server.
     * Used with the 'View Raw' button.
     * @property rawUrl
     * @type String
     * @writeOnce
     */
    rawUrl: _emberData['default'].attr('string'),

    /**
     * @property bucketId
     * @type String
     */
    bucketId: (function () {
      return this.get('bucket').get('bucketId');
    }).property('bucket'),

    /**
     * @property bucketTypeId
     * @type String
     */
    bucketTypeId: (function () {
      return this.get('bucketType').get('bucketTypeId');
    }).property('bucket'),

    /**
     * Can this object type be edited directly, in a text box?
     * @property canBeEdited
     * @readOnly
     * @default true
     * @type {Boolean}
     */
    canBeEdited: (function () {
      return true;
    }).property(),

    /**
     * Can this object be viewed/downloaded directly from the browser?
     * @property canBeViewedRaw
     * @readOnly
     * @default true
     * @type {Boolean}
     */
    canBeViewedRaw: (function () {
      return true;
    }).property(),

    /**
     * Returns the name of the cluster in which this bucket type resides.
     * (As specified in the `riak_explorer.conf` file)
     * @property clusterId
     * @type String
     */
    clusterId: (function () {
      return this.get('cluster').get('clusterId');
    }).property('bucket'),

    /**
     * Returns a browser-displayable representation of the object value,
     *     if possible (based on the object's `contentType`).
     * @method contentsForDisplay
     * @return {String|Null}
     */
    contentsForDisplay: (function () {
      var contentType = this.get('metadata').get('contentType');
      var displayContents = undefined;
      // Determine whether this is browser-displayable contents
      if (contentType.startsWith('plain/text') || contentType.startsWith('application/json') || contentType.startsWith('application/xml') || contentType.startsWith('multipart/mixed')) {
        displayContents = this.get('contents');
      } else {
        displayContents = null;
      }
      return displayContents;
    }).property('contents', 'metadata'),

    /**
     * Returns true if the object has been deleted either on the server
     *    or via the Explorer app.
     * @method isDeleted
     * @return {Boolean}
     */
    isDeleted: (function () {
      var deletedOnRiak = false;
      if (this.get('metadata')) {
        deletedOnRiak = this.get('metadata').get('isDeleted');
      }
      return this.get('markedDeleted') || deletedOnRiak;
    }).property('markedDeleted', 'metadata')
  });

  exports['default'] = RiakObject;
});
define('ember-riak-explorer/pods/riak-object/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {

  var RiakObjectRoute = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    actions: {
      error: function error(_error, transition) {
        if (_error && _error.status === 404) {
          transition.queryParams = transition.params['riak-object'];
          this.transitionTo('error.object-not-found', transition);
        } else {
          // Unknown error, bubble error event up to routes/application.js
          return true;
        }
      }
    },

    model: function model(params) {
      var explorer = this.explorer;

      return explorer.getBucket(params.clusterId, params.bucketTypeId, params.bucketId).then(function (bucket) {
        return explorer.getRiakObject(bucket, params.key);
      });
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));
    },

    setupController: function setupController(controller, model) {
      this._super(controller, model);

      if (!model.get('isLoaded')) {
        this.explorer.getRiakObject(model.get('bucket'), model.get('key')).then(function (object) {
          controller.set('model', object);
        });
      }
    }
  });

  exports['default'] = RiakObjectRoute;
});
define('ember-riak-explorer/pods/riak-object/set/controller', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/controller'], function (exports, _ember, _emberRiakExplorerPodsRiakObjectController) {

  var RiakObjectSetController = _emberRiakExplorerPodsRiakObjectController['default'].extend({
    actions: {
      /**
       * Adds an element to the set.
       * @event addElement
       * @param {RiakSetObject} set
       * @param {String} newItem Element to be added
       */
      addElement: function addElement(set, newItem) {
        this.get('explorer').updateDataType(set, 'addElement', newItem);
        set.addElement(newItem);
      },

      /**
       * Polls the server to refresh the model
       * (kicks off a delayed call to +refreshModel+)
       * @param {RiakSetObject} model
       * @param {Integer} delay Delay in milliseconds
       */
      pollForModel: function pollForModel(model, delay) {
        var self = this;
        _ember['default'].run.later(function () {
          self.refreshModel(model);
        }, delay);
      },

      /**
       * Reloads the model from the server, updates the controller with it.
       * @param {RiakSetObject} model
       */
      refreshModel: function refreshModel(model) {
        var controller = this;
        controller.get('explorer').getRiakObject(model.get('bucket'), model.get('key'), controller.store).then(function (model) {
          controller.set('model', model);
        });
      },

      /**
       * Removes specified element from the set.
       * @event removeElement
       * @param set {RiakSetObject}
       * @param item {String} Element to be removed
       */
      removeElement: function removeElement(set, item) {
        this.get('explorer').updateDataType(set, 'removeElement', item);
        set.removeElement(item);
      }
    }
  });
  exports['default'] = RiakObjectSetController;
});
define('ember-riak-explorer/pods/riak-object/set/model', ['exports', 'ember-riak-explorer/pods/riak-object/model'], function (exports, _emberRiakExplorerPodsRiakObjectModel) {

  /**
   * Represents a Riak Set data type. Can be used standalone, or as a field
   *    inside of a Map (`RiakObjectMap`).
   *
   * @class RiakObjectSet
   * @extends RiakObject
   * @constructor
   * @see RiakObjectMap
   * @see RiakObjectEmbeddedMap
   */
  var RiakObjectSet = _emberRiakExplorerPodsRiakObjectModel['default'].extend({
    /**
     * Adds a given element to the set's contents.
     *
     * @method addElement
     * @param {String} item Element to be added
     */
    addElement: function addElement(item) {
      if (!item) {
        return;
      }
      var set = this.get('contents').value;
      var index = set.indexOf(item);
      if (index > -1) {
        set.push(item);
        this.set('contents', { value: set });
      }
    },

    /**
     * Can this object type be edited directly, in a text box?
     *
     * @property canBeEdited
     * @readOnly
     * @default false
     * @type {Boolean}
     */
    canBeEdited: (function () {
      return false;
    }).property(),

    /**
     * Can this object be viewed/downloaded directly from the browser?
     *
     * @property canBeViewedRaw
     * @readOnly
     * @default false
     * @type {Boolean}
     */
    canBeViewedRaw: (function () {
      return true;
    }).property(),

    /**
     * The JSON string representation of the Set contents.
     *
     * @method contentsForDisplay
     * @return {String}
     */
    contentsForDisplay: (function () {
      return this.get('contents').value;
    }).property('contents'),

    /**
     * Removes a given element from the set's contents.
     *
     * @method removeElement
     * @param {String} item Element to be removed
     */
    removeElement: function removeElement(item) {
      var set = this.get('contents').value;
      var index = set.indexOf(item);
      if (index > -1) {
        set.splice(index, 1); // Remove item
        this.set('contents', { value: set });
      }
    }
  });

  exports['default'] = RiakObjectSet;
});
define("ember-riak-explorer/pods/riak-object/set/route", ["exports", "ember-riak-explorer/pods/riak-object/route"], function (exports, _emberRiakExplorerPodsRiakObjectRoute) {

  var RiakObjectSetRoute = _emberRiakExplorerPodsRiakObjectRoute["default"].extend({});

  exports["default"] = RiakObjectSetRoute;
});
define("ember-riak-explorer/pods/riak-object/set/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 15,
                "column": 2
              },
              "end": {
                "line": 18,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/set/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "object-contents-set", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [16, 32], [16, 37]]]]], [], []], "deleteObject", "deleteObject", "removeElement", "removeElement", "addElement", "addElement"], ["loc", [null, [16, 4], [17, 59]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 18,
                "column": 2
              },
              "end": {
                "line": 20,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/set/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "loading-spinner", ["loc", [null, [19, 4], [19, 23]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 0
            },
            "end": {
              "line": 21,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/set/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.isLoaded", ["loc", [null, [15, 8], [15, 22]]]]], [], 0, 1, ["loc", [null, [15, 2], [20, 9]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 22,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/riak-object/set/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 1, 1);
        morphs[1] = dom.createMorphAt(element0, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "bucketTypeId", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [5, 15], [5, 33]]]]], [], []], "bucketId", ["subexpr", "@mut", [["get", "model.bucketId", ["loc", [null, [6, 11], [6, 25]]]]], [], []], "keyId", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [7, 8], [7, 17]]]]], [], []]], ["loc", [null, [2, 2], [8, 4]]]], ["inline", "view-label", [], ["pre-label", "Set Object", "label", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [11, 8], [11, 17]]]]], [], []]], ["loc", [null, [9, 2], [11, 19]]]], ["block", "dashboard-module", [], ["label", "Set data"], 0, null, ["loc", [null, [14, 0], [21, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/pods/riak-object/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 19,
                  "column": 6
                },
                "end": {
                  "line": 19,
                  "column": 38
                }
              },
              "moduleName": "ember-riak-explorer/pods/riak-object/template.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              return el0;
            },
            buildRenderNodes: function buildRenderNodes() {
              return [];
            },
            statements: [],
            locals: [],
            templates: []
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 15,
                "column": 2
              },
              "end": {
                "line": 21,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("div");
            dom.setAttribute(el1, "class", "riak-object");
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n    ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(3);
            morphs[0] = dom.createMorphAt(element0, 1, 1);
            morphs[1] = dom.createMorphAt(element0, 3, 3);
            morphs[2] = dom.createMorphAt(element0, 5, 5);
            return morphs;
          },
          statements: [["inline", "object-metadata", [], ["metadata", ["subexpr", "@mut", [["get", "model.metadata", ["loc", [null, [17, 33], [17, 47]]]]], [], []], "isEditing", false], ["loc", [null, [17, 6], [17, 65]]]], ["inline", "object-contents", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [18, 30], [18, 35]]]]], [], []], "isEditing", false, "deleteObject", "deleteObject"], ["loc", [null, [18, 6], [18, 81]]]], ["block", "object-version", [], ["object", ["subexpr", "@mut", [["get", "model", ["loc", [null, [19, 31], [19, 36]]]]], [], []]], 0, null, ["loc", [null, [19, 6], [19, 57]]]]],
          locals: [],
          templates: [child0]
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 21,
                "column": 2
              },
              "end": {
                "line": 23,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/riak-object/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "loading-spinner", ["loc", [null, [22, 4], [22, 23]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 0
            },
            "end": {
              "line": 24,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.isLoaded", ["loc", [null, [15, 8], [15, 22]]]]], [], 0, 1, ["loc", [null, [15, 2], [23, 9]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 27,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/riak-object/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element1 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element1, 1, 1);
        morphs[1] = dom.createMorphAt(element1, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "data", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "bucketTypeId", ["subexpr", "@mut", [["get", "model.bucketTypeId", ["loc", [null, [5, 15], [5, 33]]]]], [], []], "bucketId", ["subexpr", "@mut", [["get", "model.bucketId", ["loc", [null, [6, 11], [6, 25]]]]], [], []], "keyId", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [7, 8], [7, 17]]]]], [], []]], ["loc", [null, [2, 2], [8, 4]]]], ["inline", "view-label", [], ["pre-label", "Riak Object", "label", ["subexpr", "@mut", [["get", "model.key", ["loc", [null, [11, 8], [11, 17]]]]], [], []]], ["loc", [null, [9, 2], [11, 19]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [14, 0], [24, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/search-index/model', ['exports', 'ember-data'], function (exports, _emberData) {

  var SearchIndex = _emberData['default'].Model.extend({
    /**
     * Riak cluster the search index was created on
     *
     * @property cluster
     * @type {DS.Model} Cluster
     * @writeOnce
     */
    cluster: _emberData['default'].belongsTo('cluster', { async: true }),

    /**
     * Schema the search index is using
     *
     * @property schema
     * @type {DS.Model} Search Schema
     * @writeOnce
     */
    schema: _emberData['default'].belongsTo('search-schema', { async: true }),

    /**
     * Returns the search index name/id
     * @property name
     * @type String
     */
    name: _emberData['default'].attr('string'),

    /**
     * Returns the search index n value
     * @property nVal
     * @type Integer
     */
    nVal: _emberData['default'].attr('number', { defaultValue: 3 }),

    /**
     * Holds the value of the schema name that index is using.
     *  Temporary hack until basho-labs/riak_explorer#89 is completed
     * @property nVal
     * @type Integer
     */
    schemaRef: _emberData['default'].attr('string'),

    /**
     * Ember.Array of bucket types on the current cluster using the index
     * @property bucketTypesUsing
     * @type {Ember.Array} bucketTypes
     */
    bucketTypesUsing: (function () {
      var bucketTypes = this.get('cluster').get('bucketTypes');

      return bucketTypes.filterBy('index.name', this.get('name'));
    }).property('cluster.bucketTypes')
  });

  exports['default'] = SearchIndex;
});
define('ember-riak-explorer/pods/search-index/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    model: function model(params) {
      return this.explorer.getCluster(params.clusterId).then(function (cluster) {
        return cluster.get('searchIndexes').findBy('name', params.searchIndexId);
      });
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));
    }
  });
});
define("ember-riak-explorer/pods/search-index/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 26,
                "column": 8
              },
              "end": {
                "line": 28,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/pods/search-index/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "model.schema.name", ["loc", [null, [27, 10], [27, 31]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        var child0 = (function () {
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 36,
                  "column": 12
                },
                "end": {
                  "line": 40,
                  "column": 12
                }
              },
              "moduleName": "ember-riak-explorer/pods/search-index/template.hbs"
            },
            arity: 1,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("              ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("li");
              var el2 = dom.createTextNode("\n                ");
              dom.appendChild(el1, el2);
              var el2 = dom.createComment("");
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n              ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
              return morphs;
            },
            statements: [["inline", "link.bucket-type", [], ["bucketType", ["subexpr", "@mut", [["get", "bucketType", ["loc", [null, [38, 46], [38, 56]]]]], [], []], "btnBlock", true], ["loc", [null, [38, 16], [38, 72]]]]],
            locals: ["bucketType"],
            templates: []
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 34,
                "column": 8
              },
              "end": {
                "line": 42,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/pods/search-index/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("ul");
            dom.setAttribute(el1, "class", "button-list");
            var el2 = dom.createTextNode("\n");
            dom.appendChild(el1, el2);
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("          ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
            return morphs;
          },
          statements: [["block", "each", [["get", "model.bucketTypesUsing", ["loc", [null, [36, 20], [36, 42]]]]], [], 0, null, ["loc", [null, [36, 12], [40, 21]]]]],
          locals: [],
          templates: [child0]
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 42,
                "column": 8
              },
              "end": {
                "line": 44,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/pods/search-index/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          None\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 12,
              "column": 0
            },
            "end": {
              "line": 49,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/search-index/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1, "class", "key-value-table");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tbody");
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "key");
          var el5 = dom.createTextNode("Name");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "value");
          var el5 = dom.createComment("");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n    ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "key");
          var el5 = dom.createTextNode("N Value");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "value");
          var el5 = dom.createComment("");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n    ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "key");
          var el5 = dom.createTextNode("Schema");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "value");
          var el5 = dom.createTextNode("\n");
          dom.appendChild(el4, el5);
          var el5 = dom.createComment("");
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("      ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n    ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "key");
          var el5 = dom.createTextNode("Bucket Types Using Index");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4, "class", "value");
          var el5 = dom.createTextNode("\n");
          dom.appendChild(el4, el5);
          var el5 = dom.createComment("");
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("      ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n    ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var morphs = new Array(4);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]), 0, 0);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]), 0, 0);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 3]), 1, 1);
          morphs[3] = dom.createMorphAt(dom.childAt(element0, [7, 3]), 1, 1);
          return morphs;
        },
        statements: [["content", "model.name", ["loc", [null, [17, 24], [17, 38]]]], ["content", "model.nVal", ["loc", [null, [21, 24], [21, 38]]]], ["block", "link-to", ["search-schema", ["get", "model.cluster.id", ["loc", [null, [26, 35], [26, 51]]]], ["get", "model.schema.name", ["loc", [null, [26, 52], [26, 69]]]]], ["class", "btn btn-small btn-primary"], 0, null, ["loc", [null, [26, 8], [28, 20]]]], ["block", "if", [["get", "model.bucketTypesUsing", ["loc", [null, [34, 14], [34, 36]]]]], [], 1, 2, ["loc", [null, [34, 8], [44, 15]]]]],
        locals: [],
        templates: [child0, child1, child2]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 50,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/search-index/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element1 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element1, 1, 1);
        morphs[1] = dom.createMorphAt(element1, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "query", "clusterId", ["subexpr", "@mut", [["get", "model.cluster.id", ["loc", [null, [4, 12], [4, 28]]]]], [], []], "indexName", ["subexpr", "@mut", [["get", "model.name", ["loc", [null, [5, 12], [5, 22]]]]], [], []]], ["loc", [null, [2, 2], [6, 4]]]], ["inline", "view-label", [], ["pre-label", "Search Index", "label", ["subexpr", "@mut", [["get", "model.name", ["loc", [null, [9, 8], [9, 18]]]]], [], []]], ["loc", [null, [7, 2], [9, 20]]]], ["block", "dashboard-module", [], ["label", "Index Details"], 0, null, ["loc", [null, [12, 0], [49, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/search-schema/create/controller', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Controller.extend({
    schemaName: '',
    schemaContent: ''
  });
});
define('ember-riak-explorer/pods/search-schema/create/route', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Route.extend({
    model: function model(params) {
      return this.explorer.getCluster(params.clusterId);
    },

    afterModel: function afterModel(model, transition) {
      this.controllerFor('application').set('currentCluster', model);
    },

    actions: {
      createSchema: function createSchema(clusterId, schemaName, schemaContent) {
        var self = this;
        var xmlDoc = null;
        var url = '/riak/clusters/' + clusterId + '/search/schema/' + schemaName;

        try {
          xmlDoc = _ember['default'].$.parseXML(schemaContent);
        } catch (error) {
          // TODO: Put in proper error messaging
          alert('Invalid XML. Please check and make sure schema is valid xml.');
          return;
        }

        if (!_ember['default'].$(xmlDoc).find('schema').attr('name')) {
          // TODO: Put in proper error messaging
          alert('Solr requires that the schema tag has a name attribute. Please update your xml.');
          return;
        }

        if (!_ember['default'].$(xmlDoc).find('schema').attr('version')) {
          // TODO: Put in proper error messaging
          alert('Solr requires that the schema tag has a version attribute. Please update your xml.');
          return;
        }

        return _ember['default'].$.ajax({
          type: 'PUT',
          url: url,
          contentType: 'application/xml',
          processData: false,
          data: xmlDoc
        }).then(function (data) {
          self.transitionTo('search-schema', clusterId, schemaName);
        }, function (error) {
          // TODO: Put in proper error messaging
          alert('Something went wrong, schema was not saved.');
        });
      }
    }

  });
});
define("ember-riak-explorer/pods/search-schema/create/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 19,
                "column": 6
              },
              "end": {
                "line": 22,
                "column": 6
              }
            },
            "moduleName": "ember-riak-explorer/pods/search-schema/create/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "glyphicon glyphicon-remove");
            dom.setAttribute(el1, "aria-hidden", "true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n        Cancel\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 0
            },
            "end": {
              "line": 37,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/search-schema/create/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("header");
          dom.setAttribute(el1, "class", "module-header");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "action-buttons");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("span");
          dom.setAttribute(el3, "class", "create action-button");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "class", "glyphicon glyphicon-pencil");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        Create Schema\n      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("form");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "form-group");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("label");
          var el4 = dom.createTextNode("Schema Name");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "form-group");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("label");
          var el4 = dom.createTextNode("Schema XML");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morphs = new Array(4);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(element0, 3, 3);
          morphs[2] = dom.createMorphAt(dom.childAt(element2, [1]), 3, 3);
          morphs[3] = dom.createMorphAt(dom.childAt(element2, [3]), 3, 3);
          return morphs;
        },
        statements: [["element", "action", ["createSchema", ["get", "model.clusterId", ["loc", [null, [14, 65], [14, 80]]]], ["get", "schemaName", ["loc", [null, [14, 81], [14, 91]]]], ["get", "schemaContent", ["loc", [null, [14, 92], [14, 105]]]]], [], ["loc", [null, [14, 41], [14, 107]]]], ["block", "link-to", ["cluster.query", ["get", "model.clusterId", ["loc", [null, [19, 33], [19, 48]]]]], ["class", "cancel action-button"], 0, null, ["loc", [null, [19, 6], [22, 18]]]], ["inline", "input", [], ["value", ["subexpr", "@mut", [["get", "schemaName", ["loc", [null, [29, 20], [29, 30]]]]], [], []], "class", "form-control"], ["loc", [null, [29, 6], [29, 53]]]], ["inline", "textarea", [], ["value", ["subexpr", "@mut", [["get", "schemaContent", ["loc", [null, [34, 23], [34, 36]]]]], [], []], "rows", 10, "class", "form-control"], ["loc", [null, [34, 6], [34, 67]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 38,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/search-schema/create/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element3 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element3, 1, 1);
        morphs[1] = dom.createMorphAt(element3, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "query", "clusterId", ["subexpr", "@mut", [["get", "model.clusterId", ["loc", [null, [4, 12], [4, 27]]]]], [], []], "schemaCreate", true], ["loc", [null, [2, 2], [6, 4]]]], ["inline", "view-label", [], ["pre-label", "Create Schema"], ["loc", [null, [7, 2], [8, 29]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [11, 0], [37, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/search-schema/edit/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    model: function model(params) {
      return this.explorer.getCluster(params.clusterId).then(function (cluster) {
        return cluster.get('searchSchemas').findBy('name', params.searchSchemaId);
      });
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));

      if (!model.get('content')) {
        return _ember['default'].$.ajax({
          type: 'GET',
          url: model.get('url'),
          dataType: 'xml'
        }).then(function (data) {
          var xmlString = new XMLSerializer().serializeToString(data);
          model.set('content', xmlString);
        });
      }
    },

    actions: {
      updateSchema: function updateSchema(schema) {
        var xmlString = schema.get('content');
        var self = this;
        var xmlDoc = null;
        var clusterId = schema.get('cluster').get('id');
        var schemaId = schema.get('name');

        try {
          xmlDoc = _ember['default'].$.parseXML(xmlString);
        } catch (error) {
          // TODO: Put in proper error messaging
          alert('Invalid XML. Please check and make sure schema is valid xml.');
          return;
        }

        return _ember['default'].$.ajax({
          type: 'PUT',
          url: schema.get('url'),
          contentType: 'application/xml',
          processData: false,
          data: xmlDoc
        }).then(function (data) {
          self.transitionTo('search-schema', clusterId, schemaId);
        }, function (error) {
          // TODO: Put in proper error messaging
          alert('Something went wrong, schema was not saved.');
          self.transitionTo('search-schema', clusterId, schemaId);
        });
      }
    }
  });
});
define("ember-riak-explorer/pods/search-schema/edit/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 21,
                "column": 6
              },
              "end": {
                "line": 24,
                "column": 6
              }
            },
            "moduleName": "ember-riak-explorer/pods/search-schema/edit/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "glyphicon glyphicon-remove");
            dom.setAttribute(el1, "aria-hidden", "true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n        Cancel\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 13,
              "column": 0
            },
            "end": {
              "line": 35,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/search-schema/edit/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("header");
          dom.setAttribute(el1, "class", "module-header");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "action-buttons");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("span");
          dom.setAttribute(el3, "class", "update action-button");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "class", "glyphicon glyphicon-pencil");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        Update Schema\n      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("pre");
          dom.setAttribute(el1, "class", "editable");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("code");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [1]);
          var morphs = new Array(3);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(element0, 3, 3);
          morphs[2] = dom.createMorphAt(dom.childAt(fragment, [3, 1]), 1, 1);
          return morphs;
        },
        statements: [["element", "action", ["updateSchema", ["get", "model", ["loc", [null, [16, 65], [16, 70]]]]], [], ["loc", [null, [16, 41], [16, 72]]]], ["block", "link-to", ["search-schema", ["get", "model.cluster.id", ["loc", [null, [21, 33], [21, 49]]]], ["get", "model.name", ["loc", [null, [21, 50], [21, 60]]]]], ["class", "cancel action-button"], 0, null, ["loc", [null, [21, 6], [24, 18]]]], ["inline", "content-editable", [], ["value", ["subexpr", "@mut", [["get", "model.content", ["loc", [null, [31, 12], [31, 25]]]]], [], []], "type", "html"], ["loc", [null, [30, 6], [32, 19]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 36,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/search-schema/edit/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element2, 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "query", "clusterId", ["subexpr", "@mut", [["get", "model.cluster.id", ["loc", [null, [4, 12], [4, 28]]]]], [], []], "schemaName", ["subexpr", "@mut", [["get", "model.name", ["loc", [null, [5, 13], [5, 23]]]]], [], []], "schemaEdit", true], ["loc", [null, [2, 2], [7, 4]]]], ["inline", "view-label", [], ["pre-label", "Search Schema", "label", ["subexpr", "@mut", [["get", "model.name", ["loc", [null, [10, 8], [10, 18]]]]], [], []]], ["loc", [null, [8, 2], [10, 20]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [13, 0], [35, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/pods/search-schema/model', ['exports', 'ember-data'], function (exports, _emberData) {
  exports['default'] = _emberData['default'].Model.extend({
    /**
     * Riak cluster the search schema was created on
     *
     * @property cluster
     * @type {DS.Model} Cluster
     * @writeOnce
     */
    cluster: _emberData['default'].belongsTo('cluster', { async: true }),

    name: _emberData['default'].attr('string'),

    content: _emberData['default'].attr(),

    /**
     * Returns a formatted schema url
     * @method url
     * @returns String
     */
    url: (function () {
      var proxyURL = this.get('cluster').get('proxyUrl');
      var name = this.get('name');

      return proxyURL + '/search/schema/' + name;
    }).property('name', 'cluster.proxyUrl')
  });
});
define('ember-riak-explorer/pods/search-schema/route', ['exports', 'ember', 'ember-riak-explorer/mixins/sidebar-select'], function (exports, _ember, _emberRiakExplorerMixinsSidebarSelect) {
  exports['default'] = _ember['default'].Route.extend(_emberRiakExplorerMixinsSidebarSelect['default'], {
    model: function model(params) {
      var self = this;

      return this.explorer.getCluster(params.clusterId).then(function (cluster) {
        var schema = cluster.get('searchSchemas').findBy('name', params.searchSchemaId);

        if (!schema) {
          schema = self.explorer.createSchema(params.searchSchemaId, cluster);
        }

        return schema;
      });
    },

    afterModel: function afterModel(model, transition) {
      this.setSidebarCluster(model.get('cluster'));

      return _ember['default'].$.ajax({
        type: 'GET',
        url: model.get('url'),
        dataType: 'xml'
      }).then(function (data) {
        var xmlString = new XMLSerializer().serializeToString(data);
        model.set('content', xmlString);
      });
    }
  });
});
define("ember-riak-explorer/pods/search-schema/template", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 15,
                "column": 6
              },
              "end": {
                "line": 18,
                "column": 6
              }
            },
            "moduleName": "ember-riak-explorer/pods/search-schema/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "glyphicon glyphicon-pencil");
            dom.setAttribute(el1, "aria-hidden", "true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n        Edit Schema\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 27,
                "column": 2
              },
              "end": {
                "line": 29,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/pods/search-schema/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "model.content", ["loc", [null, [28, 4], [28, 21]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 12,
              "column": 0
            },
            "end": {
              "line": 30,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/pods/search-schema/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("header");
          dom.setAttribute(el1, "class", "module-header");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "action-buttons");
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("a");
          dom.setAttribute(el3, "target", "_blank");
          dom.setAttribute(el3, "class", "raw action-button");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "class", "glyphicon glyphicon-eye-open");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        View Raw\n      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [3]);
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(element0, 1, 1);
          morphs[1] = dom.createAttrMorph(element1, 'href');
          morphs[2] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "link-to", ["search-schema.edit", ["get", "model.cluster.id", ["loc", [null, [15, 38], [15, 54]]]], ["get", "model.name", ["loc", [null, [15, 55], [15, 65]]]]], ["class", "edit action-button"], 0, null, ["loc", [null, [15, 6], [18, 18]]]], ["attribute", "href", ["get", "model.url", ["loc", [null, [20, 16], [20, 25]]]]], ["block", "code-highlighter", [], ["language-type", "XML"], 1, null, ["loc", [null, [27, 2], [29, 23]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 31,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/search-schema/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element2, 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["subSection", "query", "clusterId", ["subexpr", "@mut", [["get", "model.cluster.id", ["loc", [null, [4, 12], [4, 28]]]]], [], []], "schemaName", ["subexpr", "@mut", [["get", "model.name", ["loc", [null, [5, 13], [5, 23]]]]], [], []]], ["loc", [null, [2, 2], [6, 4]]]], ["inline", "view-label", [], ["pre-label", "Search Schema", "label", ["subexpr", "@mut", [["get", "model.name", ["loc", [null, [9, 8], [9, 18]]]]], [], []]], ["loc", [null, [7, 2], [9, 20]]]], ["block", "dashboard-module", [], [], 0, null, ["loc", [null, [12, 0], [30, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define('ember-riak-explorer/router', ['exports', 'ember', 'ember-riak-explorer/config/environment'], function (exports, _ember, _emberRiakExplorerConfigEnvironment) {

  var Router = _ember['default'].Router.extend({
    location: _emberRiakExplorerConfigEnvironment['default'].locationType
  });

  Router.map(function () {
    // Root Route
    this.route('explorer_api');

    // Cluster Routes
    this.route('cluster.ops', { path: '/cluster/:clusterId/ops' });
    this.route('cluster.data', { path: '/cluster/:clusterId/data' });
    this.route('cluster.query', { path: '/cluster/:clusterId/query' });

    // Cluster-Data Routes
    this.route('bucket-type', { path: '/cluster/:clusterId/data/bucket_type/:bucketTypeId' });
    this.route('bucket', { path: '/cluster/:clusterId/data/bucket_type/:bucketTypeId/bucket/:bucketId' });
    this.route('riak-object', { path: '/cluster/:clusterId/data/bucket_type/:bucketTypeId/bucket/:bucketId/key/:key' });
    this.route('riak-object.edit', { path: '/cluster/:clusterId/data/bucket_type/:bucketTypeId/bucket/:bucketId/key/:key/edit' });
    this.route('riak-object.counter', { path: '/cluster/:clusterId/data/bucket_type/:bucketTypeId/bucket/:bucketId/counter/:key' });
    this.route('riak-object.set', { path: '/cluster/:clusterId/data/bucket_type/:bucketTypeId/bucket/:bucketId/set/:key' });
    this.route('riak-object.map', { path: '/cluster/:clusterId/data/bucket_type/:bucketTypeId/bucket/:bucketId/map/:key' });

    // Cluster-Ops Routes
    this.route('node', { path: '/cluster/:clusterId/ops/nodes/:nodeId/' });
    this.route('log-file', { path: '/cluster/:clusterId/ops/nodes/:nodeId/logs/:logId' });
    this.route('config-file', { path: '/cluster/:clusterId/ops/nodes/:nodeId/configs/:configId' });

    // Cluster-Query Routes
    this.route('search-index', { path: '/cluster/:clusterId/query/index/:searchIndexId' });
    this.route('search-schema', { path: '/cluster/:clusterId/query/schema/:searchSchemaId' });
    this.route('search-schema.edit', { path: '/cluster/:clusterId/query/schema/:searchSchemaId/edit' });
    this.route('search-schema.create', { path: '/cluster/:clusterId/query/schema/create' });

    // Error Routes
    this.route('error', { path: '/error' }, function () {
      this.route('unknown');
      this.route('cluster-not-found');
      this.route('object-not-found');
      this.route('service-not-found');
    });

    this.route('search-index', { path: '/cluster/:clusterId/index/:searchIndexId' });

    this.route('search-schema', { path: '/cluster/:clusterId/schema/:searchSchemaId' });
    this.route('search-schema.edit', { path: '/cluster/:clusterId/schema/:searchSchemaId/edit' });
    this.route('search-schema.create', { path: '/cluster/:clusterId/schema/create' });
  });

  exports['default'] = Router;
});
define('ember-riak-explorer/routes/application', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Route.extend({
    actions: {
      error: function error(_error) {
        // An error has occurred that wasn't handled by any route.
        console.log('Unknown error: %O', _error);
        this.transitionTo('error.unknown');
      }
    },

    // Load the list of available clusters, for the left nav
    model: function model() {
      var self = this;

      return this.store.findAll('cluster').then(function (data) {
        return data;
      }, function (error) {
        self.transitionTo('error.service-not-found');
      });
    }
  });
});
define('ember-riak-explorer/routes/error/cluster-not-found', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Route.extend({
    queryParams: {
      cluster_id: {
        refreshModel: true
      }
    },

    model: function model(params) {
      return {
        clusterId: params.cluster_id
      };
    }
  });
});
define('ember-riak-explorer/routes/error/object-not-found', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Route.extend({
    queryParams: {
      clusterId: {
        refreshModel: true
      },
      bucketTypeId: {
        refreshModel: true
      },
      bucketId: {
        refreshModel: true
      },
      key: {
        refreshModel: true
      }
    },

    model: function model(params) {
      return params;
    }
  });
});
define('ember-riak-explorer/routes/error/service-not-found', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Route.extend({});
});
define('ember-riak-explorer/routes/error/unknown', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Route.extend({});
});
define('ember-riak-explorer/routes/explorer-api', ['exports', 'ember', 'ember-riak-explorer/config/environment'], function (exports, _ember, _emberRiakExplorerConfigEnvironment) {
  exports['default'] = _ember['default'].Route.extend({
    model: function model() {
      var serviceName = 'Riak Explorer';
      var pingUrl = _emberRiakExplorerConfigEnvironment['default'].baseURL + 'explore/ping';
      var propsUrl = _emberRiakExplorerConfigEnvironment['default'].baseURL + 'explore/props';

      return new _ember['default'].RSVP.hash({
        service: serviceName,
        pingResult: _ember['default'].$.ajax({ url: pingUrl, dataType: "json" }),
        propsResult: _ember['default'].$.ajax({ url: propsUrl, dataType: "json" }),
        routes: this.store.find('route')
      });
    }
  });
});
define('ember-riak-explorer/routes/index', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = _ember['default'].Route.extend({});
});
define('ember-riak-explorer/serializers/application', ['exports', 'ember-data', 'ember'], function (exports, _emberData, _ember) {
  exports['default'] = _emberData['default'].RESTSerializer.extend(_emberData['default'].EmbeddedRecordsMixin, {
    // Specify embedded attributes
    attrs: {
      // Bucket Type properties and Bucket properties
      props: {
        embedded: 'always'
      }
    },

    /**
     This indicates that the
     store should call `normalizeResponse` instead of `extract` and to expect
     a JSON-API Document back.
     @property isNewSerializerAPI
     */
    isNewSerializerAPI: true,

    /**
     `keyForAttribute` can be used to define rules for how to convert an
     attribute name in your model to a key in your JSON.
      @method keyForAttribute
     @param {String} key
     @param {String} method
     @return {String} normalized key
     */
    keyForAttribute: function keyForAttribute(attr, method) {
      // Riak and Explorer json uses snake case, like 'development_mode'
      return _ember['default'].String.underscore(attr);
    },

    normalizeResponse: function normalizeResponse(store, primaryModelClass, payload, id, requestType) {
      var sortBy = _ember['default'].Enumerable.sortBy;

      if (payload.clusters) {
        payload.clusters = payload.clusters.sortBy('id');
      }

      return this._super(store, primaryModelClass, payload, id, requestType);
    }
  });
});
define('ember-riak-explorer/serializers/bucket-type', ['exports', 'ember-riak-explorer/serializers/application', 'ember'], function (exports, _emberRiakExplorerSerializersApplication, _ember) {
  exports['default'] = _emberRiakExplorerSerializersApplication['default'].extend({
    normalizeResponse: function normalizeResponse(store, primaryModelClass, payload, id, requestType) {
      var sortBy = _ember['default'].Enumerable.sortBy;

      payload.bucket_types = payload.bucket_types.sortBy('original_id');

      return this._super(store, primaryModelClass, payload, id, requestType);
    }
  });
});
define('ember-riak-explorer/serializers/config-file', ['exports', 'ember-riak-explorer/serializers/application', 'ember'], function (exports, _emberRiakExplorerSerializersApplication, _ember) {
  exports['default'] = _emberRiakExplorerSerializersApplication['default'].extend({
    normalizeResponse: function normalizeResponse(store, primaryModelClass, payload, id, requestType) {
      var newPayload = {
        'config-files': payload.files
      };

      return this._super(store, primaryModelClass, newPayload, id, requestType);
    }
  });
});
define('ember-riak-explorer/serializers/log-file', ['exports', 'ember-riak-explorer/serializers/application', 'ember'], function (exports, _emberRiakExplorerSerializersApplication, _ember) {
  exports['default'] = _emberRiakExplorerSerializersApplication['default'].extend({
    normalizeResponse: function normalizeResponse(store, primaryModelClass, payload, id, requestType) {
      var newPayload = {
        'log-files': payload.files
      };

      return this._super(store, primaryModelClass, newPayload, id, requestType);
    }
  });
});
define('ember-riak-explorer/serializers/node', ['exports', 'ember-riak-explorer/serializers/application', 'ember'], function (exports, _emberRiakExplorerSerializersApplication, _ember) {
  exports['default'] = _emberRiakExplorerSerializersApplication['default'].extend({
    normalizeResponse: function normalizeResponse(store, primaryModelClass, payload, id, requestType) {
      var sortBy = _ember['default'].Enumerable.sortBy;

      payload.nodes = payload.nodes.sortBy('id');

      return this._super(store, primaryModelClass, payload, id, requestType);
    }
  });
});
define('ember-riak-explorer/serializers/search-index', ['exports', 'ember-riak-explorer/serializers/application'], function (exports, _emberRiakExplorerSerializersApplication) {
  exports['default'] = _emberRiakExplorerSerializersApplication['default'].extend({
    normalizeResponse: function normalizeResponse(store, primaryModelClass, payload, id, requestType) {
      var newPayload = {
        'search-indexes': payload
      };

      return this._super(store, primaryModelClass, newPayload, id, requestType);
    },

    // TODO: Remove once basho-labs/riak_explorer#89 is completed
    normalize: function normalize(modelClass, resourceHash, prop) {
      resourceHash.schema_ref = resourceHash.schema;
      delete resourceHash.schema;

      return this._super(modelClass, resourceHash, prop);
    }
  });
});
define('ember-riak-explorer/services/explorer', ['exports', 'ember', 'ember-riak-explorer/config/environment', 'ember-riak-explorer/utils/riak-util'], function (exports, _ember, _emberRiakExplorerConfigEnvironment, _emberRiakExplorerUtilsRiakUtil) {

  /**
   * An Ember service responsible for AJAX communication with the Explorer API.
   *
   * @class ExplorerService
   * @extends Ember.Service
   * @constructor
   * @uses Bucket
   * @uses BucketType
   * @uses BucketProps
   * @uses Cluster
   * @uses RiakObject
   * @uses ObjectMetadata
   */
  exports['default'] = _ember['default'].Service.extend({
    /**
     * User-configurable URL prefix for the Explorer GUI.
     * (Also the URL prefix for the Explorer API).
     * Currently, the options are: '/' or '/admin/'.
     *
     * @property apiURL
     * @type String
     * @default '/'
     */
    apiURL: _emberRiakExplorerConfigEnvironment['default'].baseURL,

    name: 'explorer',

    availableIn: ['controllers', 'routes'],

    /**
     * Default chunk size for requests that can potentially have large amounts of records
     * i.e. buckets and keys
     *
     * @property pageSize
     * @type Integer
     * @default 500
     */
    pageSize: 500,

    /**
     * The 'deleted' cache is a way for the Ember GUI to keep track of which
     * objects have been deleted via user actions.
     *
     * Currently, deleting an object does not remove
     * its key from the Explorer API key list cache.
     * So, when a user presses 'Delete' on an object, they are returned to the
     * cached Key List for that bucket. But since the deleted object's key was
     * not removed from the cache, the key shows up.
     * To account for this and to provide a better user experience, this cache
     * was implemented.
     *
     * This cache tracks object deletions keyed by cluster/bucket type/bucket.
     *
     * @todo If/when the Explorer API caching code supports the removal of a key
     *     on object delete, this logic will be obsolete.
     *
     * @property deleted
     * @type Hash
     */
    deleted: {
      clusters: {}
    },

    /**
     * Re-populates the Bucket List cached by the Explorer API.
     * Currently, this is done via a Streaming List Buckets HTTP call to Riak,
     * and only available in Development Mode.
     * @todo Implement other methods of populating the bucket cache
     *    (for example, a user-supplied text file, or a Search query).
     *
     * @see http://docs.basho.com/riak/latest/dev/references/http/list-buckets/
     *
     * @method bucketCacheRefresh
     * @param {String} clusterId
     * @param {String} bucketTypeId
     * @return Ember.RSVP.Promise
     */
    bucketCacheRefresh: function bucketCacheRefresh(clusterId, bucketTypeId) {
      // For the moment, 'riak_kv' is the only implemented source of
      // cache refresh
      var url = this.apiURL + 'explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/refresh_buckets/source/riak_kv';

      return this.cacheRefresh(url);
    },

    /**
     * Refreshes a key list cache or bucket list cache on the Explorer API side.
     * Usually invoked when the user presses the 'Refresh List' button on the UI.
     * @see bucketCacheRefresh
     * @see keyCacheRefresh
     *
     * @method cacheRefresh
     * @param {String} url
     * @return Ember.RSVP.Promise
     */
    cacheRefresh: function cacheRefresh(url) {
      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        _ember['default'].$.ajax({
          type: "POST",
          url: url
        }).then(function (data, textStatus, jqXHR) {
          resolve(jqXHR.status);
        }, function (jqXHR, textStatus) {
          if (jqXHR.status === 202 && textStatus === 'parsererror') {
            // Server responds with 202 Accepted, and empty body
            resolve(jqXHR.status);
          }
          reject(textStatus);
        });
      });
    },

    /**
     * Returns a Contents hash containing map data type fields sorted by
     * field type (Counters, Flags, Registers, Sets, nested Maps).
     * The sorting is done to make the editing and display UI code easier.
     * @see http://docs.basho.com/riak/latest/dev/using/data-types/
     * @see RiakObjectMap
     *
     * @method collectMapFields
     * @param rootMap {RiakObjectMap} Top-level Map in which these fields will live
     * @param parentMap {RiakObjectMap|RiakObjectEmbeddedMap} Standalone or
     *           nested map containing these fields. When a map is nested just
     *           one level deep, the parentMap is same as rootMap. For fields
     *           nested several levels deep, the parent map will be an embedded
     *           map field.
     * @param payload {Object} Value of the JSON payload of an HTTP GET
     *                   to the map object
     * @return {Object} A hash of fields indexed by CRDT type and field name.
     */
    collectMapFields: function collectMapFields(rootMap, parentMap, payload) {
      var contents = {
        counters: {},
        flags: {},
        registers: {},
        sets: {},
        maps: {}
      };
      var field;
      var self = this;

      for (var fieldName in payload) {
        if (fieldName.endsWith('_counter')) {
          field = self.store.createRecord('riak-object.map-field', {
            fieldType: 'counter',
            name: fieldName,
            rootMap: rootMap,
            parentMap: parentMap,
            value: payload[fieldName]
          });
          contents.counters[fieldName] = field;
        }
        if (fieldName.endsWith('_flag')) {
          field = self.store.createRecord('riak-object.map-field', {
            fieldType: 'flag',
            name: fieldName,
            rootMap: rootMap,
            parentMap: parentMap,
            value: payload[fieldName]
          });
          contents.flags[fieldName] = field;
        }
        if (fieldName.endsWith('_register')) {
          field = self.store.createRecord('riak-object.map-field', {
            fieldType: 'register',
            name: fieldName,
            rootMap: rootMap,
            parentMap: parentMap,
            value: payload[fieldName]
          });
          contents.registers[fieldName] = field;
        }
        if (fieldName.endsWith('_set')) {
          field = self.store.createRecord('riak-object.map-field', {
            fieldType: 'set',
            name: fieldName,
            rootMap: rootMap,
            parentMap: parentMap,
            value: payload[fieldName]
          });
          contents.sets[fieldName] = field;
        }
        if (fieldName.endsWith('_map')) {
          field = self.store.createRecord('riak-object.embedded-map', {
            fieldType: 'map',
            name: fieldName,
            rootMap: rootMap,
            parentMap: parentMap
          });
          // This `field` becomes the `parentMap` for the nested fields.
          // `rootMap` stays the same
          var mapFields = self.collectMapFields(rootMap, field, payload[fieldName]);
          field.value = mapFields;
          contents.maps[fieldName] = field;
        }
      }
      return contents;
    },

    /**
     * Checks availability and validity of nodes in a given cluster.
     *
     * @method checkNodesInCluster
     * @param {DS.Model} Cluster
     */
    checkNodesInCluster: function checkNodesInCluster(cluster) {
      // Ping each node in cluster
      this.pingNodesInCluster(cluster);
      // Get status of each node in cluster
      this.getNodesStatus(cluster);
    },

    /**
     * Creates and returns a BucketList instance, given the results of a
     * 'fetch cached Bucket List' call to the Explorer API.
     * @see ExplorerService.getBucketTypeWithBucketList
     *
     * @method createBucketList
     * @param data {Hash}
     * @param cluster {Cluster}
     * @param bucketType {BucketType}
     * @return {BucketList}
     */
    createBucketList: function createBucketList(data, cluster, bucketType, startItemIndex) {
      var self = this;

      if (!data) {
        // No data, create an empty Bucket list
        return this.store.createRecord('bucket-list', {
          cluster: cluster,
          bucketType: bucketType,
          buckets: []
        });
      }
      // Turn a list of bucket names into a list of actual bucket instances
      var bucketList = data.buckets.buckets.map(function (bucketName) {
        return self.store.createRecord('bucket', {
          name: bucketName,
          cluster: cluster,
          bucketType: bucketType
        });
      });

      return this.store.createRecord('bucket-list', {
        cluster: cluster,
        bucketType: bucketType,
        buckets: bucketList,
        total: data.buckets.total,
        count: data.buckets.count,
        created: data.buckets.created,
        isLoaded: true,
        firstItemIndex: startItemIndex,
        pageSize: this.pageSize
      });
    },

    /**
     * Creates and returns a KeyList instance, given the results of a
     * 'fetch cached Key List' call to the Explorer API.
     * @see ExplorerService.getBucketWithKeyList
     *
     * @method createKeyList
     * @param data {Hash}
     * @param bucket {Bucket}
     * @return {KeyList}
     */
    createKeyList: function createKeyList(data, bucket, startItemIndex) {
      var self = this;

      if (!data) {
        // No data, return an empty KeyList
        return this.store.createRecord('key-list', {
          bucket: bucket,
          cluster: bucket.get('cluster'),
          keys: []
        });
      }
      // The model name depends on the "object type" - plain Object, CRDT, etc
      var modelName = bucket.get('objectModelName');

      // Cycle through the list of keys and create actual RiakObject instances
      var keyList = data.keys.keys.map(function (key) {
        var obj = self.store.createRecord(modelName, {
          key: key,
          bucket: bucket,
          bucketType: bucket.get('bucketType'),
          cluster: bucket.get('cluster'),
          isLoaded: false
        });

        if (self.wasObjectDeleted(obj)) {
          obj.set('markedDeleted', true);
        }

        return obj;
      });

      return this.store.createRecord('key-list', {
        bucket: bucket,
        cluster: bucket.get('cluster'),
        created: data.keys.created,
        count: data.keys.count,
        keys: keyList,
        total: data.keys.total,
        firstItemIndex: startItemIndex,
        pageSize: this.pageSize
      });
    },

    /**
     * Creates a Schema instance if it does not exist,
     *  and the returns instance.
     *
     * @method createSchema
     * @param name {String}
     * @param cluster {Cluster}
     * @return {Schema}
     */
    createSchema: function createSchema(name, cluster) {
      var schema = cluster.get('searchSchemas').findBy('name', name);

      if (!schema) {
        schema = this.store.createRecord('search-schema', {
          id: cluster.get('id') + '/' + name,
          cluster: cluster,
          name: name
        });
      }

      return schema;
    },

    /**
     * Parses and returns the contents/value of a Riak Object, depending on
     * whether it's a CRDT or a plain object.
     *
     * @method createObjectContents
     * @param {Bucket} bucket
     * @param {RiakObject} newObject
     * @param {Object} payload
     * @return {Object}
     */
    createObjectContents: function createObjectContents(bucket, newObject, payload) {
      var contents;
      if (bucket.get('props').get('isMap')) {
        contents = this.collectMapFields(newObject, newObject, payload.value);
      } else {
        contents = payload;
      }
      return contents;
    },

    /**
     * Creates and returns a RiakObject instance from the parsed results
     * of an HTTP Fetch Object ajax call.
     * @see getRiakObject
     *
     * @method createObjectFromAjax
     * @param key {String} Riak object key
     * @param bucket {Bucket}
     * @param rawHeader {String} jQuery AJAX calls return headers as a string :(
     * @param payload {Object}
     * @param url {String} The URL to download the "raw" object payload
     *          (via an Explorer proxy request direct to Riak)
     * @return {RiakObject|RiakObjectCounter|RiakObjectMap|RiakObjectSet}
     */
    createObjectFromAjax: function createObjectFromAjax(key, bucket, rawHeader, payload, url) {
      var metadata = this.createObjectMetadata(rawHeader);
      var modelName = bucket.get('objectModelName');
      var newObject = this.store.createRecord(modelName, {
        key: key,
        bucket: bucket,
        bucketType: bucket.get('bucketType'),
        cluster: bucket.get('cluster'),
        metadata: metadata,
        isLoaded: true,
        rawUrl: url
      });
      var contents = this.createObjectContents(bucket, newObject, payload);

      newObject.set('contents', contents);

      return newObject;
    },

    /**
     * Creates and returns an ObjectMetadata instance by parsing the raw
     * header string returned by AJAX calls, if available.
     *
     * @method createObjectMetadata
     * @param rawHeader {String}
     * @return {ObjectMetadata}
     */
    createObjectMetadata: function createObjectMetadata(rawHeader) {
      if (!rawHeader) {
        return this.store.createRecord('object-metadata');
      }

      return this.store.createRecord('object-metadata', {
        headers: this.parseHeaderString(rawHeader)
      });
    },

    /**
     * Composes the JSON action object for the specified operation type for use
     * with the Riak Data Type HTTP API.
     * Invoked when the user edits and saves a Data Type object.
     * @see http://docs.basho.com/riak/latest/dev/using/data-types/
     *
     * @method dataTypeActionFor
     * @param object {RiakObject|RiakObjectMapField|RiakObjectEmbeddedMap}
     *            Data Type object to be edited
     * @param operationType {String} CRDT operation type
     *            (increment counter, add an element to set, update map)
     * @param item {String|RiakObjectMapField} Set element or map field
     * @return {String} JSON string payload used by Riak's Data Type HTTP API
     * @example Sample return value, for updating a counter:
     *     '{"increment": 1}'
     */
    dataTypeActionFor: function dataTypeActionFor(object, operationType, item) {
      var bucket = object.get('bucket');
      var operation = undefined;
      if (bucket.get('props').get('isCounter')) {
        operation = this.dataTypeUpdateCounter(object, operationType);
      } else if (bucket.get('props').get('isSet')) {
        operation = this.dataTypeUpdateSet(operationType, item);
      } else if (bucket.get('props').get('isMap')) {
        operation = this.dataTypeUpdateNestedField(object, operationType, item);
      }
      if (!operation) {
        throw new _ember['default'].Error('Invalid data type or unsupported operation: ' + operationType);
      }
      return JSON.stringify(operation);
    },

    /**
     * Returns the operation for updating a Counter data type.
     * (Will be converted to a JSON string payload, upstream.)
     *
     * @method dataTypeUpdateCounter
     * @param object {RiakObjectCounter|RiakObjectMapField}
     * @param operationType {String} increment or decrement
     * @return {Object} Update counter operation
     */
    dataTypeUpdateCounter: function dataTypeUpdateCounter(object, operationType) {
      if (operationType === 'increment') {
        return { increment: object.get('incrementBy') };
      } else if (operationType === 'decrement') {
        return { decrement: object.get('decrementBy') };
      }
    },

    /**
     * Wraps field update operations for potentially nested maps.
     *
     * @method dataTypeUpdateMap
     * @param field {RiakObjectMapField|RiakObjectEmbeddedMap}
     * @param subOperation {Object} Accumulator object for nested operations
     * @return {Object} Update map operation (to be converted to JSON and sent)
     */
    dataTypeUpdateMap: function dataTypeUpdateMap(field, subOperation) {
      var parentField = field.get('parentMap');
      if (parentField.get('isTopLevel')) {
        return subOperation;
      } else {
        var operation = { update: {} };
        operation.update[parentField.get('name')] = subOperation;
        return this.dataTypeUpdateMap(parentField, operation);
      }
    },

    /**
     * Returns the operation for updating a Set data type.
     * (Will be converted to a JSON string payload, upstream.)
     *
     * @method dataTypeUpdateNestedField
     * @param object {RiakObjectMap|RiakObjectMapField|RiakObjectEmbeddedMap}
     * @param operationType {String}
     * @param item {String|RiakObjectMapField} Set element or map field
     * @return {Object} Update nested map field operation
     * @example
     *   Remove top-level field, object: Map, item: field
     *   '{ "remove": "<field name to be removed>" }'
     *   Remove field in a nested map, object: EmbeddedMap, item: field
     *   '{
       *      "update": {
       *          "<lvlOne_map>": {
       *              "remove": "<field name to be removed>"
       *          }
       *      }
       *    }'
     *   Remove field two levels deep, object: EmbeddedMap, item: field
     *   '{
       *      "update": {
       *          "<lvlOne_map>": {
       *              "update": {
       *                  "<lvlTwo_map>": {
       *                      "remove": "<field name to be removed>"
       *                  }
       *              }
       *          }
       *      }
       *    }'
     */
    dataTypeUpdateNestedField: function dataTypeUpdateNestedField(object, operationType, item) {
      var fieldName = object.get('name');
      var fieldOperation = {
        update: {}
      };
      switch (operationType) {
        case 'removeField':
          // { "remove": "<field name to be removed>" }
          fieldOperation = { remove: item.get('name') };
          object = item; // In this case, the item is the nested field
          break;
        case 'addField':
        case 'editField':
          // Add a Register, Flag or Counter field, with given value.
          // (Note: editing a register is the same update op as adding one)
          // {
          //     "update": {
          //         "page_visits_counter": 1
          //     }
          // }
          fieldName = item.get('name');
          object = item; // In this case, the item is the nested field
          fieldOperation.update[fieldName] = item.get('value');
          break;
        case 'addElement':
        case 'removeElement':
          // {
          //     "update": {
          //         "interests_set": {
          //            "<add|remove>": "interest123"
          //         }
          //     }
          // }
          fieldOperation.update[fieldName] = this.dataTypeUpdateSet(operationType, item);
          break;
        case 'increment':
        case 'decrement':
          fieldOperation.update[fieldName] = this.dataTypeUpdateCounter(object, operationType);
          break;
        default:
          throw new _ember['default'].Error('Unsupported Update Map operation: ' + operationType);
      }
      // Now wrap the update field operation in appropriate levels of
      // update map operations
      return this.dataTypeUpdateMap(object, fieldOperation);
    },

    /**
     * Returns the operation for updating a Set data type.
     * (Will be converted to a JSON string payload, upstream.)
     *
     * @method dataTypeUpdateSet
     * @param operationType {String}
     * @param element {String}
     * @return {Object} Update set operation
     */
    dataTypeUpdateSet: function dataTypeUpdateSet(operationType, element) {
      if (operationType === 'removeElement') {
        return { remove: element };
      } else if (operationType === 'addElement') {
        return { add: element };
      }
    },

    /**
     * Performs a limited 'Delete Bucket' command via the Explorer API.
     * (This is done as a convenience operation for Devs, since Riak doesn't
     * currently support a whole-bucket delete.)
     * To be more precise, the Explorer backend iterates through all the keys
     * in its Key List cache for that bucket, and issues Delete Object commands
     * for those keys.
     *
     * Limitations:
     * - This is only available in Development Mode
     * - Explorer can only delete objects whose keys are in its cache.
     *
     * This means that the key list cache must already be populated.
     * However, since the 'Delete Bucket' button is only displayed once a
     * non-empty Key List cache is retrieved from the server, this is fine.
     *
     * @method deleteBucket
     * @param {Bucket} bucket
     * @return {Ember.RSVP.Promise} Result of the Delete Bucket AJAX request
     */
    deleteBucket: function deleteBucket(bucket) {
      var cluster = bucket.get('clusterId');
      var bucketType = bucket.get('bucketTypeId');
      var bucketId = bucket.get('bucketId');

      var url = this.apiURL + 'explore/clusters/' + cluster + '/bucket_types/' + bucketType + '/buckets/' + bucketId;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        _ember['default'].$.ajax({
          type: "DELETE",
          url: url,
          success: function success(data, textStatus, jqXHR) {
            resolve(jqXHR.status);
          },
          error: function error(jqXHR, textStatus) {
            if (jqXHR.status === 202 && textStatus === 'parsererror') {
              resolve(jqXHR.status);
            } else {
              reject(textStatus);
            }
          }
        });
      });
    },

    /**
     * Fetch the cache of Buckets and Keys deleted via the Explorer UI.
     * Initialize objects whenever missing.
     * See the `@property deleted` comments above, for explanation.
     *
     * @method deletedCacheFor
     * @param {String} clusterId
     * @param {String} bucketTypeId
     * @return {Hash}
     */
    deletedCacheFor: function deletedCacheFor(clusterId, bucketTypeId) {
      if (!this.deleted.clusters[clusterId]) {
        this.deleted.clusters[clusterId] = { types: {} };
      }
      if (!this.deleted.clusters[clusterId].types[bucketTypeId]) {
        this.deleted.clusters[clusterId].types[bucketTypeId] = { buckets: {} };
      }
      return this.deleted.clusters[clusterId].types[bucketTypeId];
    },

    /**
     * Performs a Delete Object operation, via a proxied Riak HTTP API request.
     * Also records its key in the `ExplorerService.deleted` cache
     *
     * @see http://docs.basho.com/riak/latest/ops/advanced/deletion/
     * @see http://docs.basho.com/riak/latest/dev/references/http/delete-object/
     *
     * @method deleteObject
     * @param object {RiakObject} RiakObject instance or subclasses (Maps, Sets, etc)
     * @return {Ember.RSVP.Promise} Result of the AJAX request.
     */
    deleteObject: function deleteObject(object) {
      var cluster = object.get('clusterId');
      var bucketType = object.get('bucketTypeId');
      var bucket = object.get('bucketId');
      var key = object.get('key');
      var clusterUrl = this.getClusterProxyUrl(cluster);

      var url = clusterUrl + '/types/' + bucketType + '/buckets/' + bucket + '/keys/' + key;

      object.set('markedDeleted', true);

      var request = new _ember['default'].RSVP.Promise(function (resolve, reject) {
        _ember['default'].$.ajax({
          type: "DELETE",
          url: url,
          headers: { 'X-Riak-Vclock': object.get('metadata').get('causalContext') }
        }).then(function (data, textStatus, jqXHR) {
          resolve(jqXHR.status);
        }, function (jqXHR, textStatus) {
          reject(textStatus);
        });
      });

      return request['catch'](function (error) {
        console.log('Error deleting riak object: %O', error);
      });
    },

    /**
     * Creates and returns a Bucket instance by fetching the necessary data:
     * the bucket properties, as well as a Bucket Type instance (which also
     * fetches a Cluster instance).
     * @see BucketProps
     *
     * @method getBucket
     * @param {String} clusterId
     * @param {String} bucketTypeId
     * @param {String} bucketId
     * @return {Bucket}
     */
    getBucket: function getBucket(clusterId, bucketTypeId, bucketId) {
      var self = this;

      return self.getBucketType(clusterId, bucketTypeId).then(function (bucketType) {
        return self.getBucketProps(clusterId, bucketTypeId, bucketId).then(function (bucketProps) {
          return self.store.createRecord('bucket', {
            name: bucketId,
            bucketType: bucketType,
            cluster: bucketType.get('cluster'),
            props: bucketProps
          });
        });
      });
    },

    /**
     * Performs a 'Fetch cached Bucket List' API call to Explorer.
     * If the call encounters a 404 Not Found (meaning, the bucket list cache
     * is empty), it proactively kicks off a Bucket Cache Refresh operation.
     * @see ExplorerService.bucketCacheRefresh
     *
     * @method getBucketList
     * @param {Cluster} cluster
     * @param {BucketType} bucketType
     * @return {Ember.RSVP.Promise<BucketList>} Result of the AJAX request
     */
    getBucketList: function getBucketList(cluster, bucketType) {
      var start = arguments.length <= 2 || arguments[2] === undefined ? 1 : arguments[2];
      var rows = arguments.length <= 3 || arguments[3] === undefined ? this.pageSize : arguments[3];

      var explorer = this;
      var clusterId = cluster.get('clusterId');
      var bucketTypeId = bucketType.get('bucketTypeId');

      var url = this.apiURL + 'explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets?start=' + start + '&rows=' + rows;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var xhrConfig = {
          url: url,
          dataType: 'json',
          type: 'GET',
          success: function success(data) {
            bucketType.set('isBucketListLoaded', true);
            resolve(explorer.createBucketList(data, cluster, bucketType, start));
          },
          error: function error(jqXHR, textStatus) {
            // Fail (likely a 404, cache not yet created)
            if (jqXHR.status === 404) {
              // Return an empty (Loading..) list. Controller will poll to
              // refresh it, later
              var data = null;
              var emptyList = explorer.createBucketList(data, cluster, bucketType);
              if (cluster.get('developmentMode')) {
                bucketType.set('isBucketListLoaded', false);
                emptyList.set('statusMessage', 'Cache not found. Refreshing from a streaming list buckets call...');
                // Kick off a Cache Refresh
                explorer.bucketCacheRefresh(clusterId, bucketTypeId);
              } else {
                bucketType.set('isBucketListLoaded', true);
                // In Production mode, no cache refresh will happen
                emptyList.set('cachePresent', false);
              }
              _ember['default'].run(null, resolve, emptyList);
            } else {
              _ember['default'].run(null, reject, textStatus);
            }
          }
        };

        _ember['default'].$.ajax(xhrConfig);
      });
    },

    /**
     * Performs a proxied 'Fetch Bucket Properties' HTTP API call to Riak.
     * @see http://docs.basho.com/riak/latest/dev/references/http/get-bucket-props/
     *
     * @method getBucketProps
     * @param {String} clusterId
     * @param {String} bucketTypeId
     @ @param {String} bucketId
     * @return {Ember.RSVP.Promise<BucketProps>}
     */
    getBucketProps: function getBucketProps(clusterId, bucketTypeId, bucketId) {
      var self = this;
      var clusterUrl = this.getClusterProxyUrl(clusterId);
      var propsUrl = clusterUrl + '/types/' + bucketTypeId + '/buckets/' + bucketId + '/props';

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var ajaxHash = {
          url: propsUrl,
          dataType: 'json',
          type: 'GET'
        };

        ajaxHash.success = function (data) {
          resolve(self.store.createRecord('bucket-props', data));
        };

        ajaxHash.error = function (jqXHR) {
          _ember['default'].run(null, reject, jqXHR);
        };

        _ember['default'].$.ajax(ajaxHash);
      });
    },

    /**
     * Fetches (via AJAX), creates and returns a Bucket Type instance.
     * (As well as required objects, such as the parent Cluster instance).
     *
     * Implementation note: Initially, the ability to fetch a single bucket type
     * record via the Explorer API wasn't available. This method fetches a
     * Cluster instance with all its bucket types, and selects the needed
     * individual bucket type.
     *
     * @method getBucketType
     * @param {String} clusterId
     * @param {String} bucketTypeId
     * @return {Ember.RSVP.Promise<BucketType>}
     */
    getBucketType: function getBucketType(clusterId, bucketTypeId) {
      var self = this;
      return self.getCluster(clusterId).then(function (cluster) {
        return cluster.get('bucketTypes').findBy('originalId', bucketTypeId);
      });
    },

    /**
     * Fetches, creates and returns a Bucket Type instance as well as the
     * BucketList that goes along with it.
     *
     * @method getBucketTypeWithBucketList
     * @param {BucketType} bucketType
     * @param {Cluster} cluster
     * @return {Ember.RSVP.Promise<BucketType>}
     */
    getBucketTypeWithBucketList: function getBucketTypeWithBucketList(bucketType, cluster, start, row) {
      return this.getBucketList(cluster, bucketType, start, row).then(function (bucketList) {
        bucketType.set('bucketList', bucketList);
        return bucketType;
      });
    },

    /**
     * Returns all the Bucket Types that belong to the specified cluster.
     * This method tries to work with the Ember Data store adapter to take
     * advantage of the identity store and caching.
     * @see ExplorerResourceAdapter for more details
     *
     * @method getBucketTypesForCluster
     * @param {Cluster} cluster
     * @return {Ember.RSVP.Promise<Array<BucketType>>|Array<BucketType>}
     */
    getBucketTypesForCluster: function getBucketTypesForCluster(cluster) {
      if (_ember['default'].isEmpty(cluster.get('bucketTypes'))) {
        // If this page was accessed directly
        //  (via a bookmark and not from a link), bucket types are likely
        //  to be not loaded yet. Load them.
        return this.store.query('bucket-type', { clusterId: cluster.get('clusterId') }).then(function (bucketTypes) {
          cluster.set('bucketTypes', bucketTypes);
          return bucketTypes;
        });
      } else {
        return cluster.get('bucketTypes');
      }
    },

    /**
     * Initializes a given bucket with its Key List (by fetching it via AJAX),
     * and returns that bucket instance.
     *
     * @method getBucketWithKeyList
     * @param {Bucket} bucket
     * @return {Ember.RSVP.Promise<Bucket>}
     */
    getBucketWithKeyList: function getBucketWithKeyList(bucket, start, rows) {
      return this.getKeyList(bucket, start, rows).then(function (keyList) {
        bucket.set('keyList', keyList);
        return bucket;
      });
    },

    /**
     * Fetches a given config file and its dependencies
     *
     * @method getConfigFile
     * @param clusterId
     * @param nodeId
     * @param configId
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getConfigFile: function getConfigFile(clusterId, nodeId, configId) {
      var self = this;

      return this.getNode(clusterId, nodeId).then(function (node) {
        return node.get('configFiles').findBy('fileId', configId);
      }).then(function (configFile) {
        return _ember['default'].RSVP.allSettled([configFile, self.getConfigFileContents(configFile)]);
      }).then(function (PromiseArray) {
        var configFile = PromiseArray[0].value;

        return configFile;
      });
    },

    /**
     * Fetches a given config files contents
     *
     * @method getConfigFileContents
     * @param config
     * @return {Ember.$.Promise} result of the AJAX call
     */
    getConfigFileContents: function getConfigFileContents(config) {
      var clusterId = config.get('node').get('cluster').get('id');
      var nodeId = config.get('node').get('id');
      var configId = config.get('fileId');
      var url = this.apiURL + 'explore/clusters/' + clusterId + '/nodes/' + nodeId + '/config/files/' + configId;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var request = _ember['default'].$.ajax({
          url: url,
          type: 'GET',
          headers: {
            Accept: "plain/text;"
          }
        });

        request.done(function (data) {
          config.set('content', data);

          resolve(config);
        });

        request.fail(function (data) {
          reject(data);
        });
      });
    },

    /**
     * Creates and returns a Cluster object and initializes it with dependent
     * data (including all its Bucket Types and Search Indexes).
     *
     * @method getCluster
     * @param {String} clusterId
     * @return {Ember.RSVP.Promise<Cluster>}
     */
    getCluster: function getCluster(clusterId) {
      var self = this;

      return this.store.findRecord('cluster', clusterId).then(function (cluster) {
        return _ember['default'].RSVP.allSettled([cluster, self.getBucketTypesForCluster(cluster), self.getIndexesForCluster(cluster), self.getNodesForCluster(cluster)]);
      }).then(function (PromiseArray) {
        var cluster = PromiseArray[0].value;

        // Create search-schemas from index references
        //  and set the schema/index association
        cluster.get('searchIndexes').forEach(function (index) {
          var schema = self.createSchema(index.get('schemaRef'), cluster);

          index.set('schema', schema);
        });

        // Check on node health of the cluster
        self.checkNodesInCluster(cluster);

        // Continue to check on node health
        self.pollNodesInCluster(cluster);

        return cluster;
      });
    },

    /**
     * Helper method, returns an explorer Cluster proxy URL
     * (which the Explorer API routes to a random node in the cluster).
     *
     * @method getClusterProxyUrl
     * @param {String} clusterId
     * @return {String} url
     */
    getClusterProxyUrl: function getClusterProxyUrl(clusterId) {
      return this.apiURL + 'riak/clusters/' + clusterId;
    },

    /**
     * Returns a list of Search Indexes that have been created on this cluster.
     * @see http://docs.basho.com/riak/latest/dev/references/http/search-index-info/
     *
     * @method getIndexesForCluster
     * @param {DS.Model} cluster
     * @return {Ember.RSVP.Promise<Array<SearchIndex>>|Array<SearchIndex>}
     */
    getIndexesForCluster: function getIndexesForCluster(cluster) {
      if (_ember['default'].isEmpty(cluster.get('searchIndexes'))) {
        // If this page was accessed directly
        //  (via a bookmark and not from a link), bucket types are likely
        //  to be not loaded yet. Load them.
        return this.store.query('search-index', { clusterId: cluster.get('id') }).then(function (indexes) {
          cluster.set('searchIndexes', indexes);

          return indexes;
        });
      } else {
        return cluster.get('searchIndexes');
      }
    },

    /**
     * Fetches and creates a cached Key List for a given bucket.
     *
     * @method getKeyList
     * @param {Bucket} bucket
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getKeyList: function getKeyList(bucket) {
      var start = arguments.length <= 1 || arguments[1] === undefined ? 1 : arguments[1];
      var rows = arguments.length <= 2 || arguments[2] === undefined ? this.pageSize : arguments[2];

      var clusterId = bucket.get('clusterId');
      var cluster = bucket.get('cluster');
      var bucketTypeId = bucket.get('bucketTypeId');
      var bucketId = bucket.get('bucketId');
      var explorer = this;

      var url = this.apiURL + 'explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets/' + bucketId + '/keys?start=' + start + '&rows=' + rows;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var xhrConfig = {
          url: url,
          dataType: 'json',
          type: 'GET',
          success: function success(data) {
            bucket.set('isKeyListLoaded', true);
            resolve(explorer.createKeyList(data, bucket, start));
          },
          error: function error(jqXHR, textStatus) {
            if (jqXHR.status === 404) {
              var data = null;
              var emptyList = explorer.createKeyList(data, bucket);
              if (cluster.get('developmentMode')) {
                bucket.set('isKeyListLoaded', false);
                emptyList.set('statusMessage', 'Cache not found. Refreshing from a streaming list keys call...');
                // Empty cache (need to kick off a refresh)
                // (only in development mode)
                explorer.keyCacheRefresh(clusterId, bucketTypeId, bucketId);
              } else {
                bucket.set('isKeyListLoaded', true);
                emptyList.set('cachePresent', false);
              }
              // Results in returning an empty (Loading..) key list
              _ember['default'].run(null, resolve, emptyList);
            } else {
              // Some other error
              _ember['default'].run(null, reject, textStatus);
            }
          }
        };

        _ember['default'].$.ajax(xhrConfig);
      });
    },

    /**
     * Fetches a given log file and its dependencies
     *
     * @method getLogFile
     * @param clusterId
     * @param nodeId
     * @param logId
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getLogFile: function getLogFile(clusterId, nodeId, logId) {
      var self = this;

      return this.getNode(clusterId, nodeId).then(function (node) {
        return node.get('logFiles').findBy('fileId', logId);
      }).then(function (logFile) {
        return _ember['default'].RSVP.allSettled([logFile, self.getLogFileContents(logFile), self.getLogFileLength(logFile)]);
      }).then(function (PromiseArray) {
        var logFile = PromiseArray[0].value;

        return logFile;
      });
    },

    /**
     * Fetches a given log files contents
     *
     * @method getLogFileContents
     * @param log
     * @return {Ember.$.Promise} result of the AJAX call
     */
    getLogFileContents: function getLogFileContents(log) {
      var rows = arguments.length <= 1 || arguments[1] === undefined ? this.pageSize : arguments[1];

      var clusterId = log.get('node').get('cluster').get('id');
      var nodeId = log.get('node').get('id');
      var logId = log.get('fileId');
      var url = this.apiURL + 'explore/clusters/' + clusterId + '/nodes/' + nodeId + '/log/files/' + logId + '?rows=' + rows;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var request = _ember['default'].$.ajax({
          url: url,
          type: 'GET',
          headers: {
            Accept: "plain/text;"
          }
        });

        request.done(function (data) {
          log.set('content', data);
          log.set('pageSize', rows);

          resolve(log);
        });

        request.fail(function (data) {
          reject(data);
        });
      });
    },

    /**
     * Fetches and sets the amount of lines in a given log file
     *
     * @method getLogFileLength
     * @param log
     * @return {Ember.$.Promise} result of the AJAX call
     */
    getLogFileLength: function getLogFileLength(log) {
      var clusterId = log.get('node').get('cluster').get('id');
      var nodeId = log.get('node').get('id');
      var logId = log.get('fileId');
      var url = this.apiURL + 'explore/clusters/' + clusterId + '/nodes/' + nodeId + '/log/files/' + logId;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var request = _ember['default'].$.ajax({
          url: url,
          type: 'GET',
          dataType: 'json'
        });

        request.done(function (data) {
          var totalLines = data[logId].total_lines;

          log.set('totalLines', totalLines);

          resolve(log);
        });

        request.fail(function (data) {
          reject(data);
        });
      });
    },

    /**
     * Fetches a given node and all its basic dependencies: stats, configuration, and log files
     *
     * @method getNode
     * @param clusterId
     * @param nodeId
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getNode: function getNode(clusterId, nodeId) {
      var self = this;

      return this.getCluster(clusterId).then(function (cluster) {
        return cluster.get('nodes').findBy('id', nodeId);
      }).then(function (node) {
        return _ember['default'].RSVP.allSettled([node, self.getNodeStats(node), self.getNodeConfig(node), self.getNodeLogFiles(node), self.getNodeConfigFiles(node)]);
      }).then(function (PromiseArray) {
        var node = PromiseArray[0].value;

        return node;
      });
    },

    /**
     * Fetches a given nodes basic configuration stats
     *
     * @method getNodeConfig
     * @param {Node} node
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getNodeConfig: function getNodeConfig(node) {
      var url = this.apiURL + 'explore/nodes/' + node.get('id') + '/config';

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var request = _ember['default'].$.ajax({
          url: url,
          type: 'GET'
        });

        request.done(function (data) {
          if (data.config.advanced_config) {
            node.set('advancedConfig', data.config.advanced_config);
          }

          if (data.config.config) {
            node.set('config', data.config.config);
          }

          resolve(data);
        });

        request.fail(function (data) {
          reject(data);
        });
      });
    },

    /**
     * Fetches and creates a set of config file for a given node.
     *
     * @method getNodeConfigFiles
     * @param {Node} node
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getNodeConfigFiles: function getNodeConfigFiles(node) {
      if (_ember['default'].isEmpty(node.get('configFiles'))) {
        return this.store.query('config-file', { clusterId: node.get('cluster').get('id'), nodeId: node.get('id') }).then(function (configFiles) {
          node.set('configFiles', configFiles);

          return configFiles;
        });
      } else {
        return node.get('configFiles');
      }
    },

    /**
     * Fetches and creates a set of log file for a given node.
     *
     * @method getNodeLogFiles
     * @param {Node} node
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getNodeLogFiles: function getNodeLogFiles(node) {
      if (_ember['default'].isEmpty(node.get('logFiles'))) {
        return this.store.query('log-file', { clusterId: node.get('cluster').get('id'), nodeId: node.get('id') }).then(function (logFiles) {
          node.set('logFiles', logFiles);

          return logFiles;
        });
      } else {
        return node.get('logFiles');
      }
    },

    /**
     * Returns the results of a Riak node HTTP ping result.
     *
     * @method getNodePing
     * @param {String} nodeId
     * @return {Ember.RSVP.Promise} result of the AJAX call
     */
    getNodePing: function getNodePing(nodeId) {
      var url = this.apiURL + 'riak/nodes/' + nodeId + '/ping';

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var request = _ember['default'].$.ajax({
          url: url,
          type: 'GET'
        });

        request.done(function (data) {
          resolve(data);
        });

        request.fail(function (data) {
          reject(data);
        });
      });
    },

    /**
     * Gets and sets the "status" property of each node in a cluster. Status is detrmined by whether or not
     *  the node's ring file is valid.
     *
     * @method getNodesStatus
     * @param {DS.Model} cluster
     */
    getNodesStatus: function getNodesStatus(cluster) {
      var url = this.apiURL + 'control/clusters/' + cluster.get('id') + '/status';

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var request = _ember['default'].$.ajax({
          url: url,
          type: 'GET'
        });

        request.done(function (data) {
          cluster.get('nodes').forEach(function (node) {
            var nodeId = node.get('id');
            var nodeStatus = data.status.nodes.findBy('id', nodeId).status;

            node.set('status', nodeStatus);
          });

          resolve(cluster);
        });

        request.fail(function (data) {
          reject(data);
        });
      });
    },

    /**
     * Returns all reachable nodes for a given cluster id
     *
     * @method getNodesForCluster
     * @param {DS.Model} cluster
     * @return {Ember.RSVP.Promise<Array<RiakNode>>|Array<RiakNode>}
     */
    getNodesForCluster: function getNodesForCluster(cluster) {
      if (_ember['default'].isEmpty(cluster.get('nodes'))) {
        return this.store.query('node', { clusterId: cluster.get('id') }).then(function (nodes) {
          cluster.set('nodes', nodes);

          return nodes;
        });
      } else {
        return cluster.get('nodes');
      }
    },

    /**
     * Gets and sets the node stats property. Returns the node model object.
     *
     * @method getNodeStats
     * @param {DS.Model} Node
     * @return {Ember.RSVP.Promise<Node>}
     */
    getNodeStats: function getNodeStats(node) {
      var url = this.apiURL + 'riak/nodes/' + node.get('id') + '/stats';

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var request = _ember['default'].$.ajax({
          url: url,
          type: 'GET'
        });

        request.done(function (data) {
          node.set('stats', data);

          resolve(node);
        });

        request.fail(function (data) {
          reject(data);
        });
      });
    },

    /**
     * Fetches and returns a Riak Object for the specified location
     * (bucket type, bucket and key).
     * @see http://docs.basho.com/riak/latest/dev/references/http/fetch-object/
     *
     * @method getRiakObject
     * @param {Bucket} bucket
     * @param {String} key
     * @return {Ember.RSVP.Promise<RiakObject>} RiakObject or its subclasses (CRDTs)
     */
    getRiakObject: function getRiakObject(bucket, key) {
      var explorer = this;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var processData;
        var headerString;
        var contents;
        var ajaxHash = {
          type: "GET",
          cache: false,
          headers: { 'Accept': '*/*, multipart/mixed' }
        };
        var clusterUrl = explorer.getClusterProxyUrl(bucket.get('clusterId'));
        var bucketType = bucket.get('bucketTypeId');
        var bucketId = bucket.get('bucketId');
        var url = clusterUrl + '/types/' + bucketType + '/buckets/' + bucketId;

        if (bucket.get('props').get('isCRDT')) {
          url = url + '/datatypes/' + key;
          processData = true; // Parse the payload as JSON
          ajaxHash.dataType = 'json';
          ajaxHash.success = function (data, textStatus, jqXHR) {
            headerString = jqXHR.getAllResponseHeaders();
            contents = data; // Parsed json
            resolve(explorer.createObjectFromAjax(key, bucket, headerString, contents, url));
          };
        } else {
          // Regular Riak object
          url = url + '/keys/' + key;
          processData = false;
          ajaxHash.success = function (data, textStatus, jqXHR) {
            headerString = jqXHR.getAllResponseHeaders();
            contents = jqXHR.responseText; // Unparsed payload
            resolve(explorer.createObjectFromAjax(key, bucket, headerString, contents, url));
          };
        }
        ajaxHash.processData = processData;
        ajaxHash.url = url;

        ajaxHash.error = function (jqXHR, textStatus) {
          if (jqXHR.status === 200 && textStatus === 'parsererror') {
            // jQuery tries to parse JSON objects, and throws
            // parse errors when they're invalid. Suppress this.
            headerString = jqXHR.getAllResponseHeaders();
            resolve(explorer.createObjectFromAjax(key, bucket, headerString, jqXHR.responseText, url));
          }
          if (jqXHR.status === 300) {
            // Handle 300 Multiple Choices case for siblings
            headerString = jqXHR.getAllResponseHeaders();
            resolve(explorer.createObjectFromAjax(key, bucket, headerString, jqXHR.responseText, url));
          } else {
            reject(jqXHR);
          }
        };
        _ember['default'].$.ajax(ajaxHash);
      });
    },

    /**
     * Re-populates the Key List cached by the Explorer API.
     * Currently, this is done via a Streaming List Keys HTTP call to Riak,
     * and only available in Development Mode.
     * @todo Implement other methods of populating the key cache
     *    (for example, a user-supplied text file, or a Search query).
     *
     * @see http://docs.basho.com/riak/latest/dev/references/http/list-keys/
     *
     * @method keyCacheRefresh
     * @param {String} clusterId
     * @param {String} bucketTypeId
     * @param {String} bucketId
     * @return {Ember.RSVP.Promise} Result of the AJAX call
     */
    keyCacheRefresh: function keyCacheRefresh(clusterId, bucketTypeId, bucketId) {
      // For the moment, 'riak_kv' is the only implemented source of
      // cache refresh
      var url = this.apiURL + 'explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets/' + bucketId + '/refresh_keys/source/riak_kv';
      return this.cacheRefresh(url);
    },

    /**
     * Marks a key as deleted in the client-side ExplorerService.deleted cache.
     *
     * @method markDeletedKey
     * @param {RiakObject} object
     */
    markDeletedKey: function markDeletedKey(object) {
      var clusterId = object.get('clusterId');
      var bucketTypeId = object.get('bucketTypeId');
      var bucketId = object.get('bucketId');
      var key = object.get('key');

      var bucketTypeDelCache = this.deletedCacheFor(clusterId, bucketTypeId);

      if (!bucketTypeDelCache.buckets[bucketId]) {
        bucketTypeDelCache.buckets[bucketId] = {
          keysDeleted: {},
          bucketDeleted: false
        };
      }

      bucketTypeDelCache.buckets[bucketId].keysDeleted[key] = true;
    },

    /**
     * Parses the raw AJAX headers string and returns it as a usable hash.
     *
     * XmlHttpRequest's getAllResponseHeaders() method returns a string of response
     * headers according to the format described here:
     * http://www.w3.org/TR/XMLHttpRequest/#the-getallresponseheaders-method
     *
     * Which we then have to parse. Like savages.
     *
     * @method parseHeaderString
     * @param {String} headerString
     * @return {Hash} headers
     */
    parseHeaderString: function parseHeaderString(headerString) {
      var other_headers = {};
      var indexes = [];
      var custom = [];

      var headerLines = headerString.split("\r\n");

      for (var i = 0; i < headerLines.length; i++) {
        var headerLine = headerLines[i];

        // Can't use split() here because it does the wrong thing
        // if the header value has the string ": " in it.
        var index = headerLine.indexOf(': ');
        if (index > 0) {
          var key = headerLine.substring(0, index).toLowerCase();
          var val = headerLine.substring(index + 2);
          var header = {
            key: key,
            value: val
          };

          if (key.startsWith('x-riak-meta')) {
            custom.push(header);
          } else if (key.startsWith('x-riak-index')) {
            indexes.push(header);
          } else {
            other_headers[key] = val;
          }
        }
      }
      return {
        other: other_headers,
        indexes: indexes,
        custom: custom
      };
    },

    /**
     * Pings all nodes in a given cluster and sets the nodes status
     *
     * @method getNodesForCluster
     * @param {DS.Model} cluster
     */
    pingNodesInCluster: function pingNodesInCluster(cluster) {
      var self = this;

      this.getNodesForCluster(cluster).then(function (nodes) {
        nodes.forEach(function (node) {
          var nodeId = node.get('id');

          self.getNodePing(nodeId).then(function onSuccess(data) {
            node.set('available', true);
          }, function onFail(data) {
            node.set('available', false);
          });
        });
      });
    },

    /**
     * Checks node health in a given cluster, every 10 seconds
     *
     * @method pollNodesInCluster
     * @param {DS.Model} Cluster
     */
    pollNodesInCluster: function pollNodesInCluster(cluster) {
      var self = this;

      // This check makes sure that only one cluster can be polled at any given time
      if (!this._clusterRef || cluster.get('id') !== this._clusterRef.get('id')) {
        this._clusterRef = cluster;
      }

      _ember['default'].run.later(this, function () {
        self.checkNodesInCluster(this._clusterRef);
        self.pollNodesInCluster(this._clusterRef);
      }, 10000);
    },

    /**
     * Updates a RiakObject via an HTTP Store Object request to the cluster.
     *
     * @method saveObject
     * @param {RiakObject} object
     * @return {Ember.RSVP.Promise} Result of the AJAX request.
     */
    saveObject: function saveObject(object) {
      var clusterUrl = this.getClusterProxyUrl(object.get('clusterId'));
      var bucketType = object.get('bucketTypeId');
      var bucketId = object.get('bucketId');
      var key = object.get('key');

      var url = clusterUrl + '/types/' + bucketType + '/buckets/' + bucketId + '/keys/' + key;

      var request = new _ember['default'].RSVP.Promise(function (resolve, reject) {
        _ember['default'].$.ajax({
          type: "PUT",
          processData: false,
          contentType: object.get('metadata').get('contentType'),
          url: url,
          headers: object.get('metadata').get('headersForUpdate'),
          data: object.get('contents')
        }).then(function (data, textStatus, jqXHR) {
          resolve(jqXHR.status);
        }, function (jqXHR, textStatus) {
          reject(textStatus);
        });
      });

      return request['catch'](function (error) {
        console.log('Error saving riak object: %O', error);
      });
    },

    /**
     * Performs an update AJAX operation to the Riak Data Type HTTP API endpoint
     *
     * @method updateDataType
     * @param {RiakObjectCounter|RiakObjectSet|RiakObjectMap|RiakObjectMapField} object
     * @param {String} operationType
     * @param {String|RiakObjectMapField} item
     */
    updateDataType: function updateDataType(object, operationType, item) {
      var bucket = object.get('bucket');
      var clusterUrl = this.getClusterProxyUrl(bucket.get('clusterId'));
      var bucketType = bucket.get('bucketTypeId');
      var bucketId = bucket.get('bucketId');
      var key = object.get('key');

      var url = clusterUrl + '/types/' + bucketType + '/buckets/' + bucketId + '/datatypes/' + key;
      var self = this;

      return new _ember['default'].RSVP.Promise(function (resolve, reject) {
        var ajaxHash = {
          contentType: 'application/json',
          type: 'POST',
          dataType: 'json',
          url: url,
          data: self.dataTypeActionFor(object, operationType, item),
          success: function success(data) {
            resolve(data);
          },
          error: function error(jqXHR) {
            if (jqXHR.status === 204) {
              resolve(jqXHR.status);
            } else {
              reject(jqXHR);
            }
          }
        };
        _ember['default'].$.ajax(ajaxHash);
      });
    },

    /**
     * Returns true if a given object was marked as deleted in the client-side
     * ExplorerService.deleted key cache.
     *
     * @method wasObjectDeleted
     * @param {RiakObject} object
     * @return {Boolean}
     */
    wasObjectDeleted: function wasObjectDeleted(object) {
      var clusterId = object.get('clusterId');
      var bucketTypeId = object.get('bucketTypeId');
      var bucketId = object.get('bucketId');
      var key = object.get('key');
      var bucketTypeDelCache = this.deletedCacheFor(clusterId, bucketTypeId);
      if (!bucketTypeDelCache.buckets[bucketId]) {
        return false;
      }
      return bucketTypeDelCache.buckets[bucketId].keysDeleted[key];
    }
  });
});
define("ember-riak-explorer/templates/application", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 5,
              "column": 6
            },
            "end": {
              "line": 15,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/application.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1, "class", "glyphicon glyphicon-th");
          dom.setAttribute(el1, "aria-hidden", "true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          return morphs;
        },
        statements: [["content", "cluster.id", ["loc", [null, [14, 8], [14, 22]]]]],
        locals: ["cluster"],
        templates: []
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 22,
                "column": 14
              },
              "end": {
                "line": 24,
                "column": 14
              }
            },
            "moduleName": "ember-riak-explorer/templates/application.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["content", "currentCluster.riakVersion", ["loc", [null, [23, 20], [23, 50]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 25,
                "column": 14
              },
              "end": {
                "line": 27,
                "column": 14
              }
            },
            "moduleName": "ember-riak-explorer/templates/application.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["content", "currentCluster.riakType", ["loc", [null, [26, 20], [26, 47]]]]],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 28,
                "column": 14
              },
              "end": {
                "line": 30,
                "column": 14
              }
            },
            "moduleName": "ember-riak-explorer/templates/application.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            dom.setAttribute(el1, "class", "dev-mode");
            var el2 = dom.createTextNode("Dev mode");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child3 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 35,
                "column": 12
              },
              "end": {
                "line": 38,
                "column": 12
              }
            },
            "moduleName": "ember-riak-explorer/templates/application.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("              ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "glyphicon glyphicon-hdd");
            dom.setAttribute(el1, "aria-hidden", "true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n              Data\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child4 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 39,
                "column": 12
              },
              "end": {
                "line": 42,
                "column": 12
              }
            },
            "moduleName": "ember-riak-explorer/templates/application.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("              ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "glyphicon glyphicon-dashboard");
            dom.setAttribute(el1, "aria-hidden", "true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n              Ops\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child5 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 43,
                "column": 12
              },
              "end": {
                "line": 46,
                "column": 12
              }
            },
            "moduleName": "ember-riak-explorer/templates/application.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("              ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "glyphicon glyphicon-search");
            dom.setAttribute(el1, "aria-hidden", "true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n              Query\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 17,
              "column": 6
            },
            "end": {
              "line": 49,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/application.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "current-cluster");
          var el2 = dom.createTextNode("\n          ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("header");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3, "class", "title");
          var el4 = dom.createTextNode("Cluster: ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("ul");
          dom.setAttribute(el3, "class", "meta list-inline");
          var el4 = dom.createTextNode("\n");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("            ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("span");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n          ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n          ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("nav");
          dom.setAttribute(el2, "class", "cluster-section-links");
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("          ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(element0, [1]);
          var element2 = dom.childAt(element1, [3]);
          var element3 = dom.childAt(element1, [5]);
          var element4 = dom.childAt(element0, [3]);
          var morphs = new Array(8);
          morphs[0] = dom.createMorphAt(dom.childAt(element1, [1]), 1, 1);
          morphs[1] = dom.createMorphAt(element2, 1, 1);
          morphs[2] = dom.createMorphAt(element2, 2, 2);
          morphs[3] = dom.createMorphAt(element2, 3, 3);
          morphs[4] = dom.createAttrMorph(element3, 'class');
          morphs[5] = dom.createMorphAt(element4, 1, 1);
          morphs[6] = dom.createMorphAt(element4, 2, 2);
          morphs[7] = dom.createMorphAt(element4, 3, 3);
          return morphs;
        },
        statements: [["content", "currentCluster.id", ["loc", [null, [20, 40], [20, 61]]]], ["block", "if", [["get", "currentCluster.hasVersion", ["loc", [null, [22, 20], [22, 45]]]]], [], 0, null, ["loc", [null, [22, 14], [24, 21]]]], ["block", "if", [["get", "currentCluster.hasType", ["loc", [null, [25, 20], [25, 42]]]]], [], 1, null, ["loc", [null, [25, 14], [27, 21]]]], ["block", "if", [["get", "currentCluster.developmentMode", ["loc", [null, [28, 20], [28, 50]]]]], [], 2, null, ["loc", [null, [28, 14], [30, 21]]]], ["attribute", "class", ["concat", ["cluster-status-circle ", ["get", "currentCluster.status", ["loc", [null, [32, 49], [32, 70]]]]]]], ["block", "link-to", ["cluster.data", ["get", "currentCluster.id", ["loc", [null, [35, 38], [35, 55]]]]], ["class", "data"], 3, null, ["loc", [null, [35, 12], [38, 24]]]], ["block", "link-to", ["cluster.ops", ["get", "currentCluster.id", ["loc", [null, [39, 37], [39, 54]]]]], ["class", "ops"], 4, null, ["loc", [null, [39, 12], [42, 24]]]], ["block", "link-to", ["cluster.query", ["get", "currentCluster.id", ["loc", [null, [43, 39], [43, 56]]]]], ["class", "query"], 5, null, ["loc", [null, [43, 12], [46, 24]]]]],
        locals: [],
        templates: [child0, child1, child2, child3, child4, child5]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 58,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/application.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "wrapper-panel");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "sidebar-panel");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "results-panel");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4, "class", "results-panel-inner");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element5 = dom.childAt(fragment, [2, 1]);
        var element6 = dom.childAt(element5, [1]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        morphs[1] = dom.createMorphAt(element6, 1, 1);
        morphs[2] = dom.createMorphAt(element6, 3, 3);
        morphs[3] = dom.createMorphAt(dom.childAt(element5, [3, 1]), 1, 1);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [["inline", "render", ["topbar"], [], ["loc", [null, [1, 0], [1, 19]]]], ["block", "power-select", [], ["class", "cluster-selector", "searchEnabled", false, "selected", ["subexpr", "@mut", [["get", "currentCluster", ["loc", [null, [8, 15], [8, 29]]]]], [], []], "placeholder", "Select Cluster", "options", ["subexpr", "@mut", [["get", "model", ["loc", [null, [10, 14], [10, 19]]]]], [], []], "onchange", ["subexpr", "action", ["clusterSelected"], [], ["loc", [null, [11, 15], [11, 41]]]]], 0, null, ["loc", [null, [5, 6], [15, 23]]]], ["block", "if", [["get", "currentCluster", ["loc", [null, [17, 12], [17, 26]]]]], [], 1, null, ["loc", [null, [17, 6], [49, 13]]]], ["content", "outlet", ["loc", [null, [53, 8], [53, 18]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/components/breadcrumb-component", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 5,
                "column": 4
              },
              "end": {
                "line": 8,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", ["data", "cluster.data", ["get", "clusterId", ["loc", [null, [6, 42], [6, 51]]]]], [], ["loc", [null, [6, 10], [6, 53]]]], ["inline", "link-to", [["get", "bucketTypeId", ["loc", [null, [7, 20], [7, 32]]]], "bucket-type", ["get", "clusterId", ["loc", [null, [7, 47], [7, 56]]]], ["get", "bucketTypeId", ["loc", [null, [7, 57], [7, 69]]]]], [], ["loc", [null, [7, 10], [7, 71]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 9,
                "column": 4
              },
              "end": {
                "line": 11,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", [["get", "bucketId", ["loc", [null, [10, 20], [10, 28]]]], "bucket", ["get", "clusterId", ["loc", [null, [10, 38], [10, 47]]]], ["get", "bucketTypeId", ["loc", [null, [10, 48], [10, 60]]]], ["get", "bucketId", ["loc", [null, [10, 61], [10, 69]]]]], [], ["loc", [null, [10, 10], [10, 71]]]]],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 12,
                "column": 4
              },
              "end": {
                "line": 14,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", [["get", "keyId", ["loc", [null, [13, 20], [13, 25]]]], "riak-object", ["get", "clusterId", ["loc", [null, [13, 40], [13, 49]]]], ["get", "bucketTypeId", ["loc", [null, [13, 50], [13, 62]]]], ["get", "bucketId", ["loc", [null, [13, 63], [13, 71]]]], ["get", "keyId", ["loc", [null, [13, 72], [13, 77]]]]], [], ["loc", [null, [13, 10], [13, 79]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 2
            },
            "end": {
              "line": 15,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          morphs[1] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "bucketTypeId", ["loc", [null, [5, 10], [5, 22]]]]], [], 0, null, ["loc", [null, [5, 4], [8, 11]]]], ["block", "if", [["get", "bucketId", ["loc", [null, [9, 10], [9, 18]]]]], [], 1, null, ["loc", [null, [9, 4], [11, 11]]]], ["block", "if", [["get", "keyId", ["loc", [null, [12, 10], [12, 15]]]]], [], 2, null, ["loc", [null, [12, 4], [14, 11]]]]],
        locals: [],
        templates: [child0, child1, child2]
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 18,
                "column": 4
              },
              "end": {
                "line": 21,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", ["ops", "cluster.ops", ["get", "clusterId", ["loc", [null, [19, 40], [19, 49]]]]], [], ["loc", [null, [19, 10], [19, 51]]]], ["inline", "link-to", [["get", "nodeId", ["loc", [null, [20, 20], [20, 26]]]], "node", ["get", "clusterId", ["loc", [null, [20, 34], [20, 43]]]], ["get", "nodeId", ["loc", [null, [20, 44], [20, 50]]]]], [], ["loc", [null, [20, 10], [20, 52]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 22,
                "column": 4
              },
              "end": {
                "line": 24,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", [["get", "logId", ["loc", [null, [23, 20], [23, 25]]]], "log-file", ["get", "clusterId", ["loc", [null, [23, 37], [23, 46]]]], ["get", "nodeId", ["loc", [null, [23, 47], [23, 53]]]], ["get", "logId", ["loc", [null, [23, 54], [23, 59]]]]], [], ["loc", [null, [23, 10], [23, 61]]]]],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 25,
                "column": 4
              },
              "end": {
                "line": 27,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", [["get", "configId", ["loc", [null, [26, 20], [26, 28]]]], "config-file", ["get", "clusterId", ["loc", [null, [26, 43], [26, 52]]]], ["get", "nodeId", ["loc", [null, [26, 53], [26, 59]]]], ["get", "configId", ["loc", [null, [26, 60], [26, 68]]]]], [], ["loc", [null, [26, 10], [26, 70]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 17,
              "column": 2
            },
            "end": {
              "line": 28,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          morphs[1] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "nodeId", ["loc", [null, [18, 10], [18, 16]]]]], [], 0, null, ["loc", [null, [18, 4], [21, 11]]]], ["block", "if", [["get", "logId", ["loc", [null, [22, 10], [22, 15]]]]], [], 1, null, ["loc", [null, [22, 4], [24, 11]]]], ["block", "if", [["get", "configId", ["loc", [null, [25, 10], [25, 18]]]]], [], 2, null, ["loc", [null, [25, 4], [27, 11]]]]],
        locals: [],
        templates: [child0, child1, child2]
      };
    })();
    var child2 = (function () {
      var child0 = (function () {
        var child0 = (function () {
          return {
            meta: {
              "revision": "Ember@1.13.11",
              "loc": {
                "source": null,
                "start": {
                  "line": 34,
                  "column": 6
                },
                "end": {
                  "line": 36,
                  "column": 6
                }
              },
              "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("        ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("li");
              var el2 = dom.createComment("");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
              return morphs;
            },
            statements: [["inline", "link-to", ["schema edit", "search-schema.edit", ["get", "clusterId", ["loc", [null, [35, 57], [35, 66]]]], ["get", "schemaName", ["loc", [null, [35, 67], [35, 77]]]]], [], ["loc", [null, [35, 12], [35, 79]]]]],
            locals: [],
            templates: []
          };
        })();
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 31,
                "column": 4
              },
              "end": {
                "line": 37,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(3);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3]), 0, 0);
            morphs[2] = dom.createMorphAt(fragment, 5, 5, contextualElement);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [["inline", "link-to", ["query", "cluster.query", ["get", "clusterId", ["loc", [null, [32, 44], [32, 53]]]]], [], ["loc", [null, [32, 10], [32, 55]]]], ["inline", "link-to", [["get", "schemaName", ["loc", [null, [33, 20], [33, 30]]]], "search-schema", ["get", "clusterId", ["loc", [null, [33, 47], [33, 56]]]], ["get", "schemaName", ["loc", [null, [33, 57], [33, 67]]]]], [], ["loc", [null, [33, 10], [33, 69]]]], ["block", "if", [["get", "schemaEdit", ["loc", [null, [34, 12], [34, 22]]]]], [], 0, null, ["loc", [null, [34, 6], [36, 13]]]]],
          locals: [],
          templates: [child0]
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 38,
                "column": 4
              },
              "end": {
                "line": 41,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", ["query overview", "cluster.query", ["get", "clusterId", ["loc", [null, [39, 53], [39, 62]]]]], [], ["loc", [null, [39, 10], [39, 64]]]], ["inline", "link-to", ["schema create", "search-schema.create", ["get", "clusterId", ["loc", [null, [40, 59], [40, 68]]]]], [], ["loc", [null, [40, 10], [40, 70]]]]],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 42,
                "column": 4
              },
              "end": {
                "line": 45,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3]), 0, 0);
            return morphs;
          },
          statements: [["inline", "link-to", ["query overview", "cluster.query", ["get", "clusterId", ["loc", [null, [43, 53], [43, 62]]]]], [], ["loc", [null, [43, 10], [43, 64]]]], ["inline", "link-to", [["get", "indexName", ["loc", [null, [44, 20], [44, 29]]]], "search-index", ["get", "clusterId", ["loc", [null, [44, 45], [44, 54]]]], ["get", "indexName", ["loc", [null, [44, 55], [44, 64]]]]], [], ["loc", [null, [44, 10], [44, 66]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 30,
              "column": 2
            },
            "end": {
              "line": 46,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          morphs[1] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "schemaName", ["loc", [null, [31, 10], [31, 20]]]]], [], 0, null, ["loc", [null, [31, 4], [37, 11]]]], ["block", "if", [["get", "schemaCreate", ["loc", [null, [38, 10], [38, 22]]]]], [], 1, null, ["loc", [null, [38, 4], [41, 11]]]], ["block", "if", [["get", "indexName", ["loc", [null, [42, 10], [42, 19]]]]], [], 2, null, ["loc", [null, [42, 4], [45, 11]]]]],
        locals: [],
        templates: [child0, child1, child2]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 48,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/breadcrumb-component.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("ol");
        dom.setAttribute(el1, "class", "breadcrumb");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 2, 2);
        morphs[1] = dom.createMorphAt(element0, 4, 4);
        morphs[2] = dom.createMorphAt(element0, 6, 6);
        return morphs;
      },
      statements: [["block", "if", [["get", "isClusterData", ["loc", [null, [4, 8], [4, 21]]]]], [], 0, null, ["loc", [null, [4, 2], [15, 9]]]], ["block", "if", [["get", "isClusterOps", ["loc", [null, [17, 8], [17, 20]]]]], [], 1, null, ["loc", [null, [17, 2], [28, 9]]]], ["block", "if", [["get", "isClusterQuery", ["loc", [null, [30, 8], [30, 22]]]]], [], 2, null, ["loc", [null, [30, 2], [46, 9]]]]],
      locals: [],
      templates: [child0, child1, child2]
    };
  })());
});
define("ember-riak-explorer/templates/components/bucket-properties-list", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 9,
              "column": 2
            },
            "end": {
              "line": 14,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties-list.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 0, 0);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 0, 0);
          return morphs;
        },
        statements: [["content", "prop.key", ["loc", [null, [11, 10], [11, 22]]]], ["content", "prop.value", ["loc", [null, [12, 10], [12, 24]]]]],
        locals: ["prop"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 17,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/bucket-properties-list.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1, "class", "table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Property");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Value");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n  ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 3]), 1, 1);
        return morphs;
      },
      statements: [["block", "each", [["get", "model.props.propsList", ["loc", [null, [9, 10], [9, 31]]]]], [], 0, null, ["loc", [null, [9, 2], [14, 11]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/bucket-properties", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 20,
              "column": 6
            },
            "end": {
              "line": 29,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("br");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n        R: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(", W: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(",\n        PR: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(", PW: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(",\n        DW: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("small");
          var el2 = dom.createTextNode("\n          (basic_quorum: ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode(",\n          notfound_ok: ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode(")\n        ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [13]);
          var morphs = new Array(7);
          morphs[0] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          morphs[1] = dom.createMorphAt(fragment, 5, 5, contextualElement);
          morphs[2] = dom.createMorphAt(fragment, 7, 7, contextualElement);
          morphs[3] = dom.createMorphAt(fragment, 9, 9, contextualElement);
          morphs[4] = dom.createMorphAt(fragment, 11, 11, contextualElement);
          morphs[5] = dom.createMorphAt(element0, 1, 1);
          morphs[6] = dom.createMorphAt(element0, 3, 3);
          return morphs;
        },
        statements: [["content", "model.props.quorum.r", ["loc", [null, [22, 11], [22, 35]]]], ["content", "model.props.quorum.w", ["loc", [null, [22, 40], [22, 64]]]], ["content", "model.props.quorum.pr", ["loc", [null, [23, 12], [23, 37]]]], ["content", "model.props.quorum.pw", ["loc", [null, [23, 43], [23, 68]]]], ["content", "model.props.quorum.dw", ["loc", [null, [24, 12], [24, 37]]]], ["content", "model.props.quorum.basic_quorum", ["loc", [null, [26, 25], [26, 60]]]], ["content", "model.props.quorum.basic_quorum", ["loc", [null, [27, 23], [27, 58]]]]],
        locals: [],
        templates: []
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 36,
                "column": 8
              },
              "end": {
                "line": 38,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "model.index.name", ["loc", [null, [37, 10], [37, 30]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 35,
              "column": 6
            },
            "end": {
              "line": 39,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "link-to", ["search-index", ["get", "model.index.cluster.clusterId", ["loc", [null, [36, 34], [36, 63]]]], ["get", "model.index.name", ["loc", [null, [36, 64], [36, 80]]]]], [], 0, null, ["loc", [null, [36, 8], [38, 20]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    var child2 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 39,
              "column": 6
            },
            "end": {
              "line": 41,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        n/a (not being indexed)\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    var child3 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 49,
                "column": 10
              },
              "end": {
                "line": 51,
                "column": 10
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("            ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["content", "warning", ["loc", [null, [50, 16], [50, 27]]]]],
          locals: ["warning"],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 44,
              "column": 2
            },
            "end": {
              "line": 55,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          dom.setAttribute(el2, "class", "key");
          var el3 = dom.createTextNode("Warnings:");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          dom.setAttribute(el2, "class", "value");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("ul");
          var el4 = dom.createTextNode("\n");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1]), 1, 1);
          return morphs;
        },
        statements: [["block", "each", [["get", "model.props.warnings", ["loc", [null, [49, 18], [49, 38]]]]], [], 0, null, ["loc", [null, [49, 10], [51, 19]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 57,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1, "class", "key-value-table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tr");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "key");
        var el4 = dom.createTextNode("Bucket Name:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "value");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tr");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "key");
        var el4 = dom.createTextNode("Object type:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "value");
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tr");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "key");
        var el4 = dom.createTextNode("Conflict Res. Strategy:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "value");
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tr");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "key");
        var el4 = dom.createTextNode("Quorum:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "value");
        var el4 = dom.createTextNode("\n      N_Val: ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tr");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "key");
        var el4 = dom.createTextNode("Search Index:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3, "class", "value");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element1 = dom.childAt(fragment, [0]);
        var element2 = dom.childAt(element1, [7, 3]);
        var morphs = new Array(7);
        morphs[0] = dom.createMorphAt(dom.childAt(element1, [1, 3]), 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [3, 3]), 0, 0);
        morphs[2] = dom.createMorphAt(dom.childAt(element1, [5, 3]), 0, 0);
        morphs[3] = dom.createMorphAt(element2, 1, 1);
        morphs[4] = dom.createMorphAt(element2, 3, 3);
        morphs[5] = dom.createMorphAt(dom.childAt(element1, [9, 3]), 1, 1);
        morphs[6] = dom.createMorphAt(element1, 11, 11);
        return morphs;
      },
      statements: [["content", "model.name", ["loc", [null, [5, 6], [5, 20]]]], ["content", "model.props.objectType", ["loc", [null, [10, 22], [10, 48]]]], ["content", "model.props.resolutionStrategy", ["loc", [null, [14, 22], [14, 56]]]], ["content", "model.props.nVal", ["loc", [null, [19, 13], [19, 33]]]], ["block", "if", [["get", "model.props.quorumRelevant", ["loc", [null, [20, 12], [20, 38]]]]], [], 0, null, ["loc", [null, [20, 6], [29, 13]]]], ["block", "if", [["get", "model.index", ["loc", [null, [35, 12], [35, 23]]]]], [], 1, 2, ["loc", [null, [35, 6], [41, 13]]]], ["block", "if", [["get", "model.props.warnings", ["loc", [null, [44, 8], [44, 28]]]]], [], 3, null, ["loc", [null, [44, 2], [55, 9]]]]],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  })());
});
define("ember-riak-explorer/templates/components/bucket-types", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 2
            },
            "end": {
              "line": 26,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-types.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var morphs = new Array(4);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 1, 1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 1, 1);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5]), 1, 1);
          morphs[3] = dom.createMorphAt(dom.childAt(element0, [7]), 1, 1);
          return morphs;
        },
        statements: [["inline", "link.bucket-type", [], ["bucketType", ["subexpr", "@mut", [["get", "bt", ["loc", [null, [14, 38], [14, 40]]]]], [], []], "btnBlock", true], ["loc", [null, [14, 8], [14, 56]]]], ["content", "bt.props.objectType", ["loc", [null, [17, 8], [17, 31]]]], ["content", "bt.props.nVal", ["loc", [null, [20, 8], [20, 25]]]], ["content", "bt.props.resolutionStrategy", ["loc", [null, [23, 8], [23, 39]]]]],
        locals: ["bt"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 29,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/bucket-types.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1, "class", "table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Name");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Object Type");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("n_val");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Conflict Res. Strategy");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n  ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 3]), 1, 1);
        return morphs;
      },
      statements: [["block", "each", [["get", "bucketTypes", ["loc", [null, [11, 10], [11, 21]]]]], [], 0, null, ["loc", [null, [11, 2], [26, 11]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/button/delete-object", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/button/delete-object.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1, "type", "button");
        dom.setAttribute(el1, "class", "btn btn-sm btn-danger");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2, "class", "glyphicon glyphicon-trash");
        dom.setAttribute(el2, "aria-hidden", "true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  Delete\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(1);
        morphs[0] = dom.createElementMorph(element0);
        return morphs;
      },
      statements: [["element", "action", ["deleteObject", ["get", "object", ["loc", [null, [2, 26], [2, 32]]]]], [], ["loc", [null, [2, 2], [2, 34]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/button/edit-object", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 4,
              "column": 13
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/button/edit-object.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1, "class", "glyphicon glyphicon-pencil");
          dom.setAttribute(el1, "aria-hidden", "true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  Edit Object");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/button/edit-object.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [["block", "link-to", ["riak-object.edit", ["get", "object", ["loc", [null, [1, 30], [1, 36]]]]], ["classNames", "btn btn-sm btn-primary"], 0, null, ["loc", [null, [1, 0], [4, 25]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/button/object-view-raw", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/button/object-view-raw.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("a");
        dom.setAttribute(el1, "class", "btn btn-sm btn-primary");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2, "class", "glyphicon glyphicon-download");
        dom.setAttribute(el2, "aria-hidden", "true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  View raw\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(1);
        morphs[0] = dom.createAttrMorph(element0, 'href');
        return morphs;
      },
      statements: [["attribute", "href", ["concat", [["get", "object.rawUrl", ["loc", [null, [1, 11], [1, 24]]]]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/button/refresh-buckets", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/button/refresh-buckets.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1, "type", "button");
        dom.setAttribute(el1, "class", "btn btn-xs btn-primary");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2, "class", "glyphicon glyphicon-refresh");
        dom.setAttribute(el2, "aria-hidden", "true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  Refresh Bucket Cache\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(1);
        morphs[0] = dom.createElementMorph(element0);
        return morphs;
      },
      statements: [["element", "action", ["refreshBuckets", ["get", "bucketType", ["loc", [null, [1, 79], [1, 89]]]]], [], ["loc", [null, [1, 53], [1, 91]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/button/refresh-keys", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/button/refresh-keys.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1, "type", "button");
        dom.setAttribute(el1, "class", "btn btn-xs btn-primary");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2, "class", "glyphicon glyphicon-refresh");
        dom.setAttribute(el2, "aria-hidden", "true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  Refresh Key Cache\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(1);
        morphs[0] = dom.createElementMorph(element0);
        return morphs;
      },
      statements: [["element", "action", ["refreshKeys", ["get", "bucket", ["loc", [null, [2, 25], [2, 31]]]]], [], ["loc", [null, [2, 2], [2, 33]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/button/set-element-remove", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/button/set-element-remove.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1, "type", "button");
        dom.setAttribute(el1, "class", "btn btn-xs btn-danger");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2, "class", "glyphicon glyphicon-trash");
        dom.setAttribute(el2, "aria-hidden", "true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(1);
        morphs[0] = dom.createElementMorph(element0);
        return morphs;
      },
      statements: [["element", "action", ["removeElement", ["get", "model", ["loc", [null, [2, 27], [2, 32]]]], ["get", "item", ["loc", [null, [2, 33], [2, 37]]]]], [], ["loc", [null, [2, 2], [2, 39]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/code-highlighter", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/code-highlighter.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("code");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createAttrMorph(element0, 'class');
        morphs[1] = dom.createMorphAt(element0, 1, 1);
        return morphs;
      },
      statements: [["attribute", "class", ["get", "language-type", ["loc", [null, [1, 14], [1, 27]]]]], ["content", "yield", ["loc", [null, [2, 2], [2, 11]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/dashboard-module", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 2,
              "column": 2
            },
            "end": {
              "line": 6,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/dashboard-module.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "module-label");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 1, 1);
          return morphs;
        },
        statements: [["content", "label", ["loc", [null, [4, 6], [4, 15]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 11,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/dashboard-module.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "module-content");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createAttrMorph(element0, 'class');
        morphs[1] = dom.createMorphAt(element0, 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [3]), 1, 1);
        return morphs;
      },
      statements: [["attribute", "class", ["concat", ["dashboard-module ", ["get", "class", ["loc", [null, [1, 31], [1, 36]]]]]]], ["block", "if", [["get", "label", ["loc", [null, [2, 8], [2, 13]]]]], [], 0, null, ["loc", [null, [2, 2], [6, 9]]]], ["content", "yield", ["loc", [null, [8, 4], [8, 13]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/link/bucket-type", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 4,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link/bucket-type.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1, "class", "glyphicon glyphicon-inbox cluster-resource-icon");
          dom.setAttribute(el1, "aria-hidden", "true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          return morphs;
        },
        statements: [["content", "bucketType.bucketTypeId", ["loc", [null, [3, 2], [3, 29]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/link/bucket-type.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "link-to", ["bucket-type", ["get", "bucketType", ["loc", [null, [1, 25], [1, 35]]]]], ["class", "cluster-resource-link"], 0, null, ["loc", [null, [1, 0], [4, 12]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/link/link-bucket", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link/link-bucket.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["content", "bucket.bucketId", ["loc", [null, [2, 2], [2, 21]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 4,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/link/link-bucket.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "link-to", ["bucket", ["get", "bucket", ["loc", [null, [1, 20], [1, 26]]]]], [], 0, null, ["loc", [null, [1, 0], [3, 12]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/link/link-cluster", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 5,
                "column": 4
              },
              "end": {
                "line": 5,
                "column": 40
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/link/link-cluster.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("(Dev)");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 7,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link/link-cluster.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("img");
          dom.setAttribute(el1, "src", "assets/images/riak.png");
          dom.setAttribute(el1, "class", "cluster-resource-icon");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [3]);
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(element0, 0, 0);
          morphs[1] = dom.createMorphAt(element0, 2, 2);
          return morphs;
        },
        statements: [["content", "cluster.id", ["loc", [null, [4, 8], [4, 22]]]], ["block", "if", [["get", "cluster.developmentMode", ["loc", [null, [5, 10], [5, 33]]]]], [], 0, null, ["loc", [null, [5, 4], [5, 47]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 8,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/link/link-cluster.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "link-to", ["cluster", ["get", "cluster", ["loc", [null, [1, 21], [1, 28]]]]], ["classNames", "btn btn-sm btn-primary cluster-resource-link"], 0, null, ["loc", [null, [1, 0], [7, 12]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/link/link-index", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link/link-index.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["content", "searchIndex.name", ["loc", [null, [2, 2], [2, 22]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 4,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/link/link-index.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "link-to", ["search-index", ["get", "searchIndex.cluster.clusterId", ["loc", [null, [1, 26], [1, 55]]]], ["get", "searchIndex.name", ["loc", [null, [1, 56], [1, 72]]]]], ["class", "cluster-resource-link"], 0, null, ["loc", [null, [1, 0], [3, 12]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/link/link-object", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link/link-object.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("strike");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
          return morphs;
        },
        statements: [["content", "obj.key", ["loc", [null, [2, 10], [2, 21]]]]],
        locals: [],
        templates: []
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 4,
                "column": 2
              },
              "end": {
                "line": 4,
                "column": 56
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/link/link-object.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [["content", "obj.key", ["loc", [null, [4, 45], [4, 56]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 3,
              "column": 0
            },
            "end": {
              "line": 5,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link/link-object.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["block", "link-to", [["get", "obj.bucket.objectModelName", ["loc", [null, [4, 13], [4, 39]]]], ["get", "obj", ["loc", [null, [4, 40], [4, 43]]]]], [], 0, null, ["loc", [null, [4, 2], [4, 68]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/link/link-object.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "if", [["get", "obj.markedDeleted", ["loc", [null, [1, 6], [1, 23]]]]], [], 0, 1, ["loc", [null, [1, 0], [5, 7]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/components/loading-spinner", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/loading-spinner.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "text-center");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
          return morphs;
        },
        statements: [["content", "statusMessage", ["loc", [null, [2, 27], [2, 44]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 9,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/loading-spinner.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "spinner");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "double-bounce1");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "double-bounce2");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [["block", "if", [["get", "statusMessage", ["loc", [null, [1, 6], [1, 19]]]]], [], 0, null, ["loc", [null, [1, 0], [3, 7]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/nav-cluster-link", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 3,
                "column": 2
              },
              "end": {
                "line": 3,
                "column": 60
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/nav-cluster-link.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createElement("strong");
            var el2 = dom.createTextNode("(Dev mode)");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 4,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/nav-cluster-link.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          morphs[1] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          return morphs;
        },
        statements: [["content", "cluster.id", ["loc", [null, [2, 14], [2, 28]]]], ["block", "if", [["get", "cluster.developmentMode", ["loc", [null, [3, 8], [3, 31]]]]], [], 0, null, ["loc", [null, [3, 2], [3, 67]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/nav-cluster-link.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "link-to", ["cluster", ["get", "cluster", ["loc", [null, [1, 21], [1, 28]]]]], [], 0, null, ["loc", [null, [1, 0], [4, 12]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-actions", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 3,
                "column": 2
              },
              "end": {
                "line": 5,
                "column": 10
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    Cancel");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 6,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  (Editing)\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["block", "link-to", ["riak-object", ["get", "model", ["loc", [null, [3, 27], [3, 32]]]]], ["classNames", "btn btn-xs btn-default"], 0, null, ["loc", [null, [3, 2], [5, 22]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 7,
                "column": 2
              },
              "end": {
                "line": 9,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "button.object-view-raw", [], ["object", ["subexpr", "@mut", [["get", "model", ["loc", [null, [8, 36], [8, 41]]]]], [], []]], ["loc", [null, [8, 4], [8, 43]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 10,
                "column": 2
              },
              "end": {
                "line": 12,
                "column": 2
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["inline", "button.edit-object", [], ["object", ["subexpr", "@mut", [["get", "model", ["loc", [null, [11, 32], [11, 37]]]]], [], []]], ["loc", [null, [11, 4], [11, 39]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 6,
              "column": 0
            },
            "end": {
              "line": 15,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          morphs[1] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          morphs[2] = dom.createMorphAt(fragment, 3, 3, contextualElement);
          dom.insertBoundary(fragment, 0);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.canBeViewedRaw", ["loc", [null, [7, 8], [7, 28]]]]], [], 0, null, ["loc", [null, [7, 2], [9, 9]]]], ["block", "if", [["get", "model.canBeEdited", ["loc", [null, [10, 8], [10, 25]]]]], [], 1, null, ["loc", [null, [10, 2], [12, 9]]]], ["inline", "button.delete-object", [], ["action", "deleteObject", "object", ["subexpr", "@mut", [["get", "model", ["loc", [null, [14, 9], [14, 14]]]]], [], []]], ["loc", [null, [13, 2], [14, 16]]]]],
        locals: [],
        templates: [child0, child1]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 16,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "if", [["get", "isEditing", ["loc", [null, [1, 6], [1, 15]]]]], [], 0, 1, ["loc", [null, [1, 0], [15, 7]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-counter", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 29,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-counter.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1, "class", "key-value-table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4, "class", "key");
        var el5 = dom.createTextNode("Current Value:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4, "class", "value counter-value");
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n  ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4, "class", "key");
        var el5 = dom.createTextNode("\n      Change Value:\n    ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4, "class", "value");
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("form");
        dom.setAttribute(el5, "class", "form-inline");
        var el6 = dom.createTextNode("\n        ");
        dom.appendChild(el5, el6);
        var el6 = dom.createElement("button");
        dom.setAttribute(el6, "type", "button");
        dom.setAttribute(el6, "class", "btn btn-primary");
        var el7 = dom.createTextNode("\n          ");
        dom.appendChild(el6, el7);
        var el7 = dom.createElement("span");
        dom.setAttribute(el7, "class", "glyphicon glyphicon-minus");
        dom.setAttribute(el7, "aria-hidden", "true");
        dom.appendChild(el6, el7);
        var el7 = dom.createTextNode("\n        ");
        dom.appendChild(el6, el7);
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode("\n        ");
        dom.appendChild(el5, el6);
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode("\n        ");
        dom.appendChild(el5, el6);
        var el6 = dom.createElement("button");
        dom.setAttribute(el6, "type", "button");
        dom.setAttribute(el6, "class", "btn btn-primary");
        var el7 = dom.createTextNode("\n          ");
        dom.appendChild(el6, el7);
        var el7 = dom.createElement("span");
        dom.setAttribute(el7, "class", "glyphicon glyphicon-plus");
        dom.setAttribute(el7, "aria-hidden", "true");
        dom.appendChild(el6, el7);
        var el7 = dom.createTextNode("\n        ");
        dom.appendChild(el6, el7);
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode("\n      ");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n    ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n  ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4, "class", "key");
        var el5 = dom.createTextNode("Delete Object");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4, "class", "value");
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n  ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0, 1]);
        var element1 = dom.childAt(element0, [3, 3, 1]);
        var element2 = dom.childAt(element1, [1]);
        var element3 = dom.childAt(element1, [5]);
        var morphs = new Array(5);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]), 0, 0);
        morphs[1] = dom.createElementMorph(element2);
        morphs[2] = dom.createMorphAt(element1, 3, 3);
        morphs[3] = dom.createElementMorph(element3);
        morphs[4] = dom.createMorphAt(dom.childAt(element0, [5, 3]), 0, 0);
        return morphs;
      },
      statements: [["content", "model.contentsForDisplay", ["loc", [null, [5, 36], [5, 64]]]], ["element", "action", ["decrementCounter", ["get", "model", ["loc", [null, [13, 82], [13, 87]]]]], [], ["loc", [null, [13, 54], [13, 89]]]], ["inline", "input", [], ["value", ["subexpr", "@mut", [["get", "model.valueChanger", ["loc", [null, [16, 22], [16, 40]]]]], [], []], "id", "valueChanger", "class", "form-control value-changer"], ["loc", [null, [16, 8], [16, 95]]]], ["element", "action", ["incrementCounter", ["get", "model", ["loc", [null, [17, 82], [17, 87]]]]], [], ["loc", [null, [17, 54], [17, 89]]]], ["inline", "object-actions", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [25, 45], [25, 50]]]]], [], []], "deleteObject", "deleteObject"], ["loc", [null, [25, 22], [25, 80]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-counters-embedded", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 2
            },
            "end": {
              "line": 21,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-counters-embedded.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row set-element-row");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-1");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3, "type", "button");
          dom.setAttribute(el3, "class", "btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "class", "glyphicon glyphicon-trash");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-3 field-label");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-8");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("code");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(element0, [1, 1]);
          var morphs = new Array(3);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]), 0, 0);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 1]), 1, 1);
          return morphs;
        },
        statements: [["element", "action", ["removeField", ["get", "model", ["loc", [null, [8, 33], [8, 38]]]], ["get", "counter", ["loc", [null, [8, 39], [8, 46]]]]], [], ["loc", [null, [8, 10], [8, 48]]]], ["content", "counter.name", ["loc", [null, [13, 16], [13, 32]]]], ["content", "counter.valueForDisplay", ["loc", [null, [17, 10], [17, 37]]]]],
        locals: ["counter"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 23,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-counters-embedded.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container set-contents-list");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h5");
        var el3 = dom.createTextNode("Counters (");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(")");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element2, [1]), 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        return morphs;
      },
      statements: [["content", "model.countersList.length", ["loc", [null, [2, 16], [2, 45]]]], ["block", "each", [["get", "model.countersList", ["loc", [null, [4, 10], [4, 28]]]]], [], 0, null, ["loc", [null, [4, 2], [21, 11]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-flags", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 2
            },
            "end": {
              "line": 21,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-flags.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row set-element-row");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-1");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3, "type", "button");
          dom.setAttribute(el3, "class", "btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "class", "glyphicon glyphicon-trash");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-3 field-label");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-8");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("code");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(element0, [1, 1]);
          var morphs = new Array(3);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]), 0, 0);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 1]), 1, 1);
          return morphs;
        },
        statements: [["element", "action", ["removeField", ["get", "model", ["loc", [null, [8, 33], [8, 38]]]], ["get", "flag", ["loc", [null, [8, 39], [8, 43]]]]], [], ["loc", [null, [8, 10], [8, 45]]]], ["content", "flag.name", ["loc", [null, [13, 16], [13, 29]]]], ["content", "flag.value", ["loc", [null, [17, 10], [17, 24]]]]],
        locals: ["flag"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 23,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-flags.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container set-contents-list");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h5");
        var el3 = dom.createTextNode("Flags (");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(")");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element2, [1]), 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        return morphs;
      },
      statements: [["content", "model.flagsList.length", ["loc", [null, [2, 13], [2, 39]]]], ["block", "each", [["get", "model.flagsList", ["loc", [null, [4, 10], [4, 25]]]]], [], 0, null, ["loc", [null, [4, 2], [21, 11]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-map", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 45,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-map.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "riak-object");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "object-contents");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-header");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4, "class", "object-contents-actions");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-body");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4, "class", "form-inline");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4, "class", "form-inline");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4, "class", "form-inline");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4, "class", "form-inline");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4, "class", "form-inline");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-footer");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Page loaded:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode(" ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0, 1]);
        var element1 = dom.childAt(element0, [3]);
        var morphs = new Array(7);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 1]), 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [1]), 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(element1, [3]), 1, 1);
        morphs[3] = dom.createMorphAt(dom.childAt(element1, [5]), 1, 1);
        morphs[4] = dom.createMorphAt(dom.childAt(element1, [7]), 1, 1);
        morphs[5] = dom.createMorphAt(dom.childAt(element1, [9]), 1, 1);
        morphs[6] = dom.createMorphAt(dom.childAt(element0, [5]), 3, 3);
        return morphs;
      },
      statements: [["inline", "object-actions", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [5, 31], [5, 36]]]]], [], []], "deleteObject", "deleteObject"], ["loc", [null, [5, 8], [5, 66]]]], ["inline", "object-contents-registers", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [10, 42], [10, 47]]]]], [], []], "addField", "addField", "editField", "editField", "removeField", "removeField"], ["loc", [null, [10, 8], [13, 35]]]], ["inline", "object-contents-flags", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [17, 38], [17, 43]]]]], [], []], "removeField", "removeField"], ["loc", [null, [17, 8], [18, 35]]]], ["inline", "object-contents-counters-embedded", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [22, 50], [22, 55]]]]], [], []], "removeField", "removeField"], ["loc", [null, [22, 8], [23, 35]]]], ["inline", "object-contents-sets-embedded", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [27, 46], [27, 51]]]]], [], []], "addElement", "addElement", "removeElement", "removeElement"], ["loc", [null, [27, 8], [29, 39]]]], ["inline", "object-contents-maps-embedded", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [33, 46], [33, 51]]]]], [], []], "removeField", "removeField", "addField", "addField", "editField", "editField", "addElement", "addElement", "removeElement", "removeElement"], ["loc", [null, [33, 8], [36, 10]]]], ["content", "model.metadata.dateLoaded", ["loc", [null, [40, 36], [40, 65]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-maps-embedded", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 2
            },
            "end": {
              "line": 49,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-maps-embedded.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row map-composite-field-name");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-12");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3, "type", "button");
          dom.setAttribute(el3, "class", "btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n          remove map\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n          \n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-12");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3, "class", "form-inline");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3, "class", "form-inline");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3, "class", "form-inline");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3, "class", "form-inline");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3, "class", "form-inline");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [1]);
          var element2 = dom.childAt(fragment, [3, 1]);
          var morphs = new Array(7);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 0, 0);
          morphs[2] = dom.createMorphAt(dom.childAt(element2, [1]), 1, 1);
          morphs[3] = dom.createMorphAt(dom.childAt(element2, [3]), 1, 1);
          morphs[4] = dom.createMorphAt(dom.childAt(element2, [5]), 1, 1);
          morphs[5] = dom.createMorphAt(dom.childAt(element2, [7]), 1, 1);
          morphs[6] = dom.createMorphAt(dom.childAt(element2, [9]), 1, 1);
          return morphs;
        },
        statements: [["element", "action", ["removeField", ["get", "model", ["loc", [null, [8, 33], [8, 38]]]], ["get", "map", ["loc", [null, [8, 39], [8, 42]]]]], [], ["loc", [null, [8, 10], [8, 44]]]], ["content", "map.name", ["loc", [null, [12, 16], [12, 28]]]], ["inline", "object-contents-registers", [], ["model", ["subexpr", "@mut", [["get", "map", ["loc", [null, [18, 44], [18, 47]]]]], [], []], "addField", "addField", "editField", "editField", "removeField", "removeField"], ["loc", [null, [18, 10], [21, 37]]]], ["inline", "object-contents-flags", [], ["model", ["subexpr", "@mut", [["get", "map", ["loc", [null, [25, 40], [25, 43]]]]], [], []], "removeField", "removeField"], ["loc", [null, [25, 10], [26, 37]]]], ["inline", "object-contents-counters-embedded", [], ["model", ["subexpr", "@mut", [["get", "map", ["loc", [null, [30, 52], [30, 55]]]]], [], []], "removeField", "removeField"], ["loc", [null, [30, 10], [31, 37]]]], ["inline", "object-contents-sets-embedded", [], ["model", ["subexpr", "@mut", [["get", "map", ["loc", [null, [35, 48], [35, 51]]]]], [], []], "removeField", "removeField", "addElement", "addElement", "removeElement", "removeElement"], ["loc", [null, [35, 10], [38, 41]]]], ["inline", "object-contents-maps-embedded", [], ["model", ["subexpr", "@mut", [["get", "map", ["loc", [null, [42, 48], [42, 51]]]]], [], []], "removeField", "removeField", "addField", "addField", "editField", "editField", "addElement", "addElement", "removeElement", "removeElement"], ["loc", [null, [42, 10], [45, 12]]]]],
        locals: ["map"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 51,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-maps-embedded.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container set-contents-list");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h5");
        var el3 = dom.createTextNode("Maps (");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(")");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element3 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element3, [1]), 1, 1);
        morphs[1] = dom.createMorphAt(element3, 3, 3);
        return morphs;
      },
      statements: [["content", "model.mapsList.length", ["loc", [null, [2, 12], [2, 37]]]], ["block", "each", [["get", "model.mapsList", ["loc", [null, [4, 10], [4, 24]]]]], [], 0, null, ["loc", [null, [4, 2], [49, 11]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-registers", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 2
            },
            "end": {
              "line": 25,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-registers.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row set-element-row");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-1");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3, "type", "button");
          dom.setAttribute(el3, "class", "btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "class", "glyphicon glyphicon-trash");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-3 field-label");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-8");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3, "type", "button");
          dom.setAttribute(el3, "class", "btn btn-xs btn-primary");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4, "class", "glyphicon glyphicon-pencil");
          dom.setAttribute(el4, "aria-hidden", "true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("code");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(element0, [1, 1]);
          var element2 = dom.childAt(element0, [5]);
          var element3 = dom.childAt(element2, [1]);
          var morphs = new Array(4);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]), 0, 0);
          morphs[2] = dom.createElementMorph(element3);
          morphs[3] = dom.createMorphAt(dom.childAt(element2, [3]), 1, 1);
          return morphs;
        },
        statements: [["element", "action", ["removeField", ["get", "model", ["loc", [null, [8, 33], [8, 38]]]], ["get", "register", ["loc", [null, [8, 39], [8, 47]]]]], [], ["loc", [null, [8, 10], [8, 49]]]], ["content", "register.name", ["loc", [null, [13, 16], [13, 33]]]], ["element", "action", ["editField", ["get", "model", ["loc", [null, [17, 31], [17, 36]]]], ["get", "register", ["loc", [null, [17, 37], [17, 45]]]]], [], ["loc", [null, [17, 10], [17, 47]]]], ["content", "register.value", ["loc", [null, [21, 10], [21, 28]]]]],
        locals: ["register"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 41,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-registers.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container set-contents-list");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h5");
        var el3 = dom.createTextNode("Registers (");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(")");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row set-element-row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-4 text-right");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-8");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("button");
        dom.setAttribute(el4, "type", "button");
        dom.setAttribute(el4, "class", "btn btn-sm btn-primary");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5, "class", "glyphicon glyphicon-plus");
        dom.setAttribute(el5, "aria-hidden", "true");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n        Add Register\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element4 = dom.childAt(fragment, [0]);
        var element5 = dom.childAt(element4, [5]);
        var element6 = dom.childAt(element5, [3]);
        var element7 = dom.childAt(element6, [3]);
        var morphs = new Array(5);
        morphs[0] = dom.createMorphAt(dom.childAt(element4, [1]), 1, 1);
        morphs[1] = dom.createMorphAt(element4, 3, 3);
        morphs[2] = dom.createMorphAt(dom.childAt(element5, [1]), 1, 1);
        morphs[3] = dom.createMorphAt(element6, 1, 1);
        morphs[4] = dom.createElementMorph(element7);
        return morphs;
      },
      statements: [["content", "model.registersList.length", ["loc", [null, [2, 17], [2, 47]]]], ["block", "each", [["get", "model.registersList", ["loc", [null, [4, 10], [4, 29]]]]], [], 0, null, ["loc", [null, [4, 2], [25, 11]]]], ["inline", "input", [], ["value", ["subexpr", "@mut", [["get", "fieldToAddName", ["loc", [null, [29, 20], [29, 34]]]]], [], []], "class", "form-control", "placeholder", "field name"], ["loc", [null, [29, 6], [29, 82]]]], ["inline", "input", [], ["value", ["subexpr", "@mut", [["get", "fieldToAddValue", ["loc", [null, [32, 20], [32, 35]]]]], [], []], "class", "form-control", "placeholder", "value"], ["loc", [null, [32, 6], [32, 78]]]], ["element", "action", ["addField", ["get", "model", ["loc", [null, [34, 28], [34, 33]]]]], [], ["loc", [null, [34, 8], [34, 35]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-set-elements", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 2,
              "column": 2
            },
            "end": {
              "line": 11,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-set-elements.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row set-element-row");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-12");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n          \n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("span");
          dom.setAttribute(el3, "class", "set-element");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(element0, 1, 1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 0, 0);
          return morphs;
        },
        statements: [["inline", "button.set-element-remove", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [5, 42], [5, 47]]]]], [], []], "item", ["subexpr", "@mut", [["get", "element", ["loc", [null, [5, 53], [5, 60]]]]], [], []], "removeElement", "removeElement"], ["loc", [null, [5, 8], [6, 39]]]], ["content", "element", ["loc", [null, [8, 34], [8, 45]]]]],
        locals: ["element"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 25,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-set-elements.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container set-contents-list");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row set-element-row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-12");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("button");
        dom.setAttribute(el4, "type", "button");
        dom.setAttribute(el4, "class", "btn btn-sm btn-primary");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5, "class", "glyphicon glyphicon-plus");
        dom.setAttribute(el5, "aria-hidden", "true");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n        Add element\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element1 = dom.childAt(fragment, [0]);
        var element2 = dom.childAt(element1, [3, 1]);
        var element3 = dom.childAt(element2, [3]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element1, 1, 1);
        morphs[1] = dom.createMorphAt(element2, 1, 1);
        morphs[2] = dom.createElementMorph(element3);
        return morphs;
      },
      statements: [["block", "each", [["get", "setElements", ["loc", [null, [2, 10], [2, 21]]]]], [], 0, null, ["loc", [null, [2, 2], [11, 11]]]], ["inline", "input", [], ["value", ["subexpr", "@mut", [["get", "elementToAdd", ["loc", [null, [15, 20], [15, 32]]]]], [], []], "class", "form-control"], ["loc", [null, [15, 6], [15, 55]]]], ["element", "action", ["addElement", ["get", "model", ["loc", [null, [17, 30], [17, 35]]]]], [], ["loc", [null, [17, 8], [17, 37]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-set", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 15,
              "column": 8
            },
            "end": {
              "line": 19,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-set.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["inline", "object-contents-set-elements", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [16, 47], [16, 52]]]]], [], []], "setElements", ["subexpr", "@mut", [["get", "model.contentsForDisplay", ["loc", [null, [17, 22], [17, 46]]]]], [], []], "removeElement", "removeElement", "addElement", "addElement"], ["loc", [null, [16, 10], [18, 65]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 64,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-set.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "riak-object");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "object-contents");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-header");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4, "class", "object-contents-info");
        var el5 = dom.createTextNode("\n        Set\n        (");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode(")\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4, "class", "object-contents-actions");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-body");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4, "class", "form-inline");
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-footer");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Page loaded:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode(" ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0, 1]);
        var element1 = dom.childAt(element0, [1]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(dom.childAt(element1, [1]), 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [3]), 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [3, 1]), 1, 1);
        morphs[3] = dom.createMorphAt(dom.childAt(element0, [5]), 3, 3);
        return morphs;
      },
      statements: [["content", "model.contentsForDisplay.length", ["loc", [null, [6, 9], [6, 44]]]], ["inline", "object-actions", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [9, 31], [9, 36]]]]], [], []], "isEditing", ["subexpr", "@mut", [["get", "isEditing", ["loc", [null, [9, 47], [9, 56]]]]], [], []], "deleteObject", "deleteObject"], ["loc", [null, [9, 8], [10, 37]]]], ["block", "if", [["get", "model.contentsForDisplay", ["loc", [null, [15, 14], [15, 38]]]]], [], 0, null, ["loc", [null, [15, 8], [19, 15]]]], ["content", "model.metadata.dateLoaded", ["loc", [null, [23, 36], [23, 65]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents-sets-embedded", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 2
            },
            "end": {
              "line": 22,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-sets-embedded.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row map-composite-field-name");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-12");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3, "type", "button");
          dom.setAttribute(el3, "class", "btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n          remove set\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n          \n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1, "class", "row");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "col-md-12");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [1]);
          var morphs = new Array(3);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 0, 0);
          morphs[2] = dom.createMorphAt(dom.childAt(fragment, [3, 1]), 1, 1);
          return morphs;
        },
        statements: [["element", "action", ["removeField", ["get", "model", ["loc", [null, [8, 33], [8, 38]]]], ["get", "setField", ["loc", [null, [8, 39], [8, 47]]]]], [], ["loc", [null, [8, 10], [8, 49]]]], ["content", "setField.fullName", ["loc", [null, [12, 16], [12, 37]]]], ["inline", "object-contents-set-elements", [], ["model", ["subexpr", "@mut", [["get", "setField", ["loc", [null, [17, 45], [17, 53]]]]], [], []], "setElements", ["subexpr", "@mut", [["get", "setField.value", ["loc", [null, [18, 20], [18, 34]]]]], [], []], "removeElement", "removeElement", "addElement", "addElement"], ["loc", [null, [17, 8], [19, 63]]]]],
        locals: ["setField"],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 24,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents-sets-embedded.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container set-contents-list");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h5");
        var el3 = dom.createTextNode("Sets (");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(")");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element2, [1]), 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        return morphs;
      },
      statements: [["content", "model.setsList.length", ["loc", [null, [2, 12], [2, 37]]]], ["block", "each", [["get", "model.setsList", ["loc", [null, [4, 10], [4, 24]]]]], [], 0, null, ["loc", [null, [4, 2], [22, 11]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-contents", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 5,
              "column": 8
            },
            "end": {
              "line": 13,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("form");
          dom.setAttribute(el1, "class", "form-inline");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "form-group");
          var el3 = dom.createTextNode("\n              ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("label");
          dom.setAttribute(el3, "for", "contentType");
          var el4 = dom.createTextNode("Content Type:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n              ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n          ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1]), 3, 3);
          return morphs;
        },
        statements: [["inline", "input", [], ["value", ["subexpr", "@mut", [["get", "model.metadata.contentType", ["loc", [null, [9, 28], [9, 54]]]]], [], []], "id", "metadata.contentType", "class", "form-control"], ["loc", [null, [9, 14], [10, 62]]]]],
        locals: [],
        templates: []
      };
    })();
    var child1 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 13,
              "column": 8
            },
            "end": {
              "line": 15,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("b");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
          return morphs;
        },
        statements: [["content", "model.metadata.contentType", ["loc", [null, [14, 13], [14, 43]]]]],
        locals: [],
        templates: []
      };
    })();
    var child2 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 23,
              "column": 6
            },
            "end": {
              "line": 34,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("form");
          var el2 = dom.createTextNode("\n          ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2, "class", "form-group");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n          ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n          ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("button");
          dom.setAttribute(el2, "type", "button");
          dom.setAttribute(el2, "class", "btn btn-md btn-primary");
          var el3 = dom.createTextNode("\n            Save\n          ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(element0, [3]);
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 1, 1);
          morphs[1] = dom.createElementMorph(element1);
          return morphs;
        },
        statements: [["inline", "textarea", [], ["class", "form-control", "value", ["subexpr", "@mut", [["get", "model.contents", ["loc", [null, [26, 50], [26, 64]]]]], [], []], "rows", 20, "autofocus", true], ["loc", [null, [26, 12], [27, 28]]]], ["element", "action", ["saveObject", ["get", "model", ["loc", [null, [30, 34], [30, 39]]]]], [], ["loc", [null, [30, 12], [30, 41]]]]],
        locals: [],
        templates: []
      };
    })();
    var child3 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 35,
                "column": 8
              },
              "end": {
                "line": 37,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("code");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
            return morphs;
          },
          statements: [["content", "model.contentsForDisplay", ["loc", [null, [36, 16], [36, 44]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 34,
              "column": 6
            },
            "end": {
              "line": 38,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [["block", "if", [["get", "model.contentsForDisplay", ["loc", [null, [35, 14], [35, 38]]]]], [], 0, null, ["loc", [null, [35, 8], [37, 15]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 46,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "object-contents");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-header");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4, "class", "object-contents-info");
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4, "class", "object-contents-actions");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-body");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "object-contents-footer");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Page loaded:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode(" ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0, 1]);
        var element3 = dom.childAt(element2, [1]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(dom.childAt(element3, [1]), 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element3, [3]), 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(element2, [3]), 1, 1);
        morphs[3] = dom.createMorphAt(dom.childAt(element2, [5]), 3, 3);
        return morphs;
      },
      statements: [["block", "if", [["get", "isEditing", ["loc", [null, [5, 14], [5, 23]]]]], [], 0, 1, ["loc", [null, [5, 8], [15, 15]]]], ["inline", "object-actions", [], ["model", ["subexpr", "@mut", [["get", "model", ["loc", [null, [18, 31], [18, 36]]]]], [], []], "isEditing", ["subexpr", "@mut", [["get", "isEditing", ["loc", [null, [18, 47], [18, 56]]]]], [], []], "deleteObject", "deleteObject"], ["loc", [null, [18, 8], [19, 37]]]], ["block", "if", [["get", "isEditing", ["loc", [null, [23, 12], [23, 21]]]]], [], 2, 3, ["loc", [null, [23, 6], [38, 13]]]], ["content", "model.metadata.dateLoaded", ["loc", [null, [41, 36], [41, 65]]]]],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-headers-edit", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 6,
                "column": 4
              },
              "end": {
                "line": 11,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-headers-edit.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("tr");
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2, "width", "30%;");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 0, 0);
            return morphs;
          },
          statements: [["content", "header.key", ["loc", [null, [8, 25], [8, 39]]]], ["inline", "input", [], ["value", ["subexpr", "@mut", [["get", "header.value", ["loc", [null, [9, 26], [9, 38]]]]], [], []], "id", ["subexpr", "@mut", [["get", "header.key", ["loc", [null, [9, 42], [9, 52]]]]], [], []], "class", "form-control"], ["loc", [null, [9, 12], [9, 75]]]]],
          locals: ["header"],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 15,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-headers-edit.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("h4");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1, "class", "table");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tbody");
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
          morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3, 1]), 1, 1);
          return morphs;
        },
        statements: [["content", "title", ["loc", [null, [3, 6], [3, 15]]]], ["block", "each", [["get", "headers", ["loc", [null, [6, 12], [6, 19]]]]], [], 0, null, ["loc", [null, [6, 4], [11, 13]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 16,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-headers-edit.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "if", [["get", "headers", ["loc", [null, [1, 6], [1, 13]]]]], [], 0, null, ["loc", [null, [1, 0], [15, 7]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-headers", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 6,
                "column": 4
              },
              "end": {
                "line": 11,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-headers.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("tr");
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2, "width", "30%;");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 0, 0);
            return morphs;
          },
          statements: [["content", "header.key", ["loc", [null, [8, 25], [8, 39]]]], ["content", "header.value", ["loc", [null, [9, 12], [9, 28]]]]],
          locals: ["header"],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 15,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-headers.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("h4");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1, "class", "table");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tbody");
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]), 0, 0);
          morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3, 1]), 1, 1);
          return morphs;
        },
        statements: [["content", "title", ["loc", [null, [3, 6], [3, 15]]]], ["block", "each", [["get", "headers", ["loc", [null, [6, 12], [6, 19]]]]], [], 0, null, ["loc", [null, [6, 4], [11, 13]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 16,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-headers.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "if", [["get", "headers", ["loc", [null, [1, 6], [1, 13]]]]], [], 0, null, ["loc", [null, [1, 0], [15, 7]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-location", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 24,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-location.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "crumb-trail");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "style", "display: inline-block;");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "style", "display: inline-block;");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    /\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-12 text-center");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4, "class", "cluster-resource-header center-block");
        var el5 = dom.createTextNode("\n        key:\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var element1 = dom.childAt(element0, [1]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(element1, 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [3]), 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(element1, [5]), 1, 1);
        morphs[3] = dom.createMorphAt(dom.childAt(element0, [3, 1, 1, 1]), 0, 0);
        return morphs;
      },
      statements: [["inline", "link.link-cluster", [], ["cluster", ["subexpr", "@mut", [["get", "object.cluster", ["loc", [null, [3, 32], [3, 46]]]]], [], []]], ["loc", [null, [3, 4], [3, 48]]]], ["inline", "link.bucket-type", [], ["bucketType", ["subexpr", "@mut", [["get", "object.bucketType", ["loc", [null, [6, 36], [6, 53]]]]], [], []]], ["loc", [null, [6, 6], [6, 55]]]], ["inline", "link.link-bucket", [], ["bucket", ["subexpr", "@mut", [["get", "object.bucket", ["loc", [null, [10, 32], [10, 45]]]]], [], []]], ["loc", [null, [10, 6], [10, 47]]]], ["content", "object.key", ["loc", [null, [19, 14], [19, 28]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/object-metadata", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 5,
                "column": 8
              },
              "end": {
                "line": 6,
                "column": 35
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 6
            },
            "end": {
              "line": 7,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["block", "object-headers-edit", [], ["headers", ["subexpr", "@mut", [["get", "metadata.headersIndexes", ["loc", [null, [5, 39], [5, 62]]]]], [], []], "title", "Secondary Indexes"], 0, null, ["loc", [null, [5, 8], [6, 59]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 8,
                "column": 8
              },
              "end": {
                "line": 9,
                "column": 35
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 7,
              "column": 6
            },
            "end": {
              "line": 10,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["block", "object-headers", [], ["headers", ["subexpr", "@mut", [["get", "metadata.headersIndexes", ["loc", [null, [8, 34], [8, 57]]]]], [], []], "title", "Secondary Indexes"], 0, null, ["loc", [null, [8, 8], [9, 54]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    var child2 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 16,
                "column": 8
              },
              "end": {
                "line": 17,
                "column": 32
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 15,
              "column": 6
            },
            "end": {
              "line": 18,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["block", "object-headers-edit", [], ["headers", ["subexpr", "@mut", [["get", "metadata.headersCustom", ["loc", [null, [16, 39], [16, 61]]]]], [], []], "title", "Custom Headers"], 0, null, ["loc", [null, [16, 8], [17, 56]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    var child3 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 19,
                "column": 8
              },
              "end": {
                "line": 20,
                "column": 32
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 18,
              "column": 6
            },
            "end": {
              "line": 21,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["block", "object-headers", [], ["headers", ["subexpr", "@mut", [["get", "metadata.headersCustom", ["loc", [null, [19, 34], [19, 56]]]]], [], []], "title", "Custom Headers"], 0, null, ["loc", [null, [19, 8], [20, 51]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 25,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-10");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-10");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 1]), 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]), 1, 1);
        return morphs;
      },
      statements: [["block", "if", [["get", "isEditing", ["loc", [null, [4, 12], [4, 21]]]]], [], 0, 1, ["loc", [null, [4, 6], [10, 13]]]], ["block", "if", [["get", "isEditing", ["loc", [null, [15, 12], [15, 21]]]]], [], 2, 3, ["loc", [null, [15, 6], [21, 13]]]]],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  })());
});
define("ember-riak-explorer/templates/components/object-version", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 27,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/object-version.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "container");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-2");
        dom.setAttribute(el3, "style", "text-align: right;");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Last Modified:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-10");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-2");
        dom.setAttribute(el3, "style", "text-align: right;");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Etag:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-10");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-2");
        dom.setAttribute(el3, "style", "text-align: right;");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Causal Context:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3, "class", "col-md-10");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]), 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]), 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 3]), 1, 1);
        return morphs;
      },
      statements: [["content", "object.metadata.dateLastModified", ["loc", [null, [7, 6], [7, 42]]]], ["content", "object.metadata.etag", ["loc", [null, [15, 6], [15, 30]]]], ["content", "object.metadata.causalContext", ["loc", [null, [23, 6], [23, 39]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/components/pagination-component", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 11,
                "column": 4
              },
              "end": {
                "line": 13,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/pagination-component.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            dom.setAttribute(el1, "class", "pagination-link number-link");
            var el2 = dom.createComment("");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(2);
            morphs[0] = dom.createElementMorph(element0);
            morphs[1] = dom.createMorphAt(element0, 0, 0);
            return morphs;
          },
          statements: [["element", "action", ["numberLinkClick", ["get", "link", ["loc", [null, [12, 73], [12, 77]]]]], [], ["loc", [null, [12, 46], [12, 79]]]], ["content", "link", ["loc", [null, [12, 80], [12, 88]]]]],
          locals: ["link"],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 5,
              "column": 0
            },
            "end": {
              "line": 19,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/pagination-component.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("ul");
          dom.setAttribute(el1, "class", "pagination-links");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("li");
          dom.setAttribute(el2, "class", "pagination-link text-link");
          var el3 = dom.createTextNode("\n      Prev\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("li");
          dom.setAttribute(el2, "class", "pagination-link text-link");
          var el3 = dom.createTextNode("\n      Next\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(element1, [1]);
          var element3 = dom.childAt(element1, [5]);
          var morphs = new Array(5);
          morphs[0] = dom.createAttrMorph(element2, 'disabled');
          morphs[1] = dom.createElementMorph(element2);
          morphs[2] = dom.createMorphAt(element1, 3, 3);
          morphs[3] = dom.createAttrMorph(element3, 'disabled');
          morphs[4] = dom.createElementMorph(element3);
          return morphs;
        },
        statements: [["attribute", "disabled", ["subexpr", "if", [["get", "shouldPrevBeDisabled", ["loc", [null, [7, 56], [7, 76]]]], "disabled"], [], ["loc", [null, [7, 51], [7, 89]]]]], ["element", "action", ["prevLinkClick"], [], ["loc", [null, [7, 90], [7, 116]]]], ["block", "each", [["get", "numberLinks", ["loc", [null, [11, 12], [11, 23]]]]], [], 0, null, ["loc", [null, [11, 4], [13, 13]]]], ["attribute", "disabled", ["subexpr", "if", [["get", "shouldNextBeDisabled", ["loc", [null, [15, 56], [15, 76]]]], "disabled"], [], ["loc", [null, [15, 51], [15, 89]]]]], ["element", "action", ["nextLinkClick"], [], ["loc", [null, [15, 90], [15, 116]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 20,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/pagination-component.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "pagination-content");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0]), 1, 1);
        morphs[1] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["content", "yield", ["loc", [null, [2, 2], [2, 11]]]], ["block", "if", [["get", "shouldShowPaginationLinks", ["loc", [null, [5, 6], [5, 31]]]]], [], 0, null, ["loc", [null, [5, 0], [19, 7]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/riak-buckets", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 3,
              "column": 2
            },
            "end": {
              "line": 15,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-buckets.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3, "class", "container");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("div");
          dom.setAttribute(el4, "class", "row");
          var el5 = dom.createTextNode("\n            ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          dom.setAttribute(el5, "class", "col-md-6");
          var el6 = dom.createTextNode("\n              ");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode("\n            ");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n          ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1, 1, 1, 1]), 1, 1);
          return morphs;
        },
        statements: [["inline", "link.link-bucket", [], ["bucket", ["subexpr", "@mut", [["get", "bucket", ["loc", [null, [9, 40], [9, 46]]]]], [], []]], ["loc", [null, [9, 14], [9, 48]]]]],
        locals: ["bucket"],
        templates: []
      };
    })();
    var child1 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 15,
              "column": 2
            },
            "end": {
              "line": 17,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-buckets.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    No buckets found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 20,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/riak-buckets.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1, "class", "table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1]), 1, 1);
        return morphs;
      },
      statements: [["block", "each", [["get", "bucketList.buckets", ["loc", [null, [3, 10], [3, 28]]]]], [], 0, 1, ["loc", [null, [3, 2], [17, 11]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/components/riak-keys", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 3,
              "column": 2
            },
            "end": {
              "line": 15,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-keys.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3, "class", "container");
          var el4 = dom.createTextNode("\n          ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("div");
          dom.setAttribute(el4, "class", "row");
          var el5 = dom.createTextNode("\n            ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          var el6 = dom.createTextNode("\n              ");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode("\n            ");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n          ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1, 1, 1, 1]), 1, 1);
          return morphs;
        },
        statements: [["inline", "link.link-object", [], ["obj", ["subexpr", "@mut", [["get", "obj", ["loc", [null, [9, 37], [9, 40]]]]], [], []]], ["loc", [null, [9, 14], [9, 42]]]]],
        locals: ["obj"],
        templates: []
      };
    })();
    var child1 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 15,
              "column": 2
            },
            "end": {
              "line": 17,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-keys.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    No keys found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 20,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/riak-keys.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1, "class", "table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1]), 1, 1);
        return morphs;
      },
      statements: [["block", "each", [["get", "keys", ["loc", [null, [3, 10], [3, 14]]]]], [], 0, 1, ["loc", [null, [3, 2], [17, 11]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/components/riak-node", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 4
            },
            "end": {
              "line": 2,
              "column": 6
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-node.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("Node\n  Ping");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    var child1 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 2
            },
            "end": {
              "line": 6,
              "column": 14
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-node.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    Node Stats");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 8,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/riak-node.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("td");
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("td");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("td");
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0]), 0, 0);
        morphs[1] = dom.createMorphAt(dom.childAt(fragment, [2]), 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(fragment, [4]), 0, 0);
        return morphs;
      },
      statements: [["block", "link-to", ["riak_ping", ["subexpr", "query-params", [], ["node_id", ["get", "node.id", ["loc", [null, [1, 49], [1, 56]]]]], ["loc", [null, [1, 27], [1, 57]]]]], ["classNames", "btn btn-sm btn-default"], 0, null, ["loc", [null, [1, 4], [2, 18]]]], ["block", "link-to", ["node_stats", ["subexpr", "query-params", [], ["node_id", ["get", "node.id", ["loc", [null, [4, 48], [4, 55]]]]], ["loc", [null, [4, 26], [4, 56]]]]], ["classNames", "btn btn-sm btn-primary"], 1, null, ["loc", [null, [4, 2], [6, 26]]]], ["content", "node.id", ["loc", [null, [7, 4], [7, 15]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/components/search-indexes", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 14,
                "column": 8
              },
              "end": {
                "line": 16,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/search-indexes.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "index.schema.name", ["loc", [null, [15, 10], [15, 31]]]]],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 10,
              "column": 2
            },
            "end": {
              "line": 20,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/search-indexes.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("      ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 0, 0);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 1, 1);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5]), 0, 0);
          return morphs;
        },
        statements: [["inline", "link.link-index", [], ["searchIndex", ["subexpr", "@mut", [["get", "index", ["loc", [null, [12, 40], [12, 45]]]]], [], []]], ["loc", [null, [12, 10], [12, 47]]]], ["block", "link-to", ["search-schema", ["get", "index.cluster.id", ["loc", [null, [14, 35], [14, 51]]]], ["get", "index.schema.name", ["loc", [null, [14, 52], [14, 69]]]]], ["class", "btn btn-small btn-primary"], 0, null, ["loc", [null, [14, 8], [16, 20]]]], ["content", "index.nVal", ["loc", [null, [18, 10], [18, 24]]]]],
        locals: ["index"],
        templates: [child0]
      };
    })();
    var child1 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 20,
              "column": 2
            },
            "end": {
              "line": 22,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/search-indexes.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    No indexes found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 25,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/search-indexes.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1, "class", "table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Index");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Schema");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("n_val");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n  ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 3]), 1, 1);
        return morphs;
      },
      statements: [["block", "each", [["get", "indexes", ["loc", [null, [10, 10], [10, 17]]]]], [], 0, 1, ["loc", [null, [10, 2], [22, 11]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/components/view-label", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/view-label.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(":\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [["content", "pre-label", ["loc", [null, [2, 2], [2, 15]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/view-label.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        morphs[1] = dom.createMorphAt(fragment, 1, 1, contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [["block", "if", [["get", "pre-label", ["loc", [null, [1, 6], [1, 15]]]]], [], 0, null, ["loc", [null, [1, 0], [3, 7]]]], ["content", "label", ["loc", [null, [4, 0], [4, 9]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/components/wrapper-panel", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/wrapper-panel.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "wrapper-panel");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2, "class", "row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1]), 1, 1);
        return morphs;
      },
      statements: [["content", "yield", ["loc", [null, [3, 4], [3, 13]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/error/cluster-not-found", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/error/cluster-not-found.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("p");
          var el2 = dom.createTextNode("The cluster id you requested was not found");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 4,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/error/cluster-not-found.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "dashboard-module", [], ["label", "404 Cluster Not Found"], 0, null, ["loc", [null, [1, 0], [3, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/error/object-not-found", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/error/object-not-found.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("p");
          var el2 = dom.createTextNode("The object id you requested was not found");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 4,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/error/object-not-found.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "dashboard-module", [], ["label", "404 Object Not Found"], 0, null, ["loc", [null, [1, 0], [3, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/error/service-not-found", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 7,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/error/service-not-found.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("p");
          var el2 = dom.createTextNode("The application was not able to connect to the Riak Explorer API.");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("p");
          var el2 = dom.createTextNode("\n    Please refer to any documentation in the\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("a");
          dom.setAttribute(el2, "href", "https://github.com/basho-labs/riak_explorer");
          var el3 = dom.createTextNode("Github repository");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 8,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/error/service-not-found.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "dashboard-module", [], ["label", "503 Service Unavailable"], 0, null, ["loc", [null, [1, 0], [7, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/error/unknown", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 3,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/error/unknown.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("p");
          var el2 = dom.createTextNode("Consider opening a bug report.");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 4,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/error/unknown.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "dashboard-module", [], ["label", "An unknown error has occurred"], 0, null, ["loc", [null, [1, 0], [3, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/explorer-api", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 15,
                "column": 8
              },
              "end": {
                "line": 18,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "label label-success");
            var el2 = dom.createTextNode("service available");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
            return morphs;
          },
          statements: [["content", "model.pingResult.ping.message", ["loc", [null, [16, 10], [16, 43]]]]],
          locals: [],
          templates: []
        };
      })();
      var child1 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 18,
                "column": 8
              },
              "end": {
                "line": 20,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1, "class", "label label-default");
            var el2 = dom.createTextNode("service unavailable");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child2 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 26,
                "column": 8
              },
              "end": {
                "line": 28,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          Enabled\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      var child3 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 28,
                "column": 8
              },
              "end": {
                "line": 30,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("          Disabled\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() {
            return [];
          },
          statements: [],
          locals: [],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 6,
              "column": 0
            },
            "end": {
              "line": 34,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1, "class", "key-value-table");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tr");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3, "class", "key");
          var el4 = dom.createTextNode("Name:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3, "class", "value");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tr");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3, "class", "key");
          var el4 = dom.createTextNode("Ping:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3, "class", "value");
          var el4 = dom.createTextNode("\n");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tr");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3, "class", "key");
          var el4 = dom.createTextNode("Development Mode:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3, "class", "value");
          var el4 = dom.createTextNode("\n");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("      ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element1 = dom.childAt(fragment, [1]);
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(dom.childAt(element1, [1, 3]), 0, 0);
          morphs[1] = dom.createMorphAt(dom.childAt(element1, [3, 3]), 1, 1);
          morphs[2] = dom.createMorphAt(dom.childAt(element1, [5, 3]), 1, 1);
          return morphs;
        },
        statements: [["content", "model.service", ["loc", [null, [10, 24], [10, 41]]]], ["block", "if", [["get", "model.pingResult.ping", ["loc", [null, [15, 14], [15, 35]]]]], [], 0, 1, ["loc", [null, [15, 8], [20, 15]]]], ["block", "if", [["get", "model.propsResult.props.development_mode", ["loc", [null, [26, 14], [26, 54]]]]], [], 2, 3, ["loc", [null, [26, 8], [30, 15]]]]],
        locals: [],
        templates: [child0, child1, child2, child3]
      };
    })();
    var child1 = (function () {
      var child0 = (function () {
        return {
          meta: {
            "revision": "Ember@1.13.11",
            "loc": {
              "source": null,
              "start": {
                "line": 45,
                "column": 4
              },
              "end": {
                "line": 50,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("tr");
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            var el3 = dom.createTextNode("[");
            dom.appendChild(el2, el3);
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("]");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]), 0, 0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]), 1, 1);
            return morphs;
          },
          statements: [["content", "route.links.self", ["loc", [null, [47, 12], [47, 32]]]], ["content", "route.resources", ["loc", [null, [48, 13], [48, 32]]]]],
          locals: ["route"],
          templates: []
        };
      })();
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 36,
              "column": 0
            },
            "end": {
              "line": 53,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1, "class", "table table-hover");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("thead");
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("th");
          var el5 = dom.createTextNode("Path");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("th");
          var el5 = dom.createTextNode("Resources");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n    ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tbody");
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n  ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3]), 1, 1);
          return morphs;
        },
        statements: [["block", "each", [["get", "model.routes", ["loc", [null, [45, 12], [45, 24]]]]], [], 0, null, ["loc", [null, [45, 4], [50, 13]]]]],
        locals: [],
        templates: [child0]
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 54,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "view-header");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element2 = dom.childAt(fragment, [0]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(element2, 1, 1);
        morphs[1] = dom.createMorphAt(element2, 3, 3);
        morphs[2] = dom.createMorphAt(fragment, 2, 2, contextualElement);
        morphs[3] = dom.createMorphAt(fragment, 4, 4, contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["inline", "breadcrumb-component", [], ["pageTitle", "explorer api"], ["loc", [null, [2, 2], [2, 51]]]], ["inline", "view-label", [], ["label", "Riak Explorer API"], ["loc", [null, [3, 2], [3, 42]]]], ["block", "dashboard-module", [], ["label", "Service Information"], 0, null, ["loc", [null, [6, 0], [34, 21]]]], ["block", "dashboard-module", [], ["label", "Routes"], 1, null, ["loc", [null, [36, 0], [53, 21]]]]],
      locals: [],
      templates: [child0, child1]
    };
  })());
});
define("ember-riak-explorer/templates/index", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 4,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/index.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("p");
          var el2 = dom.createTextNode("Select a cluster from the nav bar on the left to view cluser information");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("p");
          var el2 = dom.createTextNode("View the ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [3]), 1, 1);
          return morphs;
        },
        statements: [["inline", "link-to", ["Explorer API", "explorer_api"], [], ["loc", [null, [3, 14], [3, 55]]]]],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/index.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment, 0, 0, contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [["block", "dashboard-module", [], ["label", "Welcome to Riak Explorer"], 0, null, ["loc", [null, [1, 0], [4, 21]]]]],
      locals: [],
      templates: [child0]
    };
  })());
});
define("ember-riak-explorer/templates/loading", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 4,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/loading.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1, "class", "loading-container");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0]), 1, 1);
        return morphs;
      },
      statements: [["content", "loading-spinner", ["loc", [null, [2, 2], [2, 21]]]]],
      locals: [],
      templates: []
    };
  })());
});
define("ember-riak-explorer/templates/topbar", ["exports"], function (exports) {
  exports["default"] = Ember.HTMLBars.template((function () {
    var child0 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 2,
              "column": 2
            },
            "end": {
              "line": 4,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/topbar.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("img");
          dom.setAttribute(el1, "src", "assets/images/sample_logo.png");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    var child1 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 9,
              "column": 8
            },
            "end": {
              "line": 12,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/topbar.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1, "class", "glyphicon glyphicon-tasks");
          dom.setAttribute(el1, "aria-hidden", "true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n          Some Item\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    var child2 = (function () {
      return {
        meta: {
          "revision": "Ember@1.13.11",
          "loc": {
            "source": null,
            "start": {
              "line": 16,
              "column": 8
            },
            "end": {
              "line": 19,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/topbar.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1, "class", "glyphicon glyphicon-user");
          dom.setAttribute(el1, "aria-hidden", "true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n          Authz User\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() {
          return [];
        },
        statements: [],
        locals: [],
        templates: []
      };
    })();
    return {
      meta: {
        "revision": "Ember@1.13.11",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 24,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/topbar.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("nav");
        dom.setAttribute(el1, "class", "topbar");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("nav");
        dom.setAttribute(el2, "class", "topbar-nav");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("ul");
        dom.setAttribute(el3, "class", "topbar-nav-group");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("li");
        dom.setAttribute(el4, "class", "topbar-nav-item");
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("li");
        dom.setAttribute(el4, "class", "topbar-nav-item");
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var element1 = dom.childAt(element0, [3, 1]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0, 1, 1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [1]), 1, 1);
        morphs[2] = dom.createMorphAt(dom.childAt(element1, [3]), 1, 1);
        return morphs;
      },
      statements: [["block", "link-to", ["application"], ["class", "logo"], 0, null, ["loc", [null, [2, 2], [4, 14]]]], ["block", "link-to", ["application"], [], 1, null, ["loc", [null, [9, 8], [12, 20]]]], ["block", "link-to", ["application"], [], 2, null, ["loc", [null, [16, 8], [19, 20]]]]],
      locals: [],
      templates: [child0, child1, child2]
    };
  })());
});
define("ember-riak-explorer/utils/riak-util", ["exports"], function (exports) {
  exports["default"] = objectToArray;
  function objectToString(obj) {
    var propsStr = "{";
    var i = 0;
    for (var prop in obj) {
      if (obj.hasOwnProperty(prop)) {
        if (i > 0) {
          propsStr += ", ";
        }
        propsStr += prop + ': ' + obj[prop];
        i++;
      }
    }
    return propsStr + "}";
  }

  function objectToArray(obj) {
    var alphaSorted = arguments.length <= 1 || arguments[1] === undefined ? true : arguments[1];

    var propsArray = [];

    for (var prop in obj) {
      if (obj.hasOwnProperty(prop)) {
        if (Object.prototype.toString.call(obj[prop]) === '[object Object]') {
          propsArray.push({ key: prop, value: objectToString(obj[prop]) });
        } else if (Object.prototype.toString.call(obj[prop]) === '[object Array]') {
          var arrStr = "[" + obj[prop].join(', ') + "]";
          propsArray.push({ key: prop, value: arrStr });
        } else {
          propsArray.push({ key: prop, value: obj[prop] });
        }
      }
    }

    if (alphaSorted) {
      propsArray.sort(function (a, b) {
        if (a.key < b.key) {
          return -1;
        } else if (a.key > b.key) {
          return 1;
        } else {
          return 0;
        }
      });
    }

    return propsArray;
  }
});
/* jshint ignore:start */

/* jshint ignore:end */

/* jshint ignore:start */

define('ember-riak-explorer/config/environment', ['ember'], function(Ember) {
  var prefix = 'ember-riak-explorer';
/* jshint ignore:start */

try {
  var metaName = prefix + '/config/environment';
  var rawConfig = Ember['default'].$('meta[name="' + metaName + '"]').attr('content');
  var config = JSON.parse(unescape(rawConfig));

  return { 'default': config };
}
catch(err) {
  throw new Error('Could not read config from meta tag with name "' + metaName + '".');
}

/* jshint ignore:end */

});

if (!runningTests) {
  require("ember-riak-explorer/app")["default"].create({"name":"ember-riak-explorer","version":"0.0.0+0cd80890"});
}

/* jshint ignore:end */
//# sourceMappingURL=ember-riak-explorer.map