"use strict";
/* jshint ignore:start */

/* jshint ignore:end */

define('ember-riak-explorer/acceptance-tests/main', ['exports', 'ember-cli-sri/acceptance-tests/main'], function (exports, main) {

	'use strict';



	exports['default'] = main['default'];

});
define('ember-riak-explorer/adapters/application', ['exports', 'ember-riak-explorer/adapters/explorer-resource'], function (exports, ExplorerResourceAdapter) {

	'use strict';

	exports['default'] = ExplorerResourceAdapter['default'].extend({});

});
define('ember-riak-explorer/adapters/explorer-resource', ['exports', 'ember-data', 'ember'], function (exports, DS, Ember) {

    'use strict';

    var ExplorerResourceAdapter = DS['default'].RESTAdapter.extend({
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
        normalizeId: function normalizeId(record, type, query, idKey) {
            var prefix = [];
            if (query.clusterId) {
                prefix.push(query.clusterId);
            }
            if (query.bucketTypeId && type.modelName !== 'bucket-type') {
                prefix.push(query.bucketTypeId);
            }
            if (!idKey) {
                idKey = 'id';
            }
            var originalId = record[idKey];
            record.original_id = originalId;
            prefix.push(originalId);
            var compositeId = prefix.join('/');
            record['id'] = compositeId;
            if (record.props) {
                record.props['id'] = compositeId;
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
            return Ember['default'].String.underscore(Ember['default'].String.pluralize(type));
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
            var url = this.buildURL(type.modelName, null, null, 'query', _query);
            var adapter = this;
            var root;
            var promise = this.ajax(url, 'GET').then(function (payload) {
                root = adapter.pathForType(type.modelName);
                // console.log('1) model name: %O, query payload: %O, root: %O', type.modelName, payload, root);
                for (var i = 0; i < payload[root].length; i++) {
                    var record = payload[root][i];
                    adapter.normalizeId(record, type, _query);
                    adapter.injectParentIds(record, _query);
                    adapter.normalizeProps(record, type.modelName);
                    // console.log("payload after normalize: %O", payload);
                }
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
            var url = this.buildURL(type.modelName, null, null, 'query', query);
            var adapter = this;
            var root = Ember['default'].String.underscore(type.modelName);
            var promise = this.ajax(url, 'GET').then(function (payload) {
                // console.log('model name: %O, query payload: %O, root: %O', type.modelName, payload, root);
                adapter.normalizeId(payload[root], type, query);
                adapter.injectParentIds(payload[root], query);
                adapter.normalizeProps(payload[root], type.modelName);
                // console.log("payload after normalize: %O", payload);
                return payload;
            });
            return promise;
        },

        urlForQuery: function urlForQuery(query, modelName) {
            if (modelName.indexOf('.') > -1) {
                // Deal with nested model names, like 'cluster.bucket_types'
                modelName = modelName.split('.').pop();
            }
            var url = [];
            // For the moment, assume we're only dealing with cluster-based resources
            url.push(this._buildURL('cluster', query.clusterId));

            url.push(this.pathForType('bucket-type'));
            if (query.bucketTypeId) {
                url.push(query.bucketTypeId);
            }

            return url.join('/');
        }
    });
    exports['default'] = ExplorerResourceAdapter;

});
define('ember-riak-explorer/app', ['exports', 'ember', 'ember/resolver', 'ember/load-initializers', 'ember-riak-explorer/config/environment'], function (exports, Ember, Resolver, loadInitializers, config) {

  'use strict';

  var App;

  Ember['default'].MODEL_FACTORY_INJECTIONS = true;

  App = Ember['default'].Application.extend({
    modulePrefix: config['default'].modulePrefix,
    podModulePrefix: config['default'].podModulePrefix,
    Resolver: Resolver['default']
  });
  Ember['default'].deprecate = function () {};

  loadInitializers['default'](App, config['default'].modulePrefix);

  exports['default'] = App;

});
define('ember-riak-explorer/components/bs-alert', ['exports', 'ember', 'ember-bootstrap/components/bs-alert'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-button-group', ['exports', 'ember', 'ember-bootstrap/components/bs-button-group'], function (exports, Ember, bsButtonGroup) {

	'use strict';

	exports['default'] = bsButtonGroup['default'];

});
define('ember-riak-explorer/components/bs-button', ['exports', 'ember', 'ember-bootstrap/components/bs-button'], function (exports, Ember, bsButton) {

	'use strict';

	exports['default'] = bsButton['default'];

});
define('ember-riak-explorer/components/bs-dropdown-button', ['exports', 'ember', 'ember-bootstrap/components/bs-dropdown-button'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-dropdown-menu', ['exports', 'ember', 'ember-bootstrap/components/bs-dropdown-menu'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-dropdown-toggle', ['exports', 'ember', 'ember-bootstrap/components/bs-dropdown-toggle'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-dropdown', ['exports', 'ember', 'ember-bootstrap/components/bs-dropdown'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-form-element', ['exports', 'ember', 'ember-bootstrap/components/bs-form-element'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-form-group', ['exports', 'ember', 'ember-bootstrap/components/bs-form-group'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-form', ['exports', 'ember', 'ember-bootstrap/components/bs-form'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-input', ['exports', 'ember', 'ember-bootstrap/components/bs-input'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-select', ['exports', 'ember', 'ember-bootstrap/components/bs-select'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bs-textarea', ['exports', 'ember', 'ember-bootstrap/components/bs-textarea'], function (exports, Ember, component) {

	'use strict';

	exports['default'] = component['default'];

});
define('ember-riak-explorer/components/bucket-properties-list', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/bucket-properties-overview', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/bucket-properties', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/bucket-types', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/crumb-trail', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/delete-object', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        actions: {
            deleteObject: function deleteObject(object) {
                // Send its primary action to riak-object controller
                this.sendAction('action', object);
            }
        }
    });

});
define('ember-riak-explorer/components/edit-object', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/em-tab-list', ['exports', 'ember-idx-tabs/tab-list'], function (exports, TabListComponent) {

	'use strict';

	exports['default'] = TabListComponent['default'];

});
define('ember-riak-explorer/components/em-tab-panel', ['exports', 'ember-idx-tabs/tab-panel'], function (exports, TabPanelComponent) {

	'use strict';

	exports['default'] = TabPanelComponent['default'];

});
define('ember-riak-explorer/components/em-tab', ['exports', 'ember-idx-tabs/tab'], function (exports, TabComponent) {

	'use strict';

	exports['default'] = TabComponent['default'];

});
define('ember-riak-explorer/components/em-tabs', ['exports', 'ember-idx-tabs/tabs'], function (exports, TabsComponent) {

	'use strict';

	exports['default'] = TabsComponent['default'];

});
define('ember-riak-explorer/components/link-bucket-type', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/link-bucket', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/link-cluster', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/loading-spinner', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/nav-cluster-link', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'li'
    });

});
define('ember-riak-explorer/components/object-headers-edit', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/object-headers', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/object-location', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        actions: {
            editCancel: function editCancel(object) {
                // Send its primary action to parent controller
                this.sendAction('action', object);
            }
        }
    });

});
define('ember-riak-explorer/components/object-version', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/refresh-buckets', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        actions: {
            refreshBuckets: function refreshBuckets(bucketList) {
                // Send its primary action to parent controller
                this.sendAction('action', bucketList);
            }
        }
    });

});
define('ember-riak-explorer/components/refresh-keys', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        actions: {
            refreshKeys: function refreshKeys(keyList) {
                // Send its primary action to parent controller
                this.sendAction('action', keyList);
            }
        }
    });

});
define('ember-riak-explorer/components/riak-buckets', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        actions: {
            deleteBucket: function deleteBucket(bucket) {
                // Send the action to parent controller
                this.sendAction('deleteBucketAction', bucket);
            }
        }
    });

});
define('ember-riak-explorer/components/riak-keys', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/riak-node', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'tr'
    });

});
define('ember-riak-explorer/components/search-indexes', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/controllers/array', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Controller;

});
define('ember-riak-explorer/controllers/error/cluster-not-found', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['cluster_id'],
        cluster_id: null
    });

});
define('ember-riak-explorer/controllers/error/object-not-found', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id', 'object_key'],
        cluster_id: null,
        bucket_type_id: null,
        bucket_id: null,
        object_key: null
    });

});
define('ember-riak-explorer/controllers/error/unknown', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Controller.extend({});

});
define('ember-riak-explorer/controllers/explorer-api', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        pageTitle: 'Riak Explorer API'
    });

});
define('ember-riak-explorer/controllers/node-stats', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['node_id'],
        node_id: null
    });

});
define('ember-riak-explorer/controllers/object', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Controller;

});
define('ember-riak-explorer/controllers/riak-ping', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['node_id'],
        node_id: null
    });

});
define('ember-riak-explorer/initialize', ['exports', 'ember', 'ember-idx-utils/config'], function (exports, Em, IdxConfig) {

  'use strict';

  exports['default'] = {
    name: 'ember-idx-utils',
    initialize: function initialize() {
      if (!Em['default'].IdxConfig) {
        Em['default'].IdxConfig = IdxConfig['default'].create();
      }
    }
  };

});
define('ember-riak-explorer/initializers/explorer', ['exports'], function (exports) {

    'use strict';

    exports.initialize = initialize;

    function initialize(container, app) {
        app.inject('route', 'explorer', 'service:explorer');
    }

    exports['default'] = {
        name: 'explorer',
        initialize: initialize
    };

});
define('ember-riak-explorer/initializers/export-application-global', ['exports', 'ember', 'ember-riak-explorer/config/environment'], function (exports, Ember, config) {

  'use strict';

  exports.initialize = initialize;

  function initialize(container, application) {
    if (config['default'].exportApplicationGlobal !== false) {
      var value = config['default'].exportApplicationGlobal;
      var globalName;

      if (typeof value === 'string') {
        globalName = value;
      } else {
        globalName = Ember['default'].String.classify(config['default'].modulePrefix);
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

  ;

  exports['default'] = {
    name: 'export-application-global',

    initialize: initialize
  };

});
define('ember-riak-explorer/initializers/idx-tabs', ['exports', 'ember', 'ember-idx-utils/config'], function (exports, Em, IdxConfig) {

    'use strict';

    exports['default'] = {
        name: 'ember-idx-tabs',
        initialize: function initialize() {
            var Config = Em['default'].IdxConfig = Em['default'].IdxConfig ? Em['default'].IdxConfig : IdxConfig['default'].create();

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
define('ember-riak-explorer/initializers/load-bootstrap-config', ['exports', 'ember-riak-explorer/config/environment', 'ember-bootstrap/config'], function (exports, ENV, Config) {

  'use strict';

  exports.initialize = initialize;

  function initialize() /* container, application */{
    Config['default'].load(ENV['default']['ember-bootstrap'] || {});
  }

  exports['default'] = {
    name: 'load-bootstrap-config',
    initialize: initialize
  };

});
define('ember-riak-explorer/instance-initializers/app-version', ['exports', 'ember-riak-explorer/config/environment', 'ember'], function (exports, config, Ember) {

  'use strict';

  var classify = Ember['default'].String.classify;
  var registered = false;

  exports['default'] = {
    name: 'App Version',
    initialize: function initialize(application) {
      if (!registered) {
        var appName = classify(application.toString());
        Ember['default'].libraries.register(appName, config['default'].APP.version);
        registered = true;
      }
    }
  };

});
define('ember-riak-explorer/models/bucket-list', ['exports', 'ember-data', 'ember-riak-explorer/models/cached-list'], function (exports, DS, CachedList) {

    'use strict';

    exports['default'] = CachedList['default'].extend({
        // List of Bucket model instances
        buckets: DS['default'].attr(null, { defaultValue: [] }),

        // Bucket-type model instance
        bucketType: DS['default'].belongsTo('bucket-type'),

        // Cluster model instance
        cluster: DS['default'].belongsTo('cluster'),

        bucketTypeId: (function () {
            return this.get('bucketType').get('bucketTypeId');
        }).property('bucketType'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('cluster')
    });

});
define('ember-riak-explorer/models/bucket-props', ['exports', 'ember-data', 'ember', 'ember-riak-explorer/utils/riak-util'], function (exports, DS, Ember, objectToArray) {

    'use strict';

    var BucketProps = DS['default'].Model.extend({
        // Raw object from JSON payload
        // {"allow_mult":false, "basic_quorum":false, ... }
        props: DS['default'].attr(),

        propsList: (function () {
            if (!this.get('props')) {
                return [];
            }
            return objectToArray['default'](this.get('props'));
        }).property('props'),

        // Helper functions to access Properties

        // Siblings enabled
        allowMult: (function () {
            return this.get('props').allow_mult;
        }).property('props'),

        dataTypeName: (function () {
            var name;
            if (this.get('isCRDT')) {
                name = this.get('props').datatype;
            }
            if (name) {
                return name.capitalize();
            }
        }).property('props'),

        // Pre-commit or post-commit hooks enabled
        hasCommitHooks: (function () {
            var hasPrecommit = !Ember['default'].isEmpty(this.get('props').precommit);
            var hasPostcommit = !Ember['default'].isEmpty(this.get('props').postcommit);
            if (hasPrecommit || hasPostcommit) {
                return true;
            }
            return false;
        }).property('props'),

        // Bucket Type activated via riak-admin command line
        isActive: (function () {
            return this.get('props').active;
        }).property('props'),

        isCounter: (function () {
            return this.get('dataTypeName') === 'Counter';
        }).property('props'),

        isCRDT: (function () {
            return this.get('props').datatype;
        }).property('props'),

        // Last Write Wins optimization
        isLWW: (function () {
            return this.get('props').last_write_wins;
        }).property('props'),

        isMap: (function () {
            return this.get('dataTypeName') === 'Map';
        }).property('props'),

        // Has a Riak Search index been associated with this bucket type
        isSearchIndexed: (function () {
            return this.get('searchIndexName');
        }).property('props'),

        isSet: (function () {
            return this.get('dataTypeName') === 'Set';
        }).property('props'),

        isStronglyConsistent: (function () {
            return false;
        }).property('props'),

        // Riak 2.1+ feature
        isWriteOnce: (function () {
            return this.get('props').write_once;
        }).property('props'),

        nVal: (function () {
            return this.get('props').n_val;
        }).property('props'),

        // What conflict resolution strategy this bucket type uses
        resolutionStrategy: (function () {
            if (this.get('isStronglyConsistent')) {
                return 'Strongly Consistent';
            }
            // if(this.get('isCRDT')) {
            if (this.get('isCounter')) {
                return 'Convergent, Pairwise Maximum Wins';
            }
            if (this.get('isMap')) {
                return 'Convergent, Add/Update Wins Over Remove';
            }
            if (this.get('isSet')) {
                return 'Convergent, Add Wins Over Remove';
            }
            // }
            if (this.get('allowMult')) {
                return 'Causal Context (Siblings Enabled)';
            }
            if (this.get('isWriteOnce')) {
                return 'n/a (Write-Once Optimized)';
            }
            // Last Write Wins optimization enabled
            if (this.get('isLWW')) {
                return 'Wall Clock (LastWriteWins enabled)';
            }

            // Default, regular riak object, allow_mult = false
            return 'Causal Context (Siblings Off, fallback to Wall Clock)';
        }).property('props'),

        // What type of objects are stored (default, search indexed, CRDTs)
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

        // Whether or not the notion of Eventual Consistency / Quorums applies
        // (meaning, if it's not Strongly consistent)
        quorumRelevant: (function () {
            return !this.get('isStronglyConsistent');
        }).property('props'),

        searchIndexName: (function () {
            return this.get('props').search_index;
        }).property('props'),

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
            if (this.get('allowMult')) {
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
define('ember-riak-explorer/models/cached-list', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({

        // Number of items displayed on the current page
        count: DS['default'].attr('number', { defaultValue: 0 }),

        // When was the cache created on the server side
        created: DS['default'].attr(),

        // Is the List operation waiting for a cache to be generated?
        isLoaded: DS['default'].attr('boolean', { defaultValue: false }),

        // Total number of items in the list
        total: DS['default'].attr('total', { defaultValue: 0 })
    });

});
define('ember-riak-explorer/models/key-list', ['exports', 'ember-data', 'ember-riak-explorer/models/cached-list'], function (exports, DS, CachedList) {

    'use strict';

    exports['default'] = CachedList['default'].extend({
        bucket: DS['default'].attr(),
        cluster: DS['default'].attr(),

        // List of riak-object model instances
        keys: DS['default'].attr(null, { defaultValue: [] }),

        bucketId: (function () {
            return this.get('bucket').get('bucketId');
        }).property('bucket'),

        bucketTypeId: (function () {
            return this.get('bucket').get('bucketTypeId');
        }).property('bucket'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('cluster'),

        hasKeys: (function () {
            return this.get('count') > 0;
        }).property('count'),

        showDeleteKeys: (function () {
            return this.get('cluster').get('developmentMode') && this.get('hasKeys');
        }).property('cluster', 'count')
    });

});
define('ember-riak-explorer/models/link', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        self: DS['default'].attr(),
        related: DS['default'].attr()
    });

});
define('ember-riak-explorer/models/route', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        links: DS['default'].attr(),
        resources: DS['default'].attr()
    });

});
define('ember-riak-explorer/pods/bucket-type/controller', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var BucketTypeController = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

        // delay in milliseconds
        pollForModel: function pollForModel(bucketType, delay) {
            var self = this;
            Ember['default'].run.later(function () {
                // console.log('controller: scheduling to refreshModel');
                self.refreshModel(bucketType);
            }, delay);
        },

        refreshModel: function refreshModel(bucketType) {
            var self = this;
            // console.log("Refreshing model %O", bucketType);
            self.get('explorer').getBucketList(bucketType.get('cluster'), bucketType, self.store).then(function (updatedBucketList) {
                // console.log('loaded bucket list: %O', updatedBucketList);
                var model = self.get('model');
                model.set('bucketList', updatedBucketList);
                if (!model.get('isBucketListLoaded')) {
                    self.pollForModel(model, 3000);
                }
            });
        },

        actions: {
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
define('ember-riak-explorer/pods/bucket-type/model', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    var BucketType = DS['default'].Model.extend({
        cluster: DS['default'].belongsTo('cluster'),

        bucketList: DS['default'].belongsTo('bucket-list'),

        isBucketListLoaded: DS['default'].attr('boolean', { defaultValue: false }),

        bucketTypeId: (function () {
            return this.get('originalId');
        }).property('originalId'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('cluster'),

        index: (function () {
            return this.get('cluster').get('indexes').findBy('name', this.get('props').get('searchIndexName'));
        }).property('cluster'),

        isActive: (function () {
            return this.get('props').get('isActive');
        }).property('props'),

        isInactive: (function () {
            return !this.get('props').get('isActive');
        }).property('props'),

        name: (function () {
            return this.get('id');
        }).property('id'),

        originalId: DS['default'].attr('string'),

        // {"allow_mult":false, "basic_quorum":false, ... }
        props: DS['default'].belongsTo('bucket-props')
    });

    exports['default'] = BucketType;

});
define('ember-riak-explorer/pods/bucket-type/route', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        model: function model(params) {
            var clusterId = params.clusterId;
            var bucketTypeId = params.bucketTypeId;
            var explorer = this.explorer;
            var store = this.store;

            return this.explorer.getBucketType(clusterId, bucketTypeId, store).then(function (bucketType) {
                return explorer.getBucketTypeWithBucketList(bucketType, bucketType.get('cluster'), store);
            });
        },

        setupController: function setupController(controller, model) {
            this._super(controller, model);

            if (!model.get('isBucketListLoaded')) {
                controller.pollForModel(model, 3000);
            }
        }
    });

});
define('ember-riak-explorer/pods/bucket-type/template', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 12,
              "column": 0
            },
            "end": {
              "line": 14,
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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["inline","bucket-properties",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[13,30],[13,35]]]]],[],[]],"title","Bucket Type"],["loc",[null,[13,4],[13,57]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 0
            },
            "end": {
              "line": 16,
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
          var el1 = dom.createTextNode("    Properties not loaded.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 26,
                "column": 12
              },
              "end": {
                "line": 28,
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
            var el1 = dom.createTextNode("                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","refresh-buckets",[],["action","refreshBuckets","bucketType",["subexpr","@mut",[["get","model",["loc",[null,[27,69],[27,74]]]]],[],[]]],["loc",[null,[27,16],[27,76]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      var child1 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
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
            "moduleName": "ember-riak-explorer/pods/bucket-type/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 21,
              "column": 0
            },
            "end": {
              "line": 46,
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
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","margin-top: 2em;");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","row");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          var el4 = dom.createTextNode("\n            Cache created: ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("            ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","margin-top: 2em;");
          var el2 = dom.createTextNode("\n        Displaying\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("strong");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        buckets out of\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("strong");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        total.\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","padding-top: 2em;");
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1, 1, 1]);
          var element1 = dom.childAt(fragment, [3]);
          var morphs = new Array(5);
          morphs[0] = dom.createMorphAt(element0,1,1);
          morphs[1] = dom.createMorphAt(element0,3,3);
          morphs[2] = dom.createMorphAt(dom.childAt(element1, [1]),0,0);
          morphs[3] = dom.createMorphAt(dom.childAt(element1, [3]),0,0);
          morphs[4] = dom.createMorphAt(dom.childAt(fragment, [5]),1,1);
          return morphs;
        },
        statements: [
          ["content","model.bucketList.created",["loc",[null,[25,27],[25,55]]]],
          ["block","if",[["get","model.cluster.developmentMode",["loc",[null,[26,18],[26,47]]]]],[],0,null,["loc",[null,[26,12],[28,19]]]],
          ["content","model.bucketList.count",["loc",[null,[35,16],[35,42]]]],
          ["content","model.bucketList.total",["loc",[null,[37,16],[37,42]]]],
          ["block","riak-buckets",[],["bucketList",["subexpr","@mut",[["get","model.bucketList",["loc",[null,[42,35],[42,51]]]]],[],[]],"cluster",["subexpr","@mut",[["get","model.cluster",["loc",[null,[42,60],[42,73]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[43,25],[43,43]]]]],[],[]]],1,null,["loc",[null,[42,8],[44,25]]]]
        ],
        locals: [],
        templates: [child0, child1]
      };
    }());
    var child3 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 46,
              "column": 0
            },
            "end": {
              "line": 48,
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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["content","loading-spinner",["loc",[null,[47,4],[47,23]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 49,
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
        dom.setAttribute(el1,"class","crumb-trail");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","cluster-resource-header center-block");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("span");
        dom.setAttribute(el3,"class","glyphicon glyphicon-inbox cluster-resource-icon");
        dom.setAttribute(el3,"aria-hidden","true");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("span");
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container bucket-properties center-block");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("h4");
        var el2 = dom.createTextNode("Bucket List");
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
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(element2,1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element2, [3, 3]),0,0);
        morphs[2] = dom.createMorphAt(dom.childAt(fragment, [2]),1,1);
        morphs[3] = dom.createMorphAt(fragment,6,6,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","link-cluster",[],["cluster",["subexpr","@mut",[["get","model.cluster",["loc",[null,[2,27],[2,40]]]]],[],[]]],["loc",[null,[2,4],[2,42]]]],
        ["content","model.bucketTypeId",["loc",[null,[7,14],[7,36]]]],
        ["block","if",[["get","model.props",["loc",[null,[12,6],[12,17]]]]],[],0,1,["loc",[null,[12,0],[16,7]]]],
        ["block","if",[["get","model.isBucketListLoaded",["loc",[null,[21,6],[21,30]]]]],[],2,3,["loc",[null,[21,0],[48,7]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/pods/bucket/controller', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var BucketController = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

        // delay in milliseconds
        pollForModel: function pollForModel(bucket, delay) {
            var self = this;
            Ember['default'].run.later(function () {
                self.refreshModel(bucket);
            }, delay);
        },

        refreshModel: function refreshModel(bucket) {
            var self = this;
            self.get('explorer').getKeyList(bucket, self.store).then(function (updatedKeyList) {
                bucket.set('keyList', updatedKeyList);
                if (!bucket.get('isKeyListLoaded')) {
                    self.pollForModel(bucket, 3000);
                }
            });
        },

        actions: {
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
define('ember-riak-explorer/pods/bucket/model', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    var Bucket = DS['default'].Model.extend({
        cluster: DS['default'].belongsTo('cluster'),

        bucketType: DS['default'].belongsTo('bucket-type'),

        isKeyListLoaded: DS['default'].attr('boolean', { defaultValue: false }),

        keyList: DS['default'].belongsTo('key-list'),

        name: DS['default'].attr('string'),

        props: DS['default'].belongsTo('bucket-props'),

        bucketId: (function () {
            return this.get('name');
        }).property('name'),

        bucketTypeId: (function () {
            return this.get('bucketType').get('bucketTypeId');
        }).property('cluster'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('cluster'),

        isActive: (function () {
            return this.get('props').get('isActive');
        }).property('props'),

        index: (function () {
            return this.get('cluster').get('indexes').findBy('name', this.get('props').get('searchIndexName'));
        }).property('cluster'),

        propsList: (function () {
            if (!this.get('props')) {
                return [];
            }
            return this.get('props').get('propsList');
        }).property('props')
    });

    exports['default'] = Bucket;

});
define('ember-riak-explorer/pods/bucket/route', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        model: function model(params) {
            var explorer = this.explorer;
            var store = this.store;
            return explorer.getBucket(params.clusterId, params.bucketTypeId, params.bucketId, store).then(function (bucket) {
                return explorer.getBucketWithKeyList(bucket, store);
            });
        },

        setupController: function setupController(controller, model) {
            this._super(controller, model);
            // When user follows a bucket link from the Bucket Type view,
            //   the props are not yet initialized. Also, the model()
            //   function, above, is not called. Handle this case.
            if (Ember['default'].isEmpty(model.get('props'))) {
                this.explorer.getBucketProps(model.get('clusterId'), model.get('bucketTypeId'), model.get('bucketId'), this.store).then(function (bucketProps) {
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
define('ember-riak-explorer/pods/bucket/template', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 0
            },
            "end": {
              "line": 16,
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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["inline","bucket-properties",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[15,30],[15,35]]]]],[],[]],"title","Bucket"],["loc",[null,[15,4],[15,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 16,
              "column": 0
            },
            "end": {
              "line": 18,
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
          var el1 = dom.createTextNode("    Properties not loaded.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 28,
                "column": 12
              },
              "end": {
                "line": 30,
                "column": 12
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
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","refresh-keys",[],["action","refreshKeys","bucket",["subexpr","@mut",[["get","model",["loc",[null,[29,59],[29,64]]]]],[],[]]],["loc",[null,[29,16],[29,66]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      var child1 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 41,
                "column": 8
              },
              "end": {
                "line": 46,
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
            var el1 = dom.createTextNode("            ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("button");
            dom.setAttribute(el1,"type","button");
            dom.setAttribute(el1,"class","btn btn-xs btn-danger");
            var el2 = dom.createTextNode("\n                ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("span");
            dom.setAttribute(el2,"class","glyphicon glyphicon-trash");
            dom.setAttribute(el2,"aria-hidden","true");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n                Delete Listed Objects");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(1);
            morphs[0] = dom.createElementMorph(element0);
            return morphs;
          },
          statements: [
            ["element","action",["deleteBucket",["get","model",["loc",[null,[43,44],[43,49]]]]],[],["loc",[null,[43,20],[43,51]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      var child2 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 51,
                "column": 8
              },
              "end": {
                "line": 51,
                "column": 74
              }
            },
            "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 23,
              "column": 0
            },
            "end": {
              "line": 53,
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
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","margin-top: 2em;");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","row");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          var el4 = dom.createTextNode("\n            Cache created: ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("            ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","margin-top: 2em;");
          var el2 = dom.createTextNode("\n        Displaying\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("strong");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        keys out of\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("strong");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        total.\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","padding-top: 2em;");
          var el2 = dom.createTextNode("\n        ");
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
          var element1 = dom.childAt(fragment, [1, 1, 1]);
          var element2 = dom.childAt(fragment, [3]);
          var morphs = new Array(6);
          morphs[0] = dom.createMorphAt(element1,1,1);
          morphs[1] = dom.createMorphAt(element1,3,3);
          morphs[2] = dom.createMorphAt(dom.childAt(element2, [1]),0,0);
          morphs[3] = dom.createMorphAt(dom.childAt(element2, [3]),0,0);
          morphs[4] = dom.createMorphAt(element2,5,5);
          morphs[5] = dom.createMorphAt(dom.childAt(fragment, [5]),1,1);
          return morphs;
        },
        statements: [
          ["content","model.keyList.created",["loc",[null,[27,27],[27,52]]]],
          ["block","if",[["get","model.cluster.developmentMode",["loc",[null,[28,18],[28,47]]]]],[],0,null,["loc",[null,[28,12],[30,19]]]],
          ["content","model.keyList.count",["loc",[null,[37,16],[37,39]]]],
          ["content","model.keyList.total",["loc",[null,[39,16],[39,39]]]],
          ["block","if",[["get","model.keyList.showDeleteKeys",["loc",[null,[41,14],[41,42]]]]],[],1,null,["loc",[null,[41,8],[46,15]]]],
          ["block","riak-keys",[],["deleteObject","deleteObject","keys",["subexpr","@mut",[["get","model.keyList.keys",["loc",[null,[51,54],[51,72]]]]],[],[]]],2,null,["loc",[null,[51,8],[51,88]]]]
        ],
        locals: [],
        templates: [child0, child1, child2]
      };
    }());
    var child3 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 53,
              "column": 0
            },
            "end": {
              "line": 55,
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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["content","loading-spinner",["loc",[null,[54,4],[54,23]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 56,
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
        dom.setAttribute(el1,"class","crumb-trail");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","cluster-resource-header center-block");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("span");
        dom.setAttribute(el3,"class","glyphicon glyphicon-folder-close cluster-resource-icon");
        dom.setAttribute(el3,"aria-hidden","true");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("span");
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container bucket-properties center-block");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("h4");
        var el2 = dom.createTextNode("Key List");
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
        var morphs = new Array(5);
        morphs[0] = dom.createMorphAt(element3,1,1);
        morphs[1] = dom.createMorphAt(element3,3,3);
        morphs[2] = dom.createMorphAt(dom.childAt(element3, [5, 3]),0,0);
        morphs[3] = dom.createMorphAt(dom.childAt(fragment, [2]),1,1);
        morphs[4] = dom.createMorphAt(fragment,6,6,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","link-cluster",[],["cluster",["subexpr","@mut",[["get","model.cluster",["loc",[null,[2,27],[2,40]]]]],[],[]]],["loc",[null,[2,4],[2,42]]]],
        ["inline","link-bucket-type",[],["bucketType",["subexpr","@mut",[["get","model.bucketType",["loc",[null,[4,34],[4,50]]]]],[],[]]],["loc",[null,[4,4],[4,52]]]],
        ["content","model.bucketId",["loc",[null,[9,14],[9,32]]]],
        ["block","if",[["get","model.props",["loc",[null,[14,6],[14,17]]]]],[],0,1,["loc",[null,[14,0],[18,7]]]],
        ["block","if",[["get","model.isKeyListLoaded",["loc",[null,[23,6],[23,27]]]]],[],2,3,["loc",[null,[23,0],[55,7]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/pods/cluster/model', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    var Cluster = DS['default'].Model.extend({
        activeBucketTypes: (function () {
            return this.get('bucketTypes').filterBy('isActive');
        }).property('bucketTypes'),

        // Bucket types created on the cluster
        bucketTypes: DS['default'].hasMany('bucket-type'),

        clusterId: (function () {
            return this.get('id');
        }).property('id'),

        // Riak node through which Explorer connects to the riak cluster
        riakNode: DS['default'].attr('string', { defaultValue: null }), //'riak@127.0.0.1'

        // Is this cluster in Dev Mode? Set in the Explorer config file
        // Dev mode allows expensive operations like list keys, delete bucket, etc
        developmentMode: DS['default'].attr('boolean', { defaultValue: false }),

        inactiveBucketTypes: (function () {
            return this.get('bucketTypes').filterBy('isInactive');
        }).property('bucketTypes'),

        // (Solr) Search Indexes in the cluster
        indexes: DS['default'].attr(),

        // Nodes belonging to the cluster
        nodes: DS['default'].attr(),

        productionMode: (function () {
            return !this.get('developmentMode');
        }).property('developmentMode'),

        // URL which Explorer uses to forward requests to the Riak cluster
        // Currently in the form of /riak/clusters/$clusterId
        proxyUrl: (function () {
            return '/riak/clusters/' + this.get('id');
        }).property('id')
    });

    exports['default'] = Cluster;

});
define('ember-riak-explorer/pods/cluster/route', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
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
            return this.store.findRecord('cluster', params.cluster_id);
        },

        setupController: function setupController(controller, model) {
            this._super(controller, model);
            var clusterId = model.get('id');
            this.explorer.getIndexes(clusterId).then(function (indexes) {
                model.set('indexes', indexes);
            });
            this.explorer.getNodes(clusterId).then(function (nodes) {
                model.set('nodes', nodes);
            });
            this.store.query('bucket-type', { clusterId: clusterId }).then(function (bucketTypes) {
                model.set('bucketTypes', bucketTypes);
            });
        }
    });

});
define('ember-riak-explorer/pods/cluster/template', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 20
            },
            "end": {
              "line": 8,
              "column": 54
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
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
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 22,
                "column": 16
              },
              "end": {
                "line": 23,
                "column": 57
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 21,
              "column": 12
            },
            "end": {
              "line": 24,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["block","bucket-types",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[22,42],[22,57]]]]],[],[]],"bucketTypes",["subexpr","@mut",[["get","model.activeBucketTypes",["loc",[null,[23,32],[23,55]]]]],[],[]]],0,null,["loc",[null,[22,16],[23,74]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 24,
              "column": 12
            },
            "end": {
              "line": 26,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("            ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          var el2 = dom.createTextNode("No bucket types have been activated.");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child3 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 34,
                "column": 16
              },
              "end": {
                "line": 35,
                "column": 59
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 29,
              "column": 4
            },
            "end": {
              "line": 38,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-12");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("h5");
          var el4 = dom.createTextNode("Inactive Bucket Types");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1]),3,3);
          return morphs;
        },
        statements: [
          ["block","bucket-types",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[34,42],[34,57]]]]],[],[]],"bucketTypes",["subexpr","@mut",[["get","model.inactiveBucketTypes",["loc",[null,[35,32],[35,57]]]]],[],[]]],0,null,["loc",[null,[34,16],[35,76]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    var child4 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 46,
                "column": 16
              },
              "end": {
                "line": 48,
                "column": 16
              }
            },
            "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 44,
              "column": 12
            },
            "end": {
              "line": 49,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("h5");
          var el2 = dom.createTextNode("Search Indexes");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","search-indexes",[],["indexes",["subexpr","@mut",[["get","model.indexes",["loc",[null,[46,42],[46,55]]]]],[],[]],"clusterProxyUrl",["subexpr","@mut",[["get","model.proxyUrl",["loc",[null,[47,36],[47,50]]]]],[],[]]],0,null,["loc",[null,[46,16],[48,35]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    var child5 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 49,
              "column": 12
            },
            "end": {
              "line": 51,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                No search indexes found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child6 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 66,
              "column": 4
            },
            "end": {
              "line": 68,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
        },
        arity: 1,
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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["inline","riak-node",[],["node",["subexpr","@mut",[["get","node",["loc",[null,[67,23],[67,27]]]]],[],[]]],["loc",[null,[67,6],[67,29]]]]
        ],
        locals: ["node"],
        templates: []
      };
    }());
    var child7 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 68,
              "column": 4
            },
            "end": {
              "line": 70,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("      No nodes detected.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 76,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12 text-center");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","cluster-resource-header center-block");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("img");
        dom.setAttribute(el5,"src","assets/images/riak.png");
        dom.setAttribute(el5,"class","cluster-resource-icon");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        var el6 = dom.createTextNode("\n                    ");
        dom.appendChild(el5, el6);
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode("\n                    ");
        dom.appendChild(el5, el6);
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode("\n                ");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("h4");
        var el5 = dom.createTextNode("Explore");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("h5");
        var el5 = dom.createTextNode("Active Bucket Types");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createElement("h4");
        var el5 = dom.createTextNode("Search");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("h4");
        var el5 = dom.createTextNode("Nodes");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("table");
        dom.setAttribute(el4,"class","table");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("thead");
        var el6 = dom.createTextNode("\n            ");
        dom.appendChild(el5, el6);
        var el6 = dom.createElement("tr");
        var el7 = dom.createTextNode("\n                ");
        dom.appendChild(el6, el7);
        var el7 = dom.createElement("th");
        var el8 = dom.createTextNode("Ping");
        dom.appendChild(el7, el8);
        dom.appendChild(el6, el7);
        var el7 = dom.createTextNode("\n                ");
        dom.appendChild(el6, el7);
        var el7 = dom.createElement("th");
        var el8 = dom.createTextNode("Stats");
        dom.appendChild(el7, el8);
        dom.appendChild(el6, el7);
        var el7 = dom.createTextNode("\n                ");
        dom.appendChild(el6, el7);
        var el7 = dom.createElement("th");
        var el8 = dom.createTextNode("Node");
        dom.appendChild(el7, el8);
        dom.appendChild(el6, el7);
        var el7 = dom.createTextNode("\n            ");
        dom.appendChild(el6, el7);
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode("\n        ");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("tbody");
        var el6 = dom.createTextNode("\n");
        dom.appendChild(el5, el6);
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode("        ");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n    ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment(" <div class=\"col-md-12\"> ");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment(" <div class=\"row\"> ");
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
        var element1 = dom.childAt(element0, [1, 1, 1, 3]);
        var morphs = new Array(6);
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element1,3,3);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 1]),3,3);
        morphs[3] = dom.createMorphAt(element0,7,7);
        morphs[4] = dom.createMorphAt(dom.childAt(element0, [11, 1]),1,1);
        morphs[5] = dom.createMorphAt(dom.childAt(element0, [13, 1, 3, 3]),1,1);
        return morphs;
      },
      statements: [
        ["content","model.clusterId",["loc",[null,[7,20],[7,39]]]],
        ["block","if",[["get","model.developmentMode",["loc",[null,[8,26],[8,47]]]]],[],0,null,["loc",[null,[8,20],[8,61]]]],
        ["block","if",[["get","model.activeBucketTypes",["loc",[null,[21,18],[21,41]]]]],[],1,2,["loc",[null,[21,12],[26,19]]]],
        ["block","if",[["get","model.inactiveBucketTypes",["loc",[null,[29,10],[29,35]]]]],[],3,null,["loc",[null,[29,4],[38,11]]]],
        ["block","if",[["get","model.indexes",["loc",[null,[44,18],[44,31]]]]],[],4,5,["loc",[null,[44,12],[51,19]]]],
        ["block","each",[["get","model.nodes",["loc",[null,[66,12],[66,23]]]]],[],6,7,["loc",[null,[66,4],[70,13]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3, child4, child5, child6, child7]
    };
  }()));

});
define('ember-riak-explorer/pods/riak-object/controller', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

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

});
define('ember-riak-explorer/pods/riak-object/edit/controller', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

        actions: {
            saveObject: function saveObject(object) {
                this.get('explorer').saveObject(object);
                this.transitionToRoute('riak-object', object);
            }
        }
    });

});
define('ember-riak-explorer/pods/riak-object/edit/route', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        model: function model(params) {
            var clusterId = params.clusterId;
            var bucketTypeId = params.bucketTypeId;
            var bucketId = params.bucketId;
            var key = params.key;
            var explorer = this.explorer;
            var store = this.store;
            return explorer.getBucket(clusterId, bucketTypeId, bucketId, store).then(function (bucket) {
                return explorer.getRiakObject(clusterId, bucketTypeId, bucket, key, store);
            });
        }
    });

});
define('ember-riak-explorer/pods/riak-object/edit/template', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 1,
              "column": 48
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 7,
              "column": 12
            },
            "end": {
              "line": 8,
              "column": 43
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 13,
              "column": 12
            },
            "end": {
              "line": 14,
              "column": 40
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child3 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 40,
              "column": 0
            },
            "end": {
              "line": 40,
              "column": 32
            }
          },
          "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h4");
        var el3 = dom.createTextNode("Content");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12 form-group");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("label");
        dom.setAttribute(el4,"for","contentType");
        var el5 = dom.createTextNode("Content Type:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("button");
        dom.setAttribute(el2,"type","button");
        dom.setAttribute(el2,"class","btn btn-md btn-primary");
        var el3 = dom.createTextNode("\n        Save");
        dom.appendChild(el2, el3);
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
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [4]);
        var element1 = dom.childAt(fragment, [14, 1]);
        var morphs = new Array(7);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [1, 1]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [3, 1]),1,1);
        morphs[3] = dom.createMorphAt(dom.childAt(fragment, [8, 3, 1]),3,3);
        morphs[4] = dom.createMorphAt(dom.childAt(fragment, [12]),1,1);
        morphs[5] = dom.createElementMorph(element1);
        morphs[6] = dom.createMorphAt(fragment,16,16,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","object-location",[],["object",["subexpr","@mut",[["get","model",["loc",[null,[1,26],[1,31]]]]],[],[]],"isEditing",true],0,null,["loc",[null,[1,0],[1,68]]]],
        ["block","object-headers-edit",[],["headers",["subexpr","@mut",[["get","model.headersIndexes",["loc",[null,[7,43],[7,63]]]]],[],[]],"title","Secondary Indexes"],1,null,["loc",[null,[7,12],[8,67]]]],
        ["block","object-headers-edit",[],["headers",["subexpr","@mut",[["get","model.headersCustom",["loc",[null,[13,43],[13,62]]]]],[],[]],"title","Custom Headers"],2,null,["loc",[null,[13,12],[14,64]]]],
        ["inline","input",[],["value",["subexpr","@mut",[["get","model.contentType",["loc",[null,[25,26],[25,43]]]]],[],[]],"id","contentType","class","form-control"],["loc",[null,[25,12],[25,83]]]],
        ["inline","textarea",[],["value",["subexpr","@mut",[["get","model.contents",["loc",[null,[32,21],[32,35]]]]],[],[]],"autofocus",true,"rows",20,"cols",100],["loc",[null,[32,4],[32,69]]]],
        ["element","action",["saveObject",["get","model",["loc",[null,[36,34],[36,39]]]]],[],["loc",[null,[36,12],[36,41]]]],
        ["block","object-version",[],["object",["subexpr","@mut",[["get","model",["loc",[null,[40,25],[40,30]]]]],[],[]]],3,null,["loc",[null,[40,0],[40,51]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/pods/riak-object/model', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    var RiakObject = DS['default'].Model.extend({

        bucket: DS['default'].belongsTo('bucket'),

        bucketType: DS['default'].belongsTo('bucket-type'),

        cluster: DS['default'].belongsTo('cluster'),

        contents: DS['default'].attr(),

        headers: DS['default'].attr(),

        isLoaded: DS['default'].attr('boolean', { defaultValue: false }),

        key: DS['default'].attr('string'),

        // This object was marked as deleted by Explorer UI,
        //  but may show up in key list cache.
        markedDeleted: DS['default'].attr('boolean', { defaultValue: false }),

        rawUrl: DS['default'].attr('string'),

        bucketId: (function () {
            return this.get('bucket').get('bucketId');
        }).property('bucket'),

        bucketTypeId: (function () {
            return this.get('bucketType').get('bucketTypeId');
        }).property('bucket'),

        causalContext: (function () {
            return this.get('headers').other['x-riak-vclock'];
        }).property('headers'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('bucket'),

        contentType: (function () {
            return this.get('headers').other['content-type'];
        }).property('headers'),

        dateLastModified: (function () {
            return this.get('headers').other['last-modified'];
        }).property('headers'),

        // When this object was loaded from Riak via an HTTP request
        dateLoaded: (function () {
            return this.get('headers').other['date'];
        }).property('headers'),

        etag: (function () {
            return this.get('headers').other['etag'];
        }).property('headers'),

        headersCustom: (function () {
            return this.get('headers').custom;
        }).property('headers'),

        /**
        * Return the necessary headers when saving an object via HTTP PUT
        */
        headersForUpdate: (function () {
            // Start with the causal context
            var headers = {
                'X-Riak-Vclock': this.get('headers').other['x-riak-vclock']
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

        headersIndexes: (function () {
            return this.get('headers').indexes;
        }).property('headers'),

        isDeleted: (function () {
            var deletedOnRiak = false;
            if (this.get('headers')) {
                deletedOnRiak = this.get('headers').other['x-riak-deleted'];
            }
            return this.get('markedDeleted') || deletedOnRiak;
        }).property('markedDeleted', 'headers')
    });

    exports['default'] = RiakObject;

});
define('ember-riak-explorer/pods/riak-object/route', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        actions: {
            error: function error(_error, transition) {
                if (_error && _error.status === 404) {
                    this.transitionTo('error.object-not-found', transition);
                } else {
                    // Unknown error, bubble error event up to routes/application.js
                    return true;
                }
            }
        },

        model: function model(params) {
            var clusterId = params.clusterId;
            var bucketTypeId = params.bucketTypeId;
            var bucketId = params.bucketId;
            var key = params.key;
            var explorer = this.explorer;
            var store = this.store;
            return explorer.getBucket(clusterId, bucketTypeId, bucketId, store).then(function (bucket) {
                return explorer.getRiakObject(clusterId, bucketTypeId, bucket, key, store);
            });
        },

        setupController: function setupController(controller, model) {
            this._super(controller, model);
            if (!model.get('isLoaded')) {
                this.explorer.getRiakObject(model.get('clusterId'), model.get('bucketTypeId'), model.get('bucket'), model.get('key'), this.store).then(function (object) {
                    controller.set('model', object);
                });
            }
        }
    });

});
define('ember-riak-explorer/pods/riak-object/template', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 1,
              "column": 49
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
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 8,
                "column": 16
              },
              "end": {
                "line": 9,
                "column": 47
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
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      var child1 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 14,
                "column": 16
              },
              "end": {
                "line": 15,
                "column": 44
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
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      var child2 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 35,
                "column": 4
              },
              "end": {
                "line": 37,
                "column": 4
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
            var el1 = dom.createElement("pre");
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
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 0]),0,0);
            return morphs;
          },
          statements: [
            ["content","model.contents",["loc",[null,[36,15],[36,33]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      var child3 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 39,
                "column": 4
              },
              "end": {
                "line": 39,
                "column": 36
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
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 0
            },
            "end": {
              "line": 40,
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
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","row");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3,"class","col-md-12");
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n            ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","row");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3,"class","col-md-12");
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n            ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("br");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("h4");
          var el3 = dom.createTextNode("Contents");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","row");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3,"class","col-md-2");
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("a");
          dom.setAttribute(el4,"class","btn btn-xs btn-primary");
          var el5 = dom.createTextNode("\n                    ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("span");
          dom.setAttribute(el5,"class","glyphicon glyphicon-download");
          dom.setAttribute(el5,"aria-hidden","true");
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n                    View raw");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n            ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3,"class","col-md-10");
          var el4 = dom.createTextNode("\n                Content Type: ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("b");
          var el5 = dom.createComment("");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n            ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("br");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(fragment, [5, 3]);
          var element2 = dom.childAt(element1, [1, 1]);
          var morphs = new Array(6);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 1]),1,1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]),1,1);
          morphs[2] = dom.createAttrMorph(element2, 'href');
          morphs[3] = dom.createMorphAt(dom.childAt(element1, [3, 1]),0,0);
          morphs[4] = dom.createMorphAt(fragment,9,9,contextualElement);
          morphs[5] = dom.createMorphAt(fragment,11,11,contextualElement);
          return morphs;
        },
        statements: [
          ["block","object-headers",[],["headers",["subexpr","@mut",[["get","model.headersIndexes",["loc",[null,[8,42],[8,62]]]]],[],[]],"title","Secondary Indexes"],0,null,["loc",[null,[8,16],[9,66]]]],
          ["block","object-headers",[],["headers",["subexpr","@mut",[["get","model.headersCustom",["loc",[null,[14,42],[14,61]]]]],[],[]],"title","Custom Headers"],1,null,["loc",[null,[14,16],[15,63]]]],
          ["attribute","href",["concat",[["get","model.rawUrl",["loc",[null,[25,27],[25,39]]]]]]],
          ["content","model.contentType",["loc",[null,[30,33],[30,54]]]],
          ["block","if",[["get","model.contents",["loc",[null,[35,10],[35,24]]]]],[],2,null,["loc",[null,[35,4],[37,11]]]],
          ["block","object-version",[],["object",["subexpr","@mut",[["get","model",["loc",[null,[39,29],[39,34]]]]],[],[]]],3,null,["loc",[null,[39,4],[39,55]]]]
        ],
        locals: [],
        templates: [child0, child1, child2, child3]
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 40,
              "column": 0
            },
            "end": {
              "line": 42,
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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["content","loading-spinner",["loc",[null,[41,4],[41,23]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 43,
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
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,4,4,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","object-location",[],["object",["subexpr","@mut",[["get","model",["loc",[null,[1,26],[1,31]]]]],[],[]],"isEditing",false],0,null,["loc",[null,[1,0],[1,69]]]],
        ["block","if",[["get","model.isLoaded",["loc",[null,[4,6],[4,20]]]]],[],1,2,["loc",[null,[4,0],[42,7]]]]
      ],
      locals: [],
      templates: [child0, child1, child2]
    };
  }()));

});
define('ember-riak-explorer/pods/search-index/model', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    var SearchIndex = DS['default'].Model.extend({
        cluster: DS['default'].belongsTo('cluster'),

        name: DS['default'].attr('string'),

        // Index's n_val
        nVal: DS['default'].attr('integer', { defaultValue: 3 }),

        // Name of the schema the index is using
        schema: DS['default'].attr('string')
    });
    exports['default'] = SearchIndex;

});
define('ember-riak-explorer/router', ['exports', 'ember', 'ember-riak-explorer/config/environment'], function (exports, Ember, config) {

    'use strict';

    var Router = Ember['default'].Router.extend({
        location: config['default'].locationType
    });

    exports['default'] = Router.map(function () {
        this.route('explorer_api');
        this.route('cluster', { path: '/cluster/:cluster_id' });
        this.route('bucket-type', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId' });
        this.route('bucket', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId' });
        this.route('riak-object', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/keys/:key' }, function () {
            this.route('edit');
        });
        this.route('riak-object.edit', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/keys/:key/edit' });

        this.route('riak_ping');
        this.route('node_stats');
        this.route('error', { path: '/error' }, function () {
            this.route('unknown');
            this.route('cluster-not-found');
            this.route('object-not-found');
        });
    });

});
define('ember-riak-explorer/routes/application', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        actions: {
            error: function error(_error) {
                // An error has occurred that wasn't handled by any route.
                console.log('Unknown error: %O', _error);
                this.transitionTo('errors.unknown');
            }
        },

        // Load the list of available clusters, for the left nav
        model: function model() {
            return this.store.findAll('cluster');
        }
    });

});
define('ember-riak-explorer/routes/error/cluster-not-found', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
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
define('ember-riak-explorer/routes/error/object-not-found', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            cluster_id: {
                refreshModel: true
            },
            bucket_type_id: {
                refreshModel: true
            },
            bucket_id: {
                refreshModel: true
            },
            object_key: {
                refreshModel: true
            }
        },

        model: function model(params) {
            return {
                clusterId: params.cluster_id,
                bucketTypeId: params.bucket_type_id,
                bucketId: params.bucket_id,
                key: params.object_key
            };
        }
    });

});
define('ember-riak-explorer/routes/error/unknown', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Route.extend({});

});
define('ember-riak-explorer/routes/explorer-api', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        model: function model() {
            var serviceName = 'Riak Explorer';
            var pingUrl = '/explore/ping';
            var propsUrl = '/explore/props';

            return new Ember['default'].RSVP.hash({
                service: serviceName,
                pingResult: Ember['default'].$.ajax({ url: pingUrl, dataType: "json" }),
                propsResult: Ember['default'].$.ajax({ url: propsUrl, dataType: "json" }),
                routes: this.store.find('route')
            });
        }
    });

});
define('ember-riak-explorer/routes/index', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Route.extend({});

});
define('ember-riak-explorer/routes/node-stats', ['exports', 'ember', 'ember-riak-explorer/utils/riak-util'], function (exports, Ember, objectToArray) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            node_id: {
                refreshModel: true
            }
        },

        model: function model(params) {
            var propsUrl = '/riak/nodes/' + params.node_id + '/stats';
            var propsResult = Ember['default'].$.ajax(propsUrl, { dataType: "json" });
            return propsResult.then(function (data) {
                var statsArray = objectToArray['default'](data);
                return {
                    node: params.node_id,
                    stats: statsArray
                };
            });
        }
    });

});
define('ember-riak-explorer/routes/riak-ping', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            node_id: {
                refreshModel: true
            }
        },

        model: function model(params) {
            var url = '/riak/nodes/' + params.node_id + '/ping';

            var result = Ember['default'].$.ajax({ url: url }); // returns a Promise obj
            return result.then(
            // Success
            function (data) {
                return {
                    message: 'Available (' + data + ')'
                };
            },
            // Error
            function (error) {
                return {
                    message: 'Unavailable. Error encountered: ' + error.message
                };
            }).fail(function (error) {
                return {
                    message: 'Unavailable. Error encountered: ' + error.message
                };
            });
        }
    });

});
define('ember-riak-explorer/serializers/application', ['exports', 'ember-data', 'ember'], function (exports, DS, Ember) {

    'use strict';

    exports['default'] = DS['default'].RESTSerializer.extend(DS['default'].EmbeddedRecordsMixin, {
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
        isNewSerializerAPI: false,

        /**
         `keyForAttribute` can be used to define rules for how to convert an
         attribute name in your model to a key in your JSON.
          @method keyForAttribute
         @param {String} key
         @param {String} method
         @return {String} normalized key
        */
        keyForAttribute: function keyForAttribute(attr /*, method*/) {
            // Riak and Explorer json uses snake case, like 'development_mode'
            return Ember['default'].String.underscore(attr);
        },
        /**
        This method is used to convert each JSON root key in the payload
        into a modelName that it can use to look up the appropriate model for
        that part of the payload.
        @method modelNameFromPayloadKey
        @param {String} key
        @return {String} the model's modelName
        */
        modelNameFromPayloadKey: function modelNameFromPayloadKey(payloadKey) {
            return this._super(payloadKey);
        }

        /**
        Normalizes a part of the JSON payload returned by
        the server.
        @method normalize
        @param {DS.Model} typeClass
        @param {Object} hash
        @return {Object}
        */
        // normalize: function(typeClass, hash) {
        //   var fields = Ember.get(typeClass, 'fields');
        //   fields.forEach(function(field) {
        //     var payloadField = Ember.String.underscore(field);
        //     if (field === payloadField) { return; }
        //
        //     hash[field] = hash[payloadField];
        //     delete hash[payloadField];
        //   });
        //   return this._super.apply(this, arguments);
        // }
        // keyForAttribute: function(attr, method) {
        //        return Ember.String.underscore(attr);
        // }
    });

});
define('ember-riak-explorer/services/explorer', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    function bucketCacheRefresh(clusterId, bucketTypeId) {
        // For the moment, 'riak_kv' is the only implemented source of
        // cache refresh
        var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/refresh_buckets/source/riak_kv';
        return cacheRefresh(url);
    }

    /**
    * Refresh a key list cache or bucket list cache on the Explorer API side
    */
    function cacheRefresh(url) {
        return new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
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
    }

    function displayContentsForType(headers, contents) {
        var contentType = headers.other['content-type'];
        var displayContents;
        // Determine whether this is browser-displayable contents
        if (contentType.startsWith('text') || contentType.startsWith('application/json') || contentType.startsWith('application/xml') || contentType.startsWith('multipart/mixed')) {
            displayContents = contents;
        }
        return displayContents;
    }

    function deleteBucket(bucket) {
        var url = '/explore/clusters/' + bucket.get('clusterId') + '/bucket_types/' + bucket.get('bucketTypeId') + '/buckets/' + bucket.get('bucketId');

        return new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
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
    }

    function deleteObject(object) {
        var url = getClusterProxyUrl(object.get('clusterId')) + '/types/' + object.get('bucketTypeId') + '/buckets/' + object.get('bucketId') + '/keys/' + object.get('key');

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: "DELETE",
                url: url,
                headers: { 'X-Riak-Vclock': object.get('headers').other['x-riak-vclock'] }
            }).then(function (data, textStatus, jqXHR) {
                resolve(jqXHR.status);
            }, function (jqXHR, textStatus) {
                reject(textStatus);
            });
        });

        return request['catch'](function (error) {
            console.log('Error deleting riak object: %O', error);
        });
    }

    function getClusterProxyUrl(clusterId) {
        return '/riak/clusters/' + clusterId;
    }

    function getIndexes(clusterId) {
        var url = getClusterProxyUrl(clusterId) + '/search/index';

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: "GET",
                url: url
            }).then(
            // Success
            function (data) {
                resolve(data);
            },
            // Error
            function (jqXHR, textStatus) {
                if (jqXHR.status === 404) {
                    // No indexes found, simply return an empty list
                    resolve([]);
                } else {
                    // Some other error
                    reject(textStatus);
                }
            });
        });
        return request;
    }

    function getKeyList(bucket, store) {
        var clusterId = bucket.get('clusterId');
        var bucketTypeId = bucket.get('bucketTypeId');
        var bucketId = bucket.get('bucketId');
        var explorer = this;

        var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets/' + bucketId + '/keys';
        // console.log('Retrieving key list, url: %s', url);

        return new Ember['default'].RSVP.Promise(function (resolve, reject) {
            var ajaxHash = {
                url: url,
                dataType: 'json',
                type: 'GET'
            };
            ajaxHash.success = function (data) {
                // Success, key list returned
                bucket.set('isKeyListLoaded', true);
                resolve(explorer.createKeyList(data, bucket, store));
            };
            ajaxHash.error = function (jqXHR, textStatus) {
                if (jqXHR.status === 404) {
                    // Empty cache (need to kick off a refresh)
                    keyCacheRefresh(clusterId, bucketTypeId, bucketId);
                    // Results in returning an empty (Loading..) key list
                    Ember['default'].run(null, resolve, null);
                } else {
                    // Some other error
                    Ember['default'].run(null, reject, textStatus);
                }
            };
            Ember['default'].$.ajax(ajaxHash);
        });
    }

    function getNodes(clusterId) {
        var url = '/explore/clusters/' + clusterId + '/nodes';

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: "GET",
                url: url
            }).then(
            // Success
            function (data) {
                resolve(data.nodes);
            },
            // Error
            function (jqXHR, textStatus) {
                if (jqXHR.status === 404) {
                    // No nodes found, simply return an empty list
                    resolve([]);
                } else {
                    // Some other error
                    reject(textStatus);
                }
            });
        });
        return request;
    }

    // Fetch the cache of Deleted keys/buckets for a
    //  given cluster and bucket type. Initialize objects whenever missing.
    function deletedCacheFor(clusterId, bucketTypeId) {
        if (!this.deleted.clusters[clusterId]) {
            this.deleted.clusters[clusterId] = { types: {} };
        }
        if (!this.deleted.clusters[clusterId].types[bucketTypeId]) {
            this.deleted.clusters[clusterId].types[bucketTypeId] = { buckets: {} };
        }
        return this.deleted.clusters[clusterId].types[bucketTypeId];
    }

    function markDeletedKey(object) {
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
    }

    function keyCacheRefresh(clusterId, bucketTypeId, bucketId) {
        // For the moment, 'riak_kv' is the only implemented source of
        // cache refresh
        var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets/' + bucketId + '/refresh_keys/source/riak_kv';
        return cacheRefresh(url);
    }

    function saveObject(object) {
        var url = getClusterProxyUrl(object.get('clusterId')) + '/types/' + object.get('bucketTypeId') + '/buckets/' + object.get('bucketId') + '/keys/' + object.get('key');

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: "PUT",
                processData: false,
                contentType: object.get('contentType'),
                url: url,
                headers: object.get('headersForUpdate'),
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
    }

    exports['default'] = Ember['default'].Service.extend({
        name: 'explorer',
        availableIn: ['controllers', 'routes'],

        // Keep track of keys/buckets deleted through the Explorer UI
        deleted: {
            clusters: {}
        },

        bucketCacheRefresh: bucketCacheRefresh,

        compositeId: function compositeId(clusterId, bucketTypeId) {
            return clusterId + '/' + bucketTypeId;
        },

        createBucketList: function createBucketList(data, cluster, bucketType, store) {
            var bucketList = data.buckets.buckets.map(function (bucketName) {
                return store.createRecord('bucket', {
                    name: bucketName,
                    cluster: cluster,
                    bucketType: bucketType
                });
            });
            return store.createRecord('bucket-list', {
                cluster: cluster,
                bucketType: bucketType,
                buckets: bucketList,
                total: data.buckets.total,
                count: data.buckets.count,
                created: data.buckets.created,
                isLoaded: true
            });
        },

        createKeyList: function createKeyList(data, bucket, store) {
            var explorer = this;
            if (!data) {
                return store.createRecord('key-list', {
                    bucket: bucket,
                    cluster: bucket.get('cluster')
                });
            }
            var keyList = data.keys.keys.map(function (key) {
                var obj = store.createRecord('riak-object', {
                    key: key,
                    bucket: bucket,
                    bucketType: bucket.get('bucketType'),
                    cluster: bucket.get('cluster'),
                    headers: explorer.emptyObjectHeaders(),
                    isLoaded: false
                });
                if (explorer.wasObjectDeleted(obj)) {
                    obj.set('markedDeleted', true);
                }
                return obj;
            });
            return store.createRecord('key-list', {
                bucket: bucket,
                cluster: bucket.get('cluster'),
                created: data.keys.created,
                count: data.keys.count,
                keys: keyList,
                total: data.keys.total
            });
        },

        createObjectFromAjax: function createObjectFromAjax(key, bucket, rawHeader, responseText, store, url) {
            var headers = this.parseHeaderString(rawHeader);
            var contents = displayContentsForType(headers, responseText);

            return store.createRecord('riak-object', {
                key: key,
                bucket: bucket,
                bucketType: bucket.get('bucketType'),
                cluster: bucket.get('cluster'),
                headers: headers,
                isLoaded: true,
                contents: contents,
                rawUrl: url
            });
        },

        deletedCacheFor: deletedCacheFor,

        deleteObject: deleteObject,

        deleteBucket: deleteBucket,

        emptyObjectHeaders: function emptyObjectHeaders() {
            return {
                custom: [], // x-riak-meta-*
                indexes: [], // x-riak-index-*
                other: {} // everything else
            };
        },

        getBucket: function getBucket(clusterId, bucketTypeId, bucketId, store) {
            var self = this;
            return self.getBucketType(clusterId, bucketTypeId, store).then(function (bucketType) {
                return self.getBucketProps(clusterId, bucketTypeId, bucketId, store).then(function (bucketProps) {
                    return store.createRecord('bucket', {
                        name: bucketId,
                        bucketType: bucketType,
                        cluster: bucketType.get('cluster'),
                        props: bucketProps
                    });
                });
            });
        },

        getBucketList: function getBucketList(cluster, bucketType, store) {
            console.log('Refreshing buckets for bucketType: %O', bucketType);
            var clusterId = cluster.get('clusterId');
            var bucketTypeId = bucketType.get('bucketTypeId');
            var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets';
            var explorer = this;

            return new Ember['default'].RSVP.Promise(function (resolve, reject) {
                var ajaxHash = {
                    url: url,
                    dataType: 'json',
                    type: 'GET'
                };
                ajaxHash.success = function (data) {
                    // Success, bucket list returned
                    console.log("Found bucket list");
                    bucketType.set('isBucketListLoaded', true);
                    resolve(explorer.createBucketList(data, cluster, bucketType, store));
                };
                ajaxHash.error = function (jqXHR, textStatus) {
                    // Fail (likely a 404, cache not yet created)
                    if (jqXHR.status === 404) {
                        // Kick off a Cache Refresh, and repeat the getBucketList request
                        console.log("kicking off cache refresh...");
                        bucketCacheRefresh(clusterId, bucketTypeId);
                        // Return an empty (Loading..) list. Controller will poll to
                        // refresh it, later
                        var emptyList = store.createRecord('bucket-list', {
                            cluster: cluster,
                            bucketType: bucketType
                        });
                        Ember['default'].run(null, resolve, emptyList);
                    } else {
                        Ember['default'].run(null, reject, textStatus);
                    }
                };

                Ember['default'].$.ajax(ajaxHash);
            });
        },

        getBucketProps: function getBucketProps(clusterId, bucketTypeId, bucketId, store) {
            var propsUrl = this.getClusterProxyUrl(clusterId) + '/types/' + bucketTypeId + '/buckets/' + bucketId + '/props';
            return new Ember['default'].RSVP.Promise(function (resolve, reject) {
                var ajaxHash = {
                    url: propsUrl,
                    dataType: 'json',
                    type: 'GET'
                };
                ajaxHash.success = function (data) {
                    resolve(store.createRecord('bucket-props', data));
                };
                ajaxHash.error = function (jqXHR) {
                    Ember['default'].run(null, reject, jqXHR);
                };
                Ember['default'].$.ajax(ajaxHash);
            });
        },

        getBucketType: function getBucketType(clusterId, bucketTypeId, store) {
            var self = this;
            return self.getCluster(clusterId, store).then(function (cluster) {
                return cluster.get('bucketTypes').findBy('originalId', bucketTypeId);
            });
        },

        getBucketTypeWithBucketList: function getBucketTypeWithBucketList(bucketType, cluster, store) {
            return this.getBucketList(cluster, bucketType, store).then(function (bucketList) {
                bucketType.set('bucketList', bucketList);
                return bucketType;
            });
        },

        getBucketTypesForCluster: function getBucketTypesForCluster(cluster, store) {
            if (Ember['default'].isEmpty(cluster.get('bucketTypes'))) {
                // If this page was accessed directly
                //  (via a bookmark and not from a link), bucket types are likely
                //  to be not loaded yet. Load them.
                return store.query('bucket-type', { clusterId: cluster.get('clusterId') }).then(function (bucketTypes) {
                    cluster.set('bucketTypes', bucketTypes);
                    return bucketTypes;
                });
            } else {
                return cluster.get('bucketTypes');
            }
        },

        getBucketWithKeyList: function getBucketWithKeyList(bucket, store) {
            return this.getKeyList(bucket, store).then(function (keyList) {
                bucket.set('keyList', keyList);
                return bucket;
            });
        },

        getCluster: function getCluster(clusterId, store) {
            var self = this;
            return store.findRecord('cluster', clusterId).then(function (cluster) {
                // Ensure that bucket types are loaded
                self.getBucketTypesForCluster(cluster, store);
                return cluster;
            }).then(function (cluster) {
                return self.getIndexes(clusterId).then(function (indexes) {
                    cluster.set('indexes', indexes);
                    return cluster;
                });
            });
        },

        getClusterProxyUrl: getClusterProxyUrl,

        getIndexes: getIndexes,

        getKeyList: getKeyList,

        // Return all nodes for a particular cluster
        getNodes: getNodes,

        getRiakObject: function getRiakObject(clusterId, bucketTypeId, bucket, key, store) {
            var url = getClusterProxyUrl(clusterId) + '/types/' + bucketTypeId + '/buckets/' + bucket.get('bucketId') + '/keys/' + key;
            var explorer = this;

            return new Ember['default'].RSVP.Promise(function (resolve, reject) {
                var ajaxHash = {
                    type: "GET",
                    processData: false,
                    cache: false,
                    url: url,
                    headers: { 'Accept': '*/*, multipart/mixed' }
                };
                var headerString;
                ajaxHash.success = function (data, textStatus, jqXHR) {
                    headerString = jqXHR.getAllResponseHeaders();
                    resolve(explorer.createObjectFromAjax(key, bucket, headerString, jqXHR.responseText, store, url));
                };
                ajaxHash.error = function (jqXHR, textStatus) {
                    if (jqXHR.status === 200 && textStatus === 'parsererror') {
                        // jQuery tries to parse JSON objects, and throws
                        // parse errors when they're invalid. Suppress this.
                        headerString = jqXHR.getAllResponseHeaders();
                        resolve(explorer.createObjectFromAjax(key, bucket, headerString, jqXHR.responseText, store, url));
                    }
                    if (jqXHR.status === 300) {
                        // Handle 300 Multiple Choices case for siblings
                        headerString = jqXHR.getAllResponseHeaders();
                        resolve(explorer.createObjectFromAjax(key, bucket, headerString, jqXHR.responseText, store, url));
                    } else {
                        reject(jqXHR);
                    }
                };
                Ember['default'].$.ajax(ajaxHash);
            });
        },

        keyCacheRefresh: keyCacheRefresh,

        markDeletedKey: markDeletedKey,

        /**
        * XmlHttpRequest's getAllResponseHeaders() method returns a string of response
        * headers according to the format described here:
        * http://www.w3.org/TR/XMLHttpRequest/#the-getallresponseheaders-method
        *
        * Which we then have to parse. Like savages.
        */
        parseHeaderString: function parseHeaderString(headerString) {
            var other_headers = {};
            var indexes = [];
            var custom = [];

            if (!headerString) {
                return this.emptyObjectHeaders();
            }
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

        saveObject: saveObject,

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
define('ember-riak-explorer/templates/application', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 14,
              "column": 10
            },
            "end": {
              "line": 14,
              "column": 90
            }
          },
          "moduleName": "ember-riak-explorer/templates/application.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("a");
          dom.setAttribute(el1,"href","/explorer_api");
          var el2 = dom.createTextNode("Explorer API");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 24,
              "column": 10
            },
            "end": {
              "line": 25,
              "column": 10
            }
          },
          "moduleName": "ember-riak-explorer/templates/application.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("a");
          dom.setAttribute(el1,"href","/");
          var el2 = dom.createTextNode("Clusters");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/application.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("            ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["inline","nav-cluster-link",[],["cluster",["subexpr","@mut",[["get","cluster",["loc",[null,[28,39],[28,46]]]]],[],[]]],["loc",[null,[28,12],[28,48]]]]
        ],
        locals: ["cluster"],
        templates: []
      };
    }());
    var child3 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 31,
              "column": 10
            },
            "end": {
              "line": 31,
              "column": 82
            }
          },
          "moduleName": "ember-riak-explorer/templates/application.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("a");
          dom.setAttribute(el1,"href","/ping");
          var el2 = dom.createTextNode("Explorer API");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 40,
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
        var el1 = dom.createElement("nav");
        dom.setAttribute(el1,"class","navbar navbar-inverse navbar-fixed-top");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","container-fluid");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","navbar-header");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("button");
        dom.setAttribute(el4,"type","button");
        dom.setAttribute(el4,"class","navbar-toggle collapsed");
        dom.setAttribute(el4,"data-toggle","collapse");
        dom.setAttribute(el4,"data-target","#navbar");
        dom.setAttribute(el4,"aria-expanded","false");
        dom.setAttribute(el4,"aria-controls","navbar");
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5,"class","sr-only");
        var el6 = dom.createTextNode("Toggle navigation");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5,"class","icon-bar");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5,"class","icon-bar");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5,"class","icon-bar");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("a");
        dom.setAttribute(el4,"class","navbar-brand");
        dom.setAttribute(el4,"href","#");
        var el5 = dom.createTextNode("Riak Explorer");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"id","navbar");
        dom.setAttribute(el3,"class","navbar-collapse collapse");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("ul");
        dom.setAttribute(el4,"class","nav navbar-nav navbar-right");
        var el5 = dom.createTextNode("\n          ");
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
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container-fluid");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-sm-3 col-md-2 sidebar");
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("ul");
        dom.setAttribute(el4,"class","nav nav-sidebar");
        var el5 = dom.createTextNode("\n          ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n          ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n        ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main");
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
        var element0 = dom.childAt(fragment, [2, 1]);
        var element1 = dom.childAt(element0, [1, 1]);
        var morphs = new Array(5);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1, 3, 1]),1,1);
        morphs[1] = dom.createMorphAt(element1,1,1);
        morphs[2] = dom.createMorphAt(element1,3,3);
        morphs[3] = dom.createMorphAt(element1,5,5);
        morphs[4] = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
        return morphs;
      },
      statements: [
        ["block","link-to",["explorer_api"],["tagName","li"],0,null,["loc",[null,[14,10],[14,102]]]],
        ["block","link-to",["index"],["tagName","li"],1,null,["loc",[null,[24,10],[25,22]]]],
        ["block","each",[["get","model",["loc",[null,[27,18],[27,23]]]]],[],2,null,["loc",[null,[27,10],[29,19]]]],
        ["block","link-to",["explorer_api"],["tagName","li"],3,null,["loc",[null,[31,10],[31,94]]]],
        ["content","outlet",["loc",[null,[36,6],[36,16]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-alert', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 2,
                "column": 0
              },
              "end": {
                "line": 4,
                "column": 0
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bs-alert.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("button");
            dom.setAttribute(el1,"type","button");
            dom.setAttribute(el1,"class","close");
            dom.setAttribute(el1,"aria-label","Close");
            var el2 = dom.createElement("span");
            dom.setAttribute(el2,"aria-hidden","true");
            var el3 = dom.createTextNode("×");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var element0 = dom.childAt(fragment, [1]);
            var morphs = new Array(1);
            morphs[0] = dom.createElementMorph(element0);
            return morphs;
          },
          statements: [
            ["element","action",["dismiss"],[],["loc",[null,[3,59],[3,79]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/bs-alert.hbs"
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
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
          dom.insertBoundary(fragment, 0);
          return morphs;
        },
        statements: [
          ["block","if",[["get","dismissible",["loc",[null,[2,6],[2,17]]]]],[],0,null,["loc",[null,[2,0],[4,7]]]],
          ["content","yield",["loc",[null,[5,0],[5,9]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/bs-alert.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","unless",[["get","dismissed",["loc",[null,[1,10],[1,19]]]]],[],0,null,["loc",[null,[1,0],[6,11]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-button', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 1,
              "column": 37
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bs-button.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("i");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(" ");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [0]);
          var morphs = new Array(1);
          morphs[0] = dom.createAttrMorph(element0, 'class');
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",[["get","icon",["loc",[null,[1,24],[1,28]]]]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 1,
            "column": 61
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/bs-button.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","icon",["loc",[null,[1,6],[1,10]]]]],[],0,null,["loc",[null,[1,0],[1,44]]]],
        ["content","text",["loc",[null,[1,44],[1,52]]]],
        ["content","yield",["loc",[null,[1,52],[1,61]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-form-group', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 2,
              "column": 0
            },
            "end": {
              "line": 4,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bs-form-group.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var morphs = new Array(1);
          morphs[0] = dom.createAttrMorph(element0, 'class');
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",["form-control-feedback ",["get","iconName",["loc",[null,[3,41],[3,49]]]]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 4,
            "column": 7
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/bs-form-group.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["content","yield",["loc",[null,[1,0],[1,9]]]],
        ["block","if",[["get","hasFeedback",["loc",[null,[2,6],[2,17]]]]],[],0,null,["loc",[null,[2,0],[4,7]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-form', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 1,
            "column": 9
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/bs-form.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["content","yield",["loc",[null,[1,0],[1,9]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/bucket-properties-list', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 9,
              "column": 0
            },
            "end": {
              "line": 14,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties-list.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
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
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          return morphs;
        },
        statements: [
          ["content","prop.key",["loc",[null,[11,16],[11,28]]]],
          ["content","prop.value",["loc",[null,[12,16],[12,30]]]]
        ],
        locals: ["prop"],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        dom.setAttribute(el1,"class","table");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Property");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Value");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
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
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 3]),1,1);
        return morphs;
      },
      statements: [
        ["block","each",[["get","model.props.propsList",["loc",[null,[9,8],[9,29]]]]],[],0,null,["loc",[null,[9,0],[14,9]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bucket-properties-overview', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 13,
              "column": 8
            },
            "end": {
              "line": 19,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties-overview.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("            R: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(", W: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(",\n            PR: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(", PW: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(",");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("br");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n            DW: ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n            ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("small");
          var el2 = dom.createTextNode("(basic_quorum: ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode(",\n                notfound_ok: ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode(")");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element2 = dom.childAt(fragment, [13]);
          var morphs = new Array(7);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
          morphs[2] = dom.createMorphAt(fragment,5,5,contextualElement);
          morphs[3] = dom.createMorphAt(fragment,7,7,contextualElement);
          morphs[4] = dom.createMorphAt(fragment,11,11,contextualElement);
          morphs[5] = dom.createMorphAt(element2,1,1);
          morphs[6] = dom.createMorphAt(element2,3,3);
          return morphs;
        },
        statements: [
          ["content","props.quorum.r",["loc",[null,[14,15],[14,33]]]],
          ["content","props.quorum.w",["loc",[null,[14,38],[14,56]]]],
          ["content","props.quorum.pr",["loc",[null,[15,16],[15,35]]]],
          ["content","props.quorum.pw",["loc",[null,[15,41],[15,60]]]],
          ["content","props.quorum.dw",["loc",[null,[16,16],[16,35]]]],
          ["content","props.quorum.basic_quorum",["loc",[null,[17,34],[17,63]]]],
          ["content","props.quorum.basic_quorum",["loc",[null,[18,29],[18,58]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 23,
              "column": 0
            },
            "end": {
              "line": 39,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties-overview.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row property-row");
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-sm-4");
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createTextNode("Search Index");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("br");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-sm-4");
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createTextNode("Search Schema");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("br");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("a");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-sm-4");
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createTextNode("Index N_Val");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("br");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n");
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
          var element1 = dom.childAt(element0, [3, 4]);
          var morphs = new Array(4);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),4,4);
          morphs[1] = dom.createAttrMorph(element1, 'href');
          morphs[2] = dom.createMorphAt(element1,1,1);
          morphs[3] = dom.createMorphAt(dom.childAt(element0, [5]),4,4);
          return morphs;
        },
        statements: [
          ["content","props.searchIndexName",["loc",[null,[27,4],[27,29]]]],
          ["attribute","href",["concat",[["get","props.cluster.clusterProxyUrl",["loc",[null,[31,15],[31,44]]]],"/search/schema/",["get","props.index.schema",["loc",[null,[31,63],[31,81]]]]]]],
          ["content","model.index.schema",["loc",[null,[32,8],[32,30]]]],
          ["content","model.index.n_val",["loc",[null,[36,4],[36,25]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 39,
              "column": 0
            },
            "end": {
              "line": 48,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties-overview.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row property-row");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-sm-12");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3,"class","disabled");
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("strong");
          var el5 = dom.createTextNode("Search Index");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("br");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        n/a (not being indexed)\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child3 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 55,
                "column": 8
              },
              "end": {
                "line": 57,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties-overview.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("            ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("li");
            var el2 = dom.createElement("strong");
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
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 0]),0,0);
            return morphs;
          },
          statements: [
            ["content","warning",["loc",[null,[56,24],[56,35]]]]
          ],
          locals: ["warning"],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 50,
              "column": 0
            },
            "end": {
              "line": 61,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties-overview.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row property-row");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-sm-12");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          dom.setAttribute(el3,"class","warning");
          var el4 = dom.createTextNode("Warnings");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
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
          var el3 = dom.createTextNode("\n    ");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1, 3]),1,1);
          return morphs;
        },
        statements: [
          ["block","each",[["get","props.warnings",["loc",[null,[55,16],[55,30]]]]],[],0,null,["loc",[null,[55,8],[57,17]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 62,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/bucket-properties-overview.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","row property-row");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","col-sm-4");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("strong");
        var el4 = dom.createTextNode("Object type");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("br");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","col-sm-4");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("strong");
        var el4 = dom.createTextNode("Conflict Res. Strategy");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("br");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","col-sm-4");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("strong");
        var el4 = dom.createTextNode("Quorum");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("br");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        N_Val: ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(" ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("br");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("    ");
        dom.appendChild(el2, el3);
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
        var element3 = dom.childAt(fragment, [0]);
        var element4 = dom.childAt(element3, [5]);
        var morphs = new Array(6);
        morphs[0] = dom.createMorphAt(dom.childAt(element3, [1]),4,4);
        morphs[1] = dom.createMorphAt(dom.childAt(element3, [3]),4,4);
        morphs[2] = dom.createMorphAt(element4,4,4);
        morphs[3] = dom.createMorphAt(element4,8,8);
        morphs[4] = dom.createMorphAt(fragment,2,2,contextualElement);
        morphs[5] = dom.createMorphAt(fragment,4,4,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["content","props.objectType",["loc",[null,[4,8],[4,28]]]],
        ["content","props.resolutionStrategy",["loc",[null,[8,8],[8,36]]]],
        ["content","props.nVal",["loc",[null,[12,15],[12,29]]]],
        ["block","if",[["get","props.quorumRelevant",["loc",[null,[13,14],[13,34]]]]],[],0,null,["loc",[null,[13,8],[19,15]]]],
        ["block","if",[["get","props.isSearchIndexed",["loc",[null,[23,6],[23,27]]]]],[],1,2,["loc",[null,[23,0],[48,7]]]],
        ["block","if",[["get","props.warnings",["loc",[null,[50,6],[50,20]]]]],[],3,null,["loc",[null,[50,0],[61,7]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bucket-properties', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 16
            },
            "end": {
              "line": 11,
              "column": 16
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","glyphicon glyphicon-ok-sign");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","status-ok");
          var el2 = dom.createTextNode("Activated");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 16
            },
            "end": {
              "line": 14,
              "column": 16
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","glyphicon glyphicon-remove-sign");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","status-disabled");
          var el2 = dom.createTextNode("Inactive");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      var child0 = (function() {
        var child0 = (function() {
          return {
            meta: {
              "revision": "Ember@1.13.5",
              "loc": {
                "source": null,
                "start": {
                  "line": 20,
                  "column": 20
                },
                "end": {
                  "line": 20,
                  "column": 39
                }
              },
              "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
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
            buildRenderNodes: function buildRenderNodes() { return []; },
            statements: [

            ],
            locals: [],
            templates: []
          };
        }());
        var child1 = (function() {
          return {
            meta: {
              "revision": "Ember@1.13.5",
              "loc": {
                "source": null,
                "start": {
                  "line": 21,
                  "column": 20
                },
                "end": {
                  "line": 21,
                  "column": 50
                }
              },
              "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
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
            buildRenderNodes: function buildRenderNodes() { return []; },
            statements: [

            ],
            locals: [],
            templates: []
          };
        }());
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 19,
                "column": 16
              },
              "end": {
                "line": 22,
                "column": 16
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
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
            var el1 = dom.createTextNode("\n                    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
            return morphs;
          },
          statements: [
            ["block","em-tab",[],[],0,null,["loc",[null,[20,20],[20,50]]]],
            ["block","em-tab",[],[],1,null,["loc",[null,[21,20],[21,61]]]]
          ],
          locals: [],
          templates: [child0, child1]
        };
      }());
      var child1 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 23,
                "column": 16
              },
              "end": {
                "line": 25,
                "column": 16
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","bucket-properties-overview",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[24,55],[24,60]]]]],[],[]],"props",["subexpr","@mut",[["get","model.props",["loc",[null,[24,67],[24,78]]]]],[],[]]],["loc",[null,[24,20],[24,80]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      var child2 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 26,
                "column": 16
              },
              "end": {
                "line": 28,
                "column": 16
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","bucket-properties-list",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[27,51],[27,56]]]]],[],[]]],["loc",[null,[27,20],[27,58]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 18,
              "column": 12
            },
            "end": {
              "line": 29,
              "column": 12
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
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(3);
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
          morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","em-tab-list",[],[],0,null,["loc",[null,[19,16],[22,32]]]],
          ["block","em-tab-panel",[],[],1,null,["loc",[null,[23,16],[25,33]]]],
          ["block","em-tab-panel",[],[],2,null,["loc",[null,[26,16],[28,33]]]]
        ],
        locals: [],
        templates: [child0, child1, child2]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 34,
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","row");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","col-sm-12");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","row property-header");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","col-sm-10");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("strong");
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        var el6 = dom.createTextNode(" Properties");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","col-sm-2");
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","row");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n\n    ");
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
        var element1 = dom.childAt(element0, [1]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(dom.childAt(element1, [1, 1]),0,0);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [3]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
        return morphs;
      },
      statements: [
        ["content","title",["loc",[null,[5,24],[5,33]]]],
        ["block","if",[["get","model.isActive",["loc",[null,[8,22],[8,36]]]]],[],0,1,["loc",[null,[8,16],[14,23]]]],
        ["block","em-tabs",[],["configName","default"],2,null,["loc",[null,[18,12],[29,24]]]]
      ],
      locals: [],
      templates: [child0, child1, child2]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bucket-types', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 14,
                "column": 12
              },
              "end": {
                "line": 18,
                "column": 35
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-types.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1,"class","glyphicon glyphicon-inbox cluster-resource-icon");
            dom.setAttribute(el1,"aria-hidden","true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [
            ["content","bt.bucketTypeId",["loc",[null,[18,16],[18,35]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 0
            },
            "end": {
              "line": 30,
              "column": 0
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
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
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
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),1,1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5]),1,1);
          morphs[3] = dom.createMorphAt(dom.childAt(element0, [7]),1,1);
          return morphs;
        },
        statements: [
          ["block","link-to",["bucket-type",["get","bt",["loc",[null,[14,37],[14,39]]]]],["classNames","btn btn-sm btn-block btn-primary cluster-resource-link"],0,null,["loc",[null,[14,12],[18,47]]]],
          ["content","bt.props.objectType",["loc",[null,[21,12],[21,35]]]],
          ["content","bt.props.nVal",["loc",[null,[24,12],[24,29]]]],
          ["content","bt.props.resolutionStrategy",["loc",[null,[27,12],[27,43]]]]
        ],
        locals: ["bt"],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 33,
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
        dom.setAttribute(el1,"class","table");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Name");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Object Type");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("n_val");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Conflict Res. Strategy");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 3]),1,1);
        return morphs;
      },
      statements: [
        ["block","each",[["get","bucketTypes",["loc",[null,[11,8],[11,19]]]]],[],0,null,["loc",[null,[11,0],[30,9]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/crumb-trail', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 10,
                "column": 16
              },
              "end": {
                "line": 14,
                "column": 42
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/crumb-trail.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1,"class","glyphicon glyphicon-inbox cluster-resource-icon");
            dom.setAttribute(el1,"aria-hidden","true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n                    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [
            ["content","model.bucketTypeId",["loc",[null,[14,20],[14,42]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 12
            },
            "end": {
              "line": 15,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/crumb-trail.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                /\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["block","link-to",["bucket-type",["get","model.bucketType",["loc",[null,[10,41],[10,57]]]]],["classNames","btn btn-sm btn-primary cluster-resource-link"],0,null,["loc",[null,[10,16],[14,54]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 16,
              "column": 12
            },
            "end": {
              "line": 18,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/crumb-trail.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                /\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/crumb-trail.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-1");
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-11");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
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
        var element0 = dom.childAt(fragment, [0, 1, 3]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(element0,3,3);
        morphs[2] = dom.createMorphAt(element0,4,4);
        return morphs;
      },
      statements: [
        ["inline","link-cluster",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[6,37],[6,52]]]]],[],[]]],["loc",[null,[6,12],[6,54]]]],
        ["block","if",[["get","model.bucketTypeId",["loc",[null,[8,18],[8,36]]]]],[],0,null,["loc",[null,[8,12],[15,19]]]],
        ["block","if",[["get","model.bucketId",["loc",[null,[16,18],[16,32]]]]],[],1,null,["loc",[null,[16,12],[18,19]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/delete-object', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/delete-object.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-xs btn-danger");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2,"class","glyphicon glyphicon-trash");
        dom.setAttribute(el2,"aria-hidden","true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    Delete");
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
      statements: [
        ["element","action",["deleteObject",["get","object",["loc",[null,[2,32],[2,38]]]]],[],["loc",[null,[2,8],[2,40]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/edit-object', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 4,
              "column": 15
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/edit-object.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","glyphicon glyphicon-pencil");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    Edit Object");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/edit-object.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","link-to",["riak-object.edit",["get","object",["loc",[null,[1,30],[1,36]]]]],["classNames","btn btn-xs btn-primary"],0,null,["loc",[null,[1,0],[4,27]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/errors', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/errors.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","help-block");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","errors.firstObject",["loc",[null,[2,29],[2,51]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 3,
            "column": 7
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/errors.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","showErrors",["loc",[null,[1,6],[1,16]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/feedback-icon', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/feedback-icon.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var element0 = dom.childAt(fragment, [1]);
          var morphs = new Array(1);
          morphs[0] = dom.createAttrMorph(element0, 'class');
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",["form-control-feedback ",["get","iconName",["loc",[null,[2,41],[2,49]]]]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 3,
            "column": 7
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/feedback-icon.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasFeedback",["loc",[null,[1,6],[1,17]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/checkbox', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 8,
            "column": 6
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/checkbox.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","checkbox");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("label");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode(" ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var element1 = dom.childAt(element0, [1, 1]);
        var morphs = new Array(4);
        morphs[0] = dom.createAttrMorph(element0, 'class');
        morphs[1] = dom.createMorphAt(element1,1,1);
        morphs[2] = dom.createMorphAt(element1,3,3);
        morphs[3] = dom.createMorphAt(element0,3,3);
        return morphs;
      },
      statements: [
        ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[1,14],[1,38]]]]," ",["get","horizontalInputOffsetGridClass",["loc",[null,[1,43],[1,73]]]]]]],
        ["inline","input",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,25],[4,29]]]]],[],[]],"type","checkbox","checked",["subexpr","@mut",[["get","value",["loc",[null,[4,54],[4,59]]]]],[],[]]],["loc",[null,[4,12],[4,61]]]],
        ["content","label",["loc",[null,[4,62],[4,71]]]],
        ["inline","partial",["components/form-element/errors"],[],["loc",[null,[7,4],[7,48]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/default', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/default.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morphs = new Array(6);
          morphs[0] = dom.createAttrMorph(element1, 'class');
          morphs[1] = dom.createMorphAt(element1,0,0);
          morphs[2] = dom.createAttrMorph(element2, 'class');
          morphs[3] = dom.createMorphAt(element2,1,1);
          morphs[4] = dom.createMorphAt(element2,3,3);
          morphs[5] = dom.createMorphAt(element2,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",["control-label ",["get","horizontalLabelGridClass",["loc",[null,[2,34],[2,58]]]]]]],
          ["content","label",["loc",[null,[2,62],[2,71]]]],
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[3,18],[3,42]]]]]]],
          ["inline","bs-input",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,24],[4,28]]]]],[],[]],"type",["subexpr","@mut",[["get","controlType",["loc",[null,[4,34],[4,45]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,52],[4,57]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[4,70],[4,81]]]]],[],[]]],["loc",[null,[4,8],[4,83]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,8],[5,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,8],[6,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 0
            },
            "end": {
              "line": 14,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/default.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var morphs = new Array(4);
          morphs[0] = dom.createAttrMorph(element0, 'class');
          morphs[1] = dom.createMorphAt(element0,1,1);
          morphs[2] = dom.createMorphAt(element0,3,3);
          morphs[3] = dom.createMorphAt(element0,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[9,18],[9,42]]]]," ",["get","horizontalInputOffsetGridClass",["loc",[null,[9,47],[9,77]]]]]]],
          ["inline","bs-input",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[10,24],[10,28]]]]],[],[]],"type",["subexpr","@mut",[["get","controlType",["loc",[null,[10,34],[10,45]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[10,52],[10,57]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[10,70],[10,81]]]]],[],[]]],["loc",[null,[10,8],[10,83]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[11,8],[11,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[12,8],[12,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/default.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,1,["loc",[null,[1,0],[14,7]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/select', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/select.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morphs = new Array(6);
          morphs[0] = dom.createAttrMorph(element1, 'class');
          morphs[1] = dom.createMorphAt(element1,0,0);
          morphs[2] = dom.createAttrMorph(element2, 'class');
          morphs[3] = dom.createMorphAt(element2,1,1);
          morphs[4] = dom.createMorphAt(element2,3,3);
          morphs[5] = dom.createMorphAt(element2,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",["control-label ",["get","horizontalLabelGridClass",["loc",[null,[2,34],[2,58]]]]]]],
          ["content","label",["loc",[null,[2,62],[2,71]]]],
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[3,18],[3,42]]]]]]],
          ["inline","bs-select",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,25],[4,29]]]]],[],[]],"content",["subexpr","@mut",[["get","choices",["loc",[null,[4,38],[4,45]]]]],[],[]],"optionValuePath",["subexpr","@mut",[["get","selectValueProperty",["loc",[null,[4,62],[4,81]]]]],[],[]],"optionLabelPath",["subexpr","@mut",[["get","selectLabelProperty",["loc",[null,[4,98],[4,117]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,124],[4,129]]]]],[],[]]],["loc",[null,[4,8],[4,131]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,8],[5,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,8],[6,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 0
            },
            "end": {
              "line": 14,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/select.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var morphs = new Array(4);
          morphs[0] = dom.createAttrMorph(element0, 'class');
          morphs[1] = dom.createMorphAt(element0,1,1);
          morphs[2] = dom.createMorphAt(element0,3,3);
          morphs[3] = dom.createMorphAt(element0,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[9,18],[9,42]]]]," ",["get","horizontalInputOffsetGridClass",["loc",[null,[9,47],[9,77]]]]]]],
          ["inline","bs-select",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[10,25],[10,29]]]]],[],[]],"content",["subexpr","@mut",[["get","choices",["loc",[null,[10,38],[10,45]]]]],[],[]],"optionValuePath",["subexpr","@mut",[["get","selectValueProperty",["loc",[null,[10,62],[10,81]]]]],[],[]],"optionLabelPath",["subexpr","@mut",[["get","selectLabelProperty",["loc",[null,[10,98],[10,117]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[10,124],[10,129]]]]],[],[]]],["loc",[null,[10,8],[10,131]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[11,8],[11,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[12,8],[12,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/select.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,1,["loc",[null,[1,0],[14,7]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/select2', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/select2.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morphs = new Array(6);
          morphs[0] = dom.createAttrMorph(element1, 'class');
          morphs[1] = dom.createMorphAt(element1,0,0);
          morphs[2] = dom.createAttrMorph(element2, 'class');
          morphs[3] = dom.createMorphAt(element2,1,1);
          morphs[4] = dom.createMorphAt(element2,3,3);
          morphs[5] = dom.createMorphAt(element2,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",["control-label ",["get","horizontalLabelGridClass",["loc",[null,[2,34],[2,58]]]]]]],
          ["content","label",["loc",[null,[2,62],[2,71]]]],
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[3,18],[3,42]]]]]]],
          ["inline","select-2",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,24],[4,28]]]]],[],[]],"content",["subexpr","@mut",[["get","choices",["loc",[null,[4,37],[4,44]]]]],[],[]],"optionValuePath",["subexpr","@mut",[["get","choiceValueProperty",["loc",[null,[4,61],[4,80]]]]],[],[]],"optionLabelPath",["subexpr","@mut",[["get","choiceLabelProperty",["loc",[null,[4,97],[4,116]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,123],[4,128]]]]],[],[]],"searchEnabled",false],["loc",[null,[4,8],[4,150]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,8],[5,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,8],[6,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 0
            },
            "end": {
              "line": 14,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/select2.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var morphs = new Array(4);
          morphs[0] = dom.createAttrMorph(element0, 'class');
          morphs[1] = dom.createMorphAt(element0,1,1);
          morphs[2] = dom.createMorphAt(element0,3,3);
          morphs[3] = dom.createMorphAt(element0,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[9,18],[9,42]]]]," ",["get","horizontalInputOffsetGridClass",["loc",[null,[9,47],[9,77]]]]]]],
          ["inline","select-2",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[10,24],[10,28]]]]],[],[]],"content",["subexpr","@mut",[["get","choices",["loc",[null,[10,37],[10,44]]]]],[],[]],"optionValuePath",["subexpr","@mut",[["get","choiceValueProperty",["loc",[null,[10,61],[10,80]]]]],[],[]],"optionLabelPath",["subexpr","@mut",[["get","choiceLabelProperty",["loc",[null,[10,97],[10,116]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[10,123],[10,128]]]]],[],[]],"searchEnabled",false],["loc",[null,[10,8],[10,150]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[11,8],[11,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[12,8],[12,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/select2.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,1,["loc",[null,[1,0],[14,7]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/textarea', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/textarea.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morphs = new Array(6);
          morphs[0] = dom.createAttrMorph(element1, 'class');
          morphs[1] = dom.createMorphAt(element1,0,0);
          morphs[2] = dom.createAttrMorph(element2, 'class');
          morphs[3] = dom.createMorphAt(element2,1,1);
          morphs[4] = dom.createMorphAt(element2,3,3);
          morphs[5] = dom.createMorphAt(element2,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",["control-label ",["get","horizontalLabelGridClass",["loc",[null,[2,34],[2,58]]]]]]],
          ["content","label",["loc",[null,[2,62],[2,71]]]],
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[3,18],[3,42]]]]]]],
          ["inline","bs-textarea",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,27],[4,31]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,38],[4,43]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[4,56],[4,67]]]]],[],[]],"cols",["subexpr","@mut",[["get","cols",["loc",[null,[4,73],[4,77]]]]],[],[]],"rows",["subexpr","@mut",[["get","rows",["loc",[null,[4,83],[4,87]]]]],[],[]]],["loc",[null,[4,8],[4,89]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,8],[5,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,8],[6,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 0
            },
            "end": {
              "line": 14,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/textarea.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var morphs = new Array(4);
          morphs[0] = dom.createAttrMorph(element0, 'class');
          morphs[1] = dom.createMorphAt(element0,1,1);
          morphs[2] = dom.createMorphAt(element0,3,3);
          morphs[3] = dom.createMorphAt(element0,5,5);
          return morphs;
        },
        statements: [
          ["attribute","class",["concat",[["get","horizontalInputGridClass",["loc",[null,[9,18],[9,42]]]]," ",["get","horizontalInputOffsetGridClass",["loc",[null,[9,47],[9,77]]]]]]],
          ["inline","bs-textarea",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[10,27],[10,31]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[10,38],[10,43]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[10,56],[10,67]]]]],[],[]],"cols",["subexpr","@mut",[["get","cols",["loc",[null,[10,73],[10,77]]]]],[],[]],"rows",["subexpr","@mut",[["get","rows",["loc",[null,[10,83],[10,87]]]]],[],[]]],["loc",[null,[10,8],[10,89]]]],
          ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[11,8],[11,59]]]],
          ["inline","partial",["components/form-element/errors"],[],["loc",[null,[12,8],[12,52]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/form-element/horizontal/textarea.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,1,["loc",[null,[1,0],[14,7]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/checkbox', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 5,
            "column": 6
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/inline/checkbox.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","checkbox");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("label");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(" ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0, 1]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(element0,3,3);
        return morphs;
      },
      statements: [
        ["inline","input",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[3,21],[3,25]]]]],[],[]],"type","checkbox","checked",["subexpr","@mut",[["get","value",["loc",[null,[3,50],[3,55]]]]],[],[]]],["loc",[null,[3,8],[3,57]]]],
        ["content","label",["loc",[null,[3,58],[3,67]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/default', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/inline/default.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          dom.setAttribute(el1,"class","control-label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","label",["loc",[null,[2,33],[2,42]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/form-element/inline/default.hbs"
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
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,3,3,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]],
        ["inline","bs-input",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,16],[4,20]]]]],[],[]],"type",["subexpr","@mut",[["get","controlType",["loc",[null,[4,26],[4,37]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,44],[4,49]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[4,62],[4,73]]]]],[],[]]],["loc",[null,[4,0],[4,75]]]],
        ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,0],[5,51]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/select', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/inline/select.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          dom.setAttribute(el1,"class","control-label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","label",["loc",[null,[2,33],[2,42]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/form-element/inline/select.hbs"
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
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,3,3,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]],
        ["inline","bs-select",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,17],[4,21]]]]],[],[]],"content",["subexpr","@mut",[["get","choices",["loc",[null,[4,30],[4,37]]]]],[],[]],"optionValuePath",["subexpr","@mut",[["get","selectValueProperty",["loc",[null,[4,54],[4,73]]]]],[],[]],"optionLabelPath",["subexpr","@mut",[["get","selectLabelProperty",["loc",[null,[4,90],[4,109]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,116],[4,121]]]]],[],[]]],["loc",[null,[4,0],[4,123]]]],
        ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,0],[5,51]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/textarea', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/inline/textarea.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          dom.setAttribute(el1,"class","control-label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","label",["loc",[null,[2,33],[2,42]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 44
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/inline/textarea.hbs"
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
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,3,3,contextualElement);
        morphs[3] = dom.createMorphAt(fragment,5,5,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]],
        ["inline","bs-textarea",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,19],[4,23]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,30],[4,35]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[4,48],[4,59]]]]],[],[]],"cols",["subexpr","@mut",[["get","cols",["loc",[null,[4,65],[4,69]]]]],[],[]],"rows",["subexpr","@mut",[["get","rows",["loc",[null,[4,75],[4,79]]]]],[],[]]],["loc",[null,[4,0],[4,81]]]],
        ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,0],[5,51]]]],
        ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,0],[6,44]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/checkbox', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 44
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/vertical/checkbox.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","checkbox");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("label");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode(" ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0, 1]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(element0,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","input",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[3,21],[3,25]]]]],[],[]],"type","checkbox","checked",["subexpr","@mut",[["get","value",["loc",[null,[3,50],[3,55]]]]],[],[]]],["loc",[null,[3,8],[3,57]]]],
        ["content","label",["loc",[null,[3,58],[3,67]]]],
        ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,0],[6,44]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/default', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/vertical/default.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          dom.setAttribute(el1,"class","control-label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","label",["loc",[null,[2,33],[2,42]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 44
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/vertical/default.hbs"
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
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,3,3,contextualElement);
        morphs[3] = dom.createMorphAt(fragment,5,5,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]],
        ["inline","bs-input",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,16],[4,20]]]]],[],[]],"type",["subexpr","@mut",[["get","controlType",["loc",[null,[4,26],[4,37]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,44],[4,49]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[4,62],[4,73]]]]],[],[]]],["loc",[null,[4,0],[4,75]]]],
        ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,0],[5,51]]]],
        ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,0],[6,44]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/select', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/vertical/select.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          dom.setAttribute(el1,"class","control-label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","label",["loc",[null,[2,33],[2,42]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/form-element/vertical/select.hbs"
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
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,3,3,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]],
        ["inline","bs-select",[],["name",["subexpr","@mut",[["get","name",["loc",[null,[4,17],[4,21]]]]],[],[]],"content",["subexpr","@mut",[["get","choices",["loc",[null,[4,30],[4,37]]]]],[],[]],"optionValuePath",["subexpr","@mut",[["get","selectValueProperty",["loc",[null,[4,54],[4,73]]]]],[],[]],"optionLabelPath",["subexpr","@mut",[["get","selectLabelProperty",["loc",[null,[4,90],[4,109]]]]],[],[]],"value",["subexpr","@mut",[["get","value",["loc",[null,[4,116],[4,121]]]]],[],[]]],["loc",[null,[4,0],[4,123]]]],
        ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,0],[5,51]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/textarea', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/form-element/vertical/textarea.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("label");
          dom.setAttribute(el1,"class","control-label");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","label",["loc",[null,[2,33],[2,42]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 6,
            "column": 44
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/form-element/vertical/textarea.hbs"
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
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        morphs[1] = dom.createMorphAt(fragment,1,1,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,3,3,contextualElement);
        morphs[3] = dom.createMorphAt(fragment,5,5,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","hasLabel",["loc",[null,[1,6],[1,14]]]]],[],0,null,["loc",[null,[1,0],[3,7]]]],
        ["inline","bs-textarea",[],["value",["subexpr","@mut",[["get","value",["loc",[null,[4,20],[4,25]]]]],[],[]],"name",["subexpr","@mut",[["get","name",["loc",[null,[4,31],[4,35]]]]],[],[]],"placeholder",["subexpr","@mut",[["get","placeholder",["loc",[null,[4,48],[4,59]]]]],[],[]],"cols",["subexpr","@mut",[["get","cols",["loc",[null,[4,65],[4,69]]]]],[],[]],"rows",["subexpr","@mut",[["get","rows",["loc",[null,[4,75],[4,79]]]]],[],[]]],["loc",[null,[4,0],[4,81]]]],
        ["inline","partial",["components/form-element/feedback-icon"],[],["loc",[null,[5,0],[5,51]]]],
        ["inline","partial",["components/form-element/errors"],[],["loc",[null,[6,0],[6,44]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/link-bucket-type', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 5,
              "column": 31
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link-bucket-type.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","glyphicon glyphicon-inbox cluster-resource-icon");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["content","bucketType.bucketTypeId",["loc",[null,[5,4],[5,31]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/link-bucket-type.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","link-to",["bucket-type",["get","bucketType",["loc",[null,[1,25],[1,35]]]]],["classNames","btn btn-sm btn-primary cluster-resource-link"],0,null,["loc",[null,[1,0],[5,43]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/link-bucket', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 0
            },
            "end": {
              "line": 5,
              "column": 23
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/link-bucket.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","glyphicon glyphicon-folder-close cluster-resource-icon");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["content","bucket.bucketId",["loc",[null,[5,4],[5,23]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/link-bucket.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","link-to",["bucket",["get","bucket",["loc",[null,[1,20],[1,26]]]]],["classNames","btn btn-sm btn-primary cluster-resource-link"],0,null,["loc",[null,[1,0],[5,35]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/link-cluster', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 5,
                "column": 8
              },
              "end": {
                "line": 5,
                "column": 44
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/link-cluster.hbs"
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
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          "moduleName": "ember-riak-explorer/templates/components/link-cluster.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("img");
          dom.setAttribute(el1,"src","assets/images/riak.png");
          dom.setAttribute(el1,"class","cluster-resource-icon");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          morphs[0] = dom.createMorphAt(element0,0,0);
          morphs[1] = dom.createMorphAt(element0,2,2);
          return morphs;
        },
        statements: [
          ["content","cluster.id",["loc",[null,[4,14],[4,28]]]],
          ["block","if",[["get","cluster.developmentMode",["loc",[null,[5,14],[5,37]]]]],[],0,null,["loc",[null,[5,8],[5,51]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/link-cluster.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","link-to",["cluster",["get","cluster",["loc",[null,[1,21],[1,28]]]]],["classNames","btn btn-sm btn-primary cluster-resource-link"],0,null,["loc",[null,[1,0],[7,12]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/loading-spinner', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","center-block text-center loading");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","container");
        var el3 = dom.createTextNode("\n    Loading...\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","container spinner");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("img");
        dom.setAttribute(el3,"src","assets/images/ajax-loading-big.gif");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes() { return []; },
      statements: [

      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/nav-cluster-link', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 3,
                "column": 8
              },
              "end": {
                "line": 3,
                "column": 66
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
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 4
            },
            "end": {
              "line": 4,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/nav-cluster-link.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
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
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
          return morphs;
        },
        statements: [
          ["content","cluster.id",["loc",[null,[2,20],[2,34]]]],
          ["block","if",[["get","cluster.developmentMode",["loc",[null,[3,14],[3,37]]]]],[],0,null,["loc",[null,[3,8],[3,73]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","link-to",["cluster",["get","cluster",["loc",[null,[1,25],[1,32]]]]],[],0,null,["loc",[null,[1,4],[4,16]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-headers-edit', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 6,
                "column": 8
              },
              "end": {
                "line": 11,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-headers-edit.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("tr");
            var el2 = dom.createTextNode("\n            ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"width","30%;");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n            ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            var el3 = dom.createComment("");
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
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
            return morphs;
          },
          statements: [
            ["content","header.key",["loc",[null,[8,29],[8,43]]]],
            ["inline","input",[],["value",["subexpr","@mut",[["get","header.value",["loc",[null,[9,30],[9,42]]]]],[],[]],"id",["subexpr","@mut",[["get","header.key",["loc",[null,[9,46],[9,56]]]]],[],[]],"class","form-control"],["loc",[null,[9,16],[9,79]]]]
          ],
          locals: ["header"],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("h4");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1,"class","table");
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
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3, 1]),1,1);
          return morphs;
        },
        statements: [
          ["content","title",["loc",[null,[3,4],[3,13]]]],
          ["block","each",[["get","headers",["loc",[null,[6,16],[6,23]]]]],[],0,null,["loc",[null,[6,8],[11,17]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","headers",["loc",[null,[1,6],[1,13]]]]],[],0,null,["loc",[null,[1,0],[15,7]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-headers', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 6,
                "column": 8
              },
              "end": {
                "line": 11,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-headers.hbs"
          },
          arity: 1,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("tr");
            var el2 = dom.createTextNode("\n            ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"width","30%;");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n            ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            var el3 = dom.createComment("");
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
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
            return morphs;
          },
          statements: [
            ["content","header.key",["loc",[null,[8,29],[8,43]]]],
            ["content","header.value",["loc",[null,[9,16],[9,32]]]]
          ],
          locals: ["header"],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
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
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("h4");
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1,"class","table");
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
          var el2 = dom.createTextNode("\n");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3, 1]),1,1);
          return morphs;
        },
        statements: [
          ["content","title",["loc",[null,[3,4],[3,13]]]],
          ["block","each",[["get","headers",["loc",[null,[6,16],[6,23]]]]],[],0,null,["loc",[null,[6,8],[11,17]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","headers",["loc",[null,[1,6],[1,13]]]]],[],0,null,["loc",[null,[1,0],[15,7]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-location', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 30,
                "column": 16
              },
              "end": {
                "line": 32,
                "column": 26
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-location.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                    Cancel");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes() { return []; },
          statements: [

          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 27,
              "column": 12
            },
            "end": {
              "line": 33,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-location.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                (Editing)\n                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("br");
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
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
          return morphs;
        },
        statements: [
          ["block","link-to",["riak-object",["get","object",["loc",[null,[30,41],[30,47]]]]],["classNames","btn btn-xs btn-default"],0,null,["loc",[null,[30,16],[32,38]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 33,
              "column": 12
            },
            "end": {
              "line": 35,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-location.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["inline","edit-object",[],["object",["subexpr","@mut",[["get","object",["loc",[null,[34,37],[34,43]]]]],[],[]]],["loc",[null,[34,16],[34,45]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 39,
              "column": 12
            },
            "end": {
              "line": 41,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-location.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("strong");
          var el2 = dom.createTextNode("Yes");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child3 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 43,
                "column": 16
              },
              "end": {
                "line": 45,
                "column": 16
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-location.hbs"
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","delete-object",[],["action","deleteObject","object",["subexpr","@mut",[["get","object",["loc",[null,[44,65],[44,71]]]]],[],[]]],["loc",[null,[44,20],[44,73]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 41,
              "column": 12
            },
            "end": {
              "line": 46,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-location.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                No\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","unless",[["get","isEditing",["loc",[null,[43,26],[43,35]]]]],[],0,null,["loc",[null,[43,16],[45,27]]]]
        ],
        locals: [],
        templates: [child0]
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 63,
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
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","crumb-trail");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    /\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12 text-center");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","cluster-resource-header center-block");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5,"class","glyphicon glyphicon-tag cluster-resource-icon");
        dom.setAttribute(el5,"aria-hidden","true");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-6");
        var el4 = dom.createTextNode("\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-6");
        var el4 = dom.createTextNode("\n            Deleted:\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-6");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Last Modified:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-6");
        var el4 = dom.createTextNode("\n            Page loaded: ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
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
        var element2 = dom.childAt(fragment, [4, 1]);
        var element3 = dom.childAt(fragment, [8, 1]);
        var morphs = new Array(8);
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element1,3,3);
        morphs[2] = dom.createMorphAt(element1,5,5);
        morphs[3] = dom.createMorphAt(dom.childAt(element0, [3, 1, 1, 3]),0,0);
        morphs[4] = dom.createMorphAt(dom.childAt(element2, [1]),1,1);
        morphs[5] = dom.createMorphAt(dom.childAt(element2, [3]),1,1);
        morphs[6] = dom.createMorphAt(dom.childAt(element3, [1]),3,3);
        morphs[7] = dom.createMorphAt(dom.childAt(element3, [3]),1,1);
        return morphs;
      },
      statements: [
        ["inline","link-cluster",[],["cluster",["subexpr","@mut",[["get","object.cluster",["loc",[null,[3,27],[3,41]]]]],[],[]]],["loc",[null,[3,4],[3,43]]]],
        ["inline","link-bucket-type",[],["bucketType",["subexpr","@mut",[["get","object.bucketType",["loc",[null,[5,34],[5,51]]]]],[],[]]],["loc",[null,[5,4],[5,53]]]],
        ["inline","link-bucket",[],["bucket",["subexpr","@mut",[["get","object.bucket",["loc",[null,[7,25],[7,38]]]]],[],[]]],["loc",[null,[7,4],[7,40]]]],
        ["content","object.key",["loc",[null,[16,22],[16,36]]]],
        ["block","if",[["get","isEditing",["loc",[null,[27,18],[27,27]]]]],[],0,1,["loc",[null,[27,12],[35,19]]]],
        ["block","if",[["get","object.isDeleted",["loc",[null,[39,18],[39,34]]]]],[],2,3,["loc",[null,[39,12],[46,19]]]],
        ["content","object.dateLastModified",["loc",[null,[56,12],[56,39]]]],
        ["content","object.dateLoaded",["loc",[null,[59,25],[59,46]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-version', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/object-version.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h4");
        var el3 = dom.createTextNode("Version Details");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-7");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Causal Context:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-5");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("ETag:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
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
        var element0 = dom.childAt(fragment, [2, 3]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),3,3);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),3,3);
        return morphs;
      },
      statements: [
        ["content","object.causalContext",["loc",[null,[8,12],[8,36]]]],
        ["content","object.etag",["loc",[null,[12,12],[12,27]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/refresh-buckets', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/refresh-buckets.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-xs btn-primary");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2,"class","glyphicon glyphicon-refresh");
        dom.setAttribute(el2,"aria-hidden","true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    Refresh Bucket Cache");
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
      statements: [
        ["element","action",["refreshBuckets",["get","bucketType",["loc",[null,[2,34],[2,44]]]]],[],["loc",[null,[2,8],[2,46]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/refresh-keys', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/refresh-keys.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-xs btn-primary");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2,"class","glyphicon glyphicon-refresh");
        dom.setAttribute(el2,"aria-hidden","true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    Refresh Key Cache");
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
      statements: [
        ["element","action",["refreshKeys",["get","bucket",["loc",[null,[2,31],[2,37]]]]],[],["loc",[null,[2,8],[2,39]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/riak-buckets', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 9,
                "column": 20
              },
              "end": {
                "line": 13,
                "column": 39
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/riak-buckets.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("span");
            dom.setAttribute(el1,"class","glyphicon glyphicon-folder-close cluster-resource-icon");
            dom.setAttribute(el1,"aria-hidden","true");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n                        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(1);
            morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [
            ["content","bucket.name",["loc",[null,[13,24],[13,39]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 3,
              "column": 0
            },
            "end": {
              "line": 19,
              "column": 0
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
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3,"class","container");
          var el4 = dom.createTextNode("\n            ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("div");
          dom.setAttribute(el4,"class","row");
          var el5 = dom.createTextNode("\n                ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          dom.setAttribute(el5,"class","col-md-12");
          var el6 = dom.createTextNode("\n");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode("\n                ");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n            ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1, 1, 1, 1]),1,1);
          return morphs;
        },
        statements: [
          ["block","link-to",["bucket",["get","bucket",["loc",[null,[9,40],[9,46]]]]],["classNames","btn btn-sm btn-primary cluster-resource-link"],0,null,["loc",[null,[9,20],[13,51]]]]
        ],
        locals: ["bucket"],
        templates: [child0]
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 19,
              "column": 0
            },
            "end": {
              "line": 21,
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
          var el1 = dom.createTextNode("    No buckets found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/riak-buckets.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1,"class","table");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1]),1,1);
        return morphs;
      },
      statements: [
        ["block","each",[["get","bucketList.buckets",["loc",[null,[3,8],[3,26]]]]],[],0,1,["loc",[null,[3,0],[21,9]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/riak-keys', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 9,
                "column": 20
              },
              "end": {
                "line": 11,
                "column": 20
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/riak-keys.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                        ");
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
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
            return morphs;
          },
          statements: [
            ["content","obj.key",["loc",[null,[10,32],[10,43]]]]
          ],
          locals: [],
          templates: []
        };
      }());
      var child1 = (function() {
        var child0 = (function() {
          return {
            meta: {
              "revision": "Ember@1.13.5",
              "loc": {
                "source": null,
                "start": {
                  "line": 12,
                  "column": 24
                },
                "end": {
                  "line": 13,
                  "column": 39
                }
              },
              "moduleName": "ember-riak-explorer/templates/components/riak-keys.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("                            ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
              dom.insertBoundary(fragment, null);
              return morphs;
            },
            statements: [
              ["content","obj.key",["loc",[null,[13,28],[13,39]]]]
            ],
            locals: [],
            templates: []
          };
        }());
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 11,
                "column": 20
              },
              "end": {
                "line": 14,
                "column": 20
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/riak-keys.hbs"
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
            morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
            dom.insertBoundary(fragment, 0);
            return morphs;
          },
          statements: [
            ["block","link-to",["riak-object",["get","obj",["loc",[null,[12,49],[12,52]]]]],[],0,null,["loc",[null,[12,24],[13,51]]]]
          ],
          locals: [],
          templates: [child0]
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 3,
              "column": 0
            },
            "end": {
              "line": 20,
              "column": 0
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
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("div");
          dom.setAttribute(el3,"class","container");
          var el4 = dom.createTextNode("\n            ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("div");
          dom.setAttribute(el4,"class","row");
          var el5 = dom.createTextNode("\n                ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          var el6 = dom.createTextNode("\n");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode("                ");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n            ");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n        ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1, 1, 1, 1]),1,1);
          return morphs;
        },
        statements: [
          ["block","if",[["get","obj.markedDeleted",["loc",[null,[9,26],[9,43]]]]],[],0,1,["loc",[null,[9,20],[14,27]]]]
        ],
        locals: ["obj"],
        templates: [child0, child1]
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 20,
              "column": 0
            },
            "end": {
              "line": 22,
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
          var el1 = dom.createTextNode("    No keys found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/riak-keys.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1,"class","table");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1]),1,1);
        return morphs;
      },
      statements: [
        ["block","each",[["get","keys",["loc",[null,[3,8],[3,12]]]]],[],0,1,["loc",[null,[3,0],[22,9]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/riak-node', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 1,
              "column": 8
            },
            "end": {
              "line": 1,
              "column": 109
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-node.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("Node Ping");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 3,
              "column": 8
            },
            "end": {
              "line": 5,
              "column": 18
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/riak-node.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        Node Stats");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/riak-node.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createTextNode("    ");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("td");
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n    ");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("td");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n    ");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
        morphs[1] = dom.createMorphAt(dom.childAt(fragment, [3]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(fragment, [5]),0,0);
        return morphs;
      },
      statements: [
        ["block","link-to",["riak_ping",["subexpr","query-params",[],["node_id",["get","node.id",["loc",[null,[1,53],[1,60]]]]],["loc",[null,[1,31],[1,61]]]]],["classNames","btn btn-sm btn-default"],0,null,["loc",[null,[1,8],[1,121]]]],
        ["block","link-to",["node_stats",["subexpr","query-params",[],["node_id",["get","node.id",["loc",[null,[3,54],[3,61]]]]],["loc",[null,[3,32],[3,62]]]]],["classNames","btn btn-sm btn-primary"],1,null,["loc",[null,[3,8],[5,30]]]],
        ["content","node.id",["loc",[null,[6,8],[6,19]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/search-indexes', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 10,
              "column": 0
            },
            "end": {
              "line": 18,
              "column": 0
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
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("a");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n        ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
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
          var element1 = dom.childAt(element0, [3, 1]);
          var morphs = new Array(4);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          morphs[1] = dom.createAttrMorph(element1, 'href');
          morphs[2] = dom.createMorphAt(element1,0,0);
          morphs[3] = dom.createMorphAt(dom.childAt(element0, [5]),0,0);
          return morphs;
        },
        statements: [
          ["content","index.name",["loc",[null,[12,12],[12,26]]]],
          ["attribute","href",["concat",[["get","clusterProxyUrl",["loc",[null,[14,23],[14,38]]]],"/search/schema/",["get","index.schema",["loc",[null,[14,57],[14,69]]]]]]],
          ["content","index.schema",["loc",[null,[14,73],[14,89]]]],
          ["content","index.n_val",["loc",[null,[16,12],[16,27]]]]
        ],
        locals: ["index"],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 18,
              "column": 0
            },
            "end": {
              "line": 20,
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
          var el1 = dom.createTextNode("    No indexes found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/components/search-indexes.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("table");
        dom.setAttribute(el1,"class","table");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Index Name");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Schema");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("n_val");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 3]),1,1);
        return morphs;
      },
      statements: [
        ["block","each",[["get","indexes",["loc",[null,[10,8],[10,15]]]]],[],0,1,["loc",[null,[10,0],[20,9]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/error/cluster-not-found', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 13,
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container text-center");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h2");
        var el3 = dom.createTextNode("404 Cluster Not Found");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","container");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","row");
        var el4 = dom.createTextNode("\n            The cluster id you requested was not found:\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("strong");
        var el6 = dom.createComment("");
        dom.appendChild(el5, el6);
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 3, 1, 1, 1]),0,0);
        return morphs;
      },
      statements: [
        ["content","model.clusterId",["loc",[null,[8,20],[8,39]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/error/object-not-found', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 13,
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container text-center");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h2");
        var el3 = dom.createTextNode("404 Object Not Found");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","container");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","row key");
        var el4 = dom.createTextNode("\n                key:\n                ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n    ");
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
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [3, 1, 1]),0,0);
        morphs[1] = dom.createMorphAt(element0,5,5);
        return morphs;
      },
      statements: [
        ["content","model.key",["loc",[null,[7,24],[7,37]]]],
        ["inline","crumb-trail",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[11,24],[11,29]]]]],[],[]]],["loc",[null,[11,4],[11,31]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/error/unknown', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/error/unknown.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container text-center");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("h3");
        var el3 = dom.createTextNode("An unknown error has occurred");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("p");
        var el3 = dom.createTextNode("Consider opening a bug report.");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes() { return []; },
      statements: [

      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/explorer-api', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 7,
              "column": 0
            },
            "end": {
              "line": 8,
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
          var el1 = dom.createTextNode("Available (");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(")\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["content","model.pingResult.ping.message",["loc",[null,[7,40],[7,73]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child1 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 8,
              "column": 0
            },
            "end": {
              "line": 8,
              "column": 19
            }
          },
          "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("Unavailable");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes() { return []; },
        statements: [

        ],
        locals: [],
        templates: []
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 21,
              "column": 0
            },
            "end": {
              "line": 26,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createTextNode("[");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("]");
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
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
          return morphs;
        },
        statements: [
          ["content","route.links.self",["loc",[null,[23,16],[23,36]]]],
          ["content","route.resources",["loc",[null,[24,17],[24,36]]]]
        ],
        locals: ["route"],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
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
        "moduleName": "ember-riak-explorer/templates/explorer-api.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("h2");
        dom.setAttribute(el1,"class","page-header");
        dom.setAttribute(el1,"id","title");
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("h3");
        var el2 = dom.createTextNode("Service Information");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("strong");
        var el2 = dom.createTextNode("Name:");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode(" ");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("strong");
        var el2 = dom.createTextNode("Ping:");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("strong");
        var el2 = dom.createTextNode("Development Mode:");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode(" ");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("h3");
        var el2 = dom.createTextNode("Routes");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("table");
        dom.setAttribute(el1,"class","table");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Path");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Resources");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
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
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(5);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0]),0,0);
        morphs[1] = dom.createMorphAt(fragment,6,6,contextualElement);
        morphs[2] = dom.createMorphAt(fragment,11,11,contextualElement);
        morphs[3] = dom.createMorphAt(fragment,16,16,contextualElement);
        morphs[4] = dom.createMorphAt(dom.childAt(fragment, [21, 3]),1,1);
        return morphs;
      },
      statements: [
        ["content","pageTitle",["loc",[null,[1,35],[1,48]]]],
        ["content","model.service",["loc",[null,[5,23],[5,40]]]],
        ["block","if",[["get","model.pingResult.ping",["loc",[null,[7,6],[7,27]]]]],[],0,1,["loc",[null,[7,0],[8,26]]]],
        ["content","model.propsResult.props.development_mode",["loc",[null,[9,35],[9,79]]]],
        ["block","each",[["get","model.routes",["loc",[null,[21,8],[21,20]]]]],[],2,null,["loc",[null,[21,0],[26,9]]]]
      ],
      locals: [],
      templates: [child0, child1, child2]
    };
  }()));

});
define('ember-riak-explorer/templates/index', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 2,
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
        var el1 = dom.createTextNode("Select cluster from the nav bar on the left.\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes() { return []; },
      statements: [

      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/loading', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 2,
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
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(1);
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["content","loading-spinner",["loc",[null,[1,0],[1,19]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/node-stats', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 11,
              "column": 0
            },
            "end": {
              "line": 16,
              "column": 0
            }
          },
          "moduleName": "ember-riak-explorer/templates/node-stats.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("tr");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          var el3 = dom.createComment("");
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
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          return morphs;
        },
        statements: [
          ["content","stat.key",["loc",[null,[13,16],[13,28]]]],
          ["content","stat.value",["loc",[null,[14,16],[14,30]]]]
        ],
        locals: ["stat"],
        templates: []
      };
    }());
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 19,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/node-stats.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("h3");
        var el2 = dom.createTextNode("Riak Node Stats (");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode(")");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("table");
        dom.setAttribute(el1,"class","table");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("thead");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Property");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("th");
        var el5 = dom.createTextNode("Value");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
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
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(fragment, [2, 3]),1,1);
        return morphs;
      },
      statements: [
        ["content","model.node",["loc",[null,[1,21],[1,35]]]],
        ["block","each",[["get","model.stats",["loc",[null,[11,8],[11,19]]]]],[],0,null,["loc",[null,[11,0],[16,9]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/riak-ping', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      meta: {
        "revision": "Ember@1.13.5",
        "loc": {
          "source": null,
          "start": {
            "line": 1,
            "column": 0
          },
          "end": {
            "line": 12,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/riak-ping.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-2");
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Node id:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-10");
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-2");
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Riak Ping:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-10");
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
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
        var element0 = dom.childAt(fragment, [1]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]),0,0);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]),0,0);
        return morphs;
      },
      statements: [
        ["content","node_id",["loc",[null,[5,31],[5,42]]]],
        ["content","model.message",["loc",[null,[9,31],[9,48]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/tests/adapters/application.jshint', function () {

  'use strict';

  module('JSHint - adapters');
  test('adapters/application.js should pass jshint', function() { 
    ok(true, 'adapters/application.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/adapters/explorer-resource.jshint', function () {

  'use strict';

  module('JSHint - adapters');
  test('adapters/explorer-resource.js should pass jshint', function() { 
    ok(true, 'adapters/explorer-resource.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/app.jshint', function () {

  'use strict';

  module('JSHint - .');
  test('app.js should pass jshint', function() { 
    ok(true, 'app.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/bucket-properties-list.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/bucket-properties-list.js should pass jshint', function() { 
    ok(true, 'components/bucket-properties-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/bucket-properties-overview.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/bucket-properties-overview.js should pass jshint', function() { 
    ok(true, 'components/bucket-properties-overview.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/bucket-properties.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/bucket-properties.js should pass jshint', function() { 
    ok(true, 'components/bucket-properties.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/bucket-types.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/bucket-types.js should pass jshint', function() { 
    ok(true, 'components/bucket-types.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/crumb-trail.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/crumb-trail.js should pass jshint', function() { 
    ok(true, 'components/crumb-trail.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/delete-object.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/delete-object.js should pass jshint', function() { 
    ok(true, 'components/delete-object.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/edit-object.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/edit-object.js should pass jshint', function() { 
    ok(true, 'components/edit-object.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/link-bucket-type.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/link-bucket-type.js should pass jshint', function() { 
    ok(true, 'components/link-bucket-type.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/link-bucket.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/link-bucket.js should pass jshint', function() { 
    ok(true, 'components/link-bucket.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/link-cluster.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/link-cluster.js should pass jshint', function() { 
    ok(true, 'components/link-cluster.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/loading-spinner.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/loading-spinner.js should pass jshint', function() { 
    ok(true, 'components/loading-spinner.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/nav-cluster-link.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/nav-cluster-link.js should pass jshint', function() { 
    ok(true, 'components/nav-cluster-link.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-headers-edit.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-headers-edit.js should pass jshint', function() { 
    ok(true, 'components/object-headers-edit.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-headers.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-headers.js should pass jshint', function() { 
    ok(true, 'components/object-headers.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-location.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-location.js should pass jshint', function() { 
    ok(true, 'components/object-location.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-version.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-version.js should pass jshint', function() { 
    ok(true, 'components/object-version.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/refresh-buckets.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/refresh-buckets.js should pass jshint', function() { 
    ok(true, 'components/refresh-buckets.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/refresh-keys.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/refresh-keys.js should pass jshint', function() { 
    ok(true, 'components/refresh-keys.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/riak-buckets.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/riak-buckets.js should pass jshint', function() { 
    ok(true, 'components/riak-buckets.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/riak-keys.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/riak-keys.js should pass jshint', function() { 
    ok(true, 'components/riak-keys.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/riak-node.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/riak-node.js should pass jshint', function() { 
    ok(true, 'components/riak-node.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/search-indexes.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/search-indexes.js should pass jshint', function() { 
    ok(true, 'components/search-indexes.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/error/cluster-not-found.jshint', function () {

  'use strict';

  module('JSHint - controllers/error');
  test('controllers/error/cluster-not-found.js should pass jshint', function() { 
    ok(true, 'controllers/error/cluster-not-found.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/error/object-not-found.jshint', function () {

  'use strict';

  module('JSHint - controllers/error');
  test('controllers/error/object-not-found.js should pass jshint', function() { 
    ok(true, 'controllers/error/object-not-found.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/error/unknown.jshint', function () {

  'use strict';

  module('JSHint - controllers/error');
  test('controllers/error/unknown.js should pass jshint', function() { 
    ok(true, 'controllers/error/unknown.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/explorer-api.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/explorer-api.js should pass jshint', function() { 
    ok(true, 'controllers/explorer-api.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/node-stats.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/node-stats.js should pass jshint', function() { 
    ok(true, 'controllers/node-stats.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/riak-ping.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/riak-ping.js should pass jshint', function() { 
    ok(true, 'controllers/riak-ping.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/helpers/resolver', ['exports', 'ember/resolver', 'ember-riak-explorer/config/environment'], function (exports, Resolver, config) {

  'use strict';

  var resolver = Resolver['default'].create();

  resolver.namespace = {
    modulePrefix: config['default'].modulePrefix,
    podModulePrefix: config['default'].podModulePrefix
  };

  exports['default'] = resolver;

});
define('ember-riak-explorer/tests/helpers/resolver.jshint', function () {

  'use strict';

  module('JSHint - helpers');
  test('helpers/resolver.js should pass jshint', function() { 
    ok(true, 'helpers/resolver.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/helpers/start-app', ['exports', 'ember', 'ember-riak-explorer/app', 'ember-riak-explorer/config/environment'], function (exports, Ember, Application, config) {

  'use strict';



  exports['default'] = startApp;
  function startApp(attrs) {
    var application;

    var attributes = Ember['default'].merge({}, config['default'].APP);
    attributes = Ember['default'].merge(attributes, attrs); // use defaults, but you can override;

    Ember['default'].run(function () {
      application = Application['default'].create(attributes);
      application.setupForTesting();
      application.injectTestHelpers();
    });

    return application;
  }

});
define('ember-riak-explorer/tests/helpers/start-app.jshint', function () {

  'use strict';

  module('JSHint - helpers');
  test('helpers/start-app.js should pass jshint', function() { 
    ok(true, 'helpers/start-app.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/initializers/explorer.jshint', function () {

  'use strict';

  module('JSHint - initializers');
  test('initializers/explorer.js should pass jshint', function() { 
    ok(true, 'initializers/explorer.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/bucket-list.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/bucket-list.js should pass jshint', function() { 
    ok(true, 'models/bucket-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/bucket-props.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/bucket-props.js should pass jshint', function() { 
    ok(true, 'models/bucket-props.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/cached-list.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/cached-list.js should pass jshint', function() { 
    ok(true, 'models/cached-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/key-list.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/key-list.js should pass jshint', function() { 
    ok(true, 'models/key-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/link.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/link.js should pass jshint', function() { 
    ok(true, 'models/link.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/route.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/route.js should pass jshint', function() { 
    ok(true, 'models/route.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/bucket-type/controller.jshint', function () {

  'use strict';

  module('JSHint - pods/bucket-type');
  test('pods/bucket-type/controller.js should pass jshint', function() { 
    ok(true, 'pods/bucket-type/controller.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/bucket-type/model.jshint', function () {

  'use strict';

  module('JSHint - pods/bucket-type');
  test('pods/bucket-type/model.js should pass jshint', function() { 
    ok(true, 'pods/bucket-type/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/bucket-type/route.jshint', function () {

  'use strict';

  module('JSHint - pods/bucket-type');
  test('pods/bucket-type/route.js should pass jshint', function() { 
    ok(true, 'pods/bucket-type/route.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/bucket/controller.jshint', function () {

  'use strict';

  module('JSHint - pods/bucket');
  test('pods/bucket/controller.js should pass jshint', function() { 
    ok(true, 'pods/bucket/controller.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/bucket/model.jshint', function () {

  'use strict';

  module('JSHint - pods/bucket');
  test('pods/bucket/model.js should pass jshint', function() { 
    ok(true, 'pods/bucket/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/bucket/route.jshint', function () {

  'use strict';

  module('JSHint - pods/bucket');
  test('pods/bucket/route.js should pass jshint', function() { 
    ok(true, 'pods/bucket/route.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/cluster/model.jshint', function () {

  'use strict';

  module('JSHint - pods/cluster');
  test('pods/cluster/model.js should pass jshint', function() { 
    ok(true, 'pods/cluster/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/cluster/route.jshint', function () {

  'use strict';

  module('JSHint - pods/cluster');
  test('pods/cluster/route.js should pass jshint', function() { 
    ok(true, 'pods/cluster/route.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/controller.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object');
  test('pods/riak-object/controller.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/controller.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/edit/controller.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/edit');
  test('pods/riak-object/edit/controller.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/edit/controller.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/edit/route.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/edit');
  test('pods/riak-object/edit/route.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/edit/route.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/model.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object');
  test('pods/riak-object/model.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/route.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object');
  test('pods/riak-object/route.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/route.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/search-index/model.jshint', function () {

  'use strict';

  module('JSHint - pods/search-index');
  test('pods/search-index/model.js should pass jshint', function() { 
    ok(true, 'pods/search-index/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/router.jshint', function () {

  'use strict';

  module('JSHint - .');
  test('router.js should pass jshint', function() { 
    ok(true, 'router.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/application.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/application.js should pass jshint', function() { 
    ok(true, 'routes/application.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/error/cluster-not-found.jshint', function () {

  'use strict';

  module('JSHint - routes/error');
  test('routes/error/cluster-not-found.js should pass jshint', function() { 
    ok(true, 'routes/error/cluster-not-found.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/error/object-not-found.jshint', function () {

  'use strict';

  module('JSHint - routes/error');
  test('routes/error/object-not-found.js should pass jshint', function() { 
    ok(true, 'routes/error/object-not-found.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/error/unknown.jshint', function () {

  'use strict';

  module('JSHint - routes/error');
  test('routes/error/unknown.js should pass jshint', function() { 
    ok(true, 'routes/error/unknown.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/explorer-api.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/explorer-api.js should pass jshint', function() { 
    ok(true, 'routes/explorer-api.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/index.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/index.js should pass jshint', function() { 
    ok(true, 'routes/index.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/node-stats.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/node-stats.js should pass jshint', function() { 
    ok(true, 'routes/node-stats.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/riak-ping.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/riak-ping.js should pass jshint', function() { 
    ok(true, 'routes/riak-ping.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/serializers/application.jshint', function () {

  'use strict';

  module('JSHint - serializers');
  test('serializers/application.js should pass jshint', function() { 
    ok(true, 'serializers/application.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/services/explorer.jshint', function () {

  'use strict';

  module('JSHint - services');
  test('services/explorer.js should pass jshint', function() { 
    ok(true, 'services/explorer.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/test-helper', ['ember-riak-explorer/tests/helpers/resolver', 'ember-qunit'], function (resolver, ember_qunit) {

	'use strict';

	ember_qunit.setResolver(resolver['default']);

});
define('ember-riak-explorer/tests/test-helper.jshint', function () {

  'use strict';

  module('JSHint - .');
  test('test-helper.js should pass jshint', function() { 
    ok(true, 'test-helper.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/components/object-headers-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('object-headers', {
    // Specify the other units that are required for this test
    // needs: ['component:foo', 'helper:bar']
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Creates the component instance
    var component = this.subject();
    assert.equal(component._state, 'preRender');

    // Renders the component to the page
    this.render();
    assert.equal(component._state, 'inDOM');
  });

});
define('ember-riak-explorer/tests/unit/components/object-headers-test.jshint', function () {

  'use strict';

  module('JSHint - unit/components');
  test('unit/components/object-headers-test.js should pass jshint', function() { 
    ok(true, 'unit/components/object-headers-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/components/riak-node-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('riak-node', {
    // Specify the other units that are required for this test
    // needs: ['component:foo', 'helper:bar']
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Creates the component instance
    var component = this.subject();
    assert.equal(component._state, 'preRender');

    // Renders the component to the page
    this.render();
    assert.equal(component._state, 'inDOM');
  });

});
define('ember-riak-explorer/tests/unit/components/riak-node-test.jshint', function () {

  'use strict';

  module('JSHint - unit/components');
  test('unit/components/riak-node-test.js should pass jshint', function() { 
    ok(true, 'unit/components/riak-node-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/controllers/bucket-types-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('controller:bucket-types', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

});
define('ember-riak-explorer/tests/unit/controllers/bucket-types-test.jshint', function () {

  'use strict';

  module('JSHint - unit/controllers');
  test('unit/controllers/bucket-types-test.js should pass jshint', function() { 
    ok(true, 'unit/controllers/bucket-types-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/controllers/ping-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('controller:ping', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

});
define('ember-riak-explorer/tests/unit/controllers/ping-test.jshint', function () {

  'use strict';

  module('JSHint - unit/controllers');
  test('unit/controllers/ping-test.js should pass jshint', function() { 
    ok(true, 'unit/controllers/ping-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/controllers/riak-object-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('controller:riak-object', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

});
define('ember-riak-explorer/tests/unit/controllers/riak-object-test.jshint', function () {

  'use strict';

  module('JSHint - unit/controllers');
  test('unit/controllers/riak-object-test.js should pass jshint', function() { 
    ok(true, 'unit/controllers/riak-object-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/controllers/riak-ping-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('controller:riak-ping', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

});
define('ember-riak-explorer/tests/unit/controllers/riak-ping-test.jshint', function () {

  'use strict';

  module('JSHint - unit/controllers');
  test('unit/controllers/riak-ping-test.js should pass jshint', function() { 
    ok(true, 'unit/controllers/riak-ping-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/initializers/explorer-test', ['ember', 'ember-riak-explorer/initializers/explorer', 'qunit'], function (Ember, explorer, qunit) {

  'use strict';

  var container, application;

  qunit.module('ExplorerInitializer', {
    beforeEach: function beforeEach() {
      Ember['default'].run(function () {
        application = Ember['default'].Application.create();
        container = application.__container__;
        application.deferReadiness();
      });
    }
  });

  // Replace this with your real tests.
  qunit.test('it works', function (assert) {
    explorer.initialize(container, application);

    // you would normally confirm the results of the initializer here
    assert.ok(true);
  });

});
define('ember-riak-explorer/tests/unit/initializers/explorer-test.jshint', function () {

  'use strict';

  module('JSHint - unit/initializers');
  test('unit/initializers/explorer-test.js should pass jshint', function() { 
    ok(true, 'unit/initializers/explorer-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/models/bucket-type-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForModel('bucket-type', {
    // Specify the other units that are required for this test.
    needs: []
  });

  ember_qunit.test('it exists', function (assert) {
    var model = this.subject();
    // var store = this.store();
    assert.ok(!!model);
  });

});
define('ember-riak-explorer/tests/unit/models/bucket-type-test.jshint', function () {

  'use strict';

  module('JSHint - unit/models');
  test('unit/models/bucket-type-test.js should pass jshint', function() { 
    ok(true, 'unit/models/bucket-type-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/routes/application-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('route:application', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

});
define('ember-riak-explorer/tests/unit/routes/application-test.jshint', function () {

  'use strict';

  module('JSHint - unit/routes');
  test('unit/routes/application-test.js should pass jshint', function() { 
    ok(true, 'unit/routes/application-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/routes/bucket-types-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('route:bucket-types', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

});
define('ember-riak-explorer/tests/unit/routes/bucket-types-test.jshint', function () {

  'use strict';

  module('JSHint - unit/routes');
  test('unit/routes/bucket-types-test.js should pass jshint', function() { 
    ok(true, 'unit/routes/bucket-types-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/routes/ping-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('route:ping', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

});
define('ember-riak-explorer/tests/unit/routes/ping-test.jshint', function () {

  'use strict';

  module('JSHint - unit/routes');
  test('unit/routes/ping-test.js should pass jshint', function() { 
    ok(true, 'unit/routes/ping-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/routes/riak-object-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('route:riak-object', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

});
define('ember-riak-explorer/tests/unit/routes/riak-object-test.jshint', function () {

  'use strict';

  module('JSHint - unit/routes');
  test('unit/routes/riak-object-test.js should pass jshint', function() { 
    ok(true, 'unit/routes/riak-object-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/routes/riak-ping-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('route:riak-ping', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

});
define('ember-riak-explorer/tests/unit/routes/riak-ping-test.jshint', function () {

  'use strict';

  module('JSHint - unit/routes');
  test('unit/routes/riak-ping-test.js should pass jshint', function() { 
    ok(true, 'unit/routes/riak-ping-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/unit/services/explorer-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('service:explorer', {
    // Specify the other units that are required for this test.
    // needs: ['service:foo']
  });

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var service = this.subject();
    assert.ok(service);
  });

});
define('ember-riak-explorer/tests/unit/services/explorer-test.jshint', function () {

  'use strict';

  module('JSHint - unit/services');
  test('unit/services/explorer-test.js should pass jshint', function() { 
    ok(true, 'unit/services/explorer-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/utils/riak-util.jshint', function () {

  'use strict';

  module('JSHint - utils');
  test('utils/riak-util.js should pass jshint', function() { 
    ok(true, 'utils/riak-util.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/utils/riak-util', ['exports'], function (exports) {

    'use strict';



    exports['default'] = objectToArray;
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

if (runningTests) {
  require("ember-riak-explorer/tests/test-helper");
} else {
  require("ember-riak-explorer/app")["default"].create({"name":"ember-riak-explorer","version":"0.0.0+caad537e"});
}

/* jshint ignore:end */
//# sourceMappingURL=ember-riak-explorer.map