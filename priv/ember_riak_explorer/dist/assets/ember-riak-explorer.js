/* jshint ignore:start */

/* jshint ignore:end */

define('ember-riak-explorer/adapters/application', ['exports', 'ember-data', 'ember'], function (exports, DS, Ember) {

  'use strict';

  exports['default'] = DS['default'].RESTAdapter.extend({
    namespace: 'explore',

    /**
     @method buildURL
     @param {String} type
     @param {String} id
     @param {Object} query
     @return {String} url
    */
    buildURL: function buildURL(type, id, query) {
      var url = [];
      var host = Ember['default'].get(this, 'host');
      var prefix = this.urlPrefix();

      if (type) {
        url.push(this.pathForType(type));
      }

      // We might get passed in an array of ids from findMany
      // in which case we don't want to modify the url, as the
      // ids will be passed in through a query param
      if (id && !Ember['default'].isArray(id)) {
        url.push(encodeURIComponent(id));
      }

      if (query && query.cluster_id) {
        url.unshift('clusters/' + query.cluster_id);
      }

      if (query && query.node_id) {
        url.unshift('nodes/' + query.node_id);
      }

      if (prefix) {
        url.unshift(prefix);
      }

      url = url.join('/');
      if (!host && url) {
        url = '/' + url;
      }

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
    findQuery: function findQuery(store, type, query) {
      var url = this.buildURL(type.typeKey, null, query);

      if (this.sortQueryParams) {
        query = this.sortQueryParams(query);
      }

      return this.ajax(url, 'GET');
    },

    pathForType: function pathForType(type) {
      return Ember['default'].String.underscore(Ember['default'].String.pluralize(type));
    }
  });

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
define('ember-riak-explorer/controllers/bucket-list', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

        queryParams: ['cluster_id', 'bucket_type_id'],
        cluster_id: null,
        bucket_type_id: null,

        // delay in milliseconds
        pollForModel: function pollForModel(bucketList, delay) {
            var self = this;
            Ember['default'].run.later(function () {
                self.refreshModel(bucketList);
            }, delay);
        },

        refreshModel: function refreshModel(bucketList) {
            var clusterId = bucketList.get('cluster').get('clusterId');
            var bucketTypeId = bucketList.get('bucketTypeId');
            var self = this;

            self.get('explorer').getBucketList(clusterId, bucketTypeId, self.store).then(function (updatedModel) {
                updatedModel.then(function (data) {
                    self.set('model', data);
                });
            });
        },

        actions: {
            refreshBuckets: function refreshBuckets(bucketList) {
                var clusterId = bucketList.get('cluster').get('clusterId');
                var bucketTypeId = bucketList.get('bucketTypeId');

                this.get('model').set('isLoaded', false);
                this.get('explorer').bucketCacheRefresh(clusterId, bucketTypeId);
                this.pollForModel(this.get('model'), 3000);
            }
        }
    });

});
define('ember-riak-explorer/controllers/bucket-props', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id'],
        cluster_id: null,
        bucket_type_id: null,
        bucket_id: null
    });

});
define('ember-riak-explorer/controllers/bucket-type', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['cluster_id', 'bucket_type_id'],
        cluster_id: null,
        bucket_type_id: null
    });

});
define('ember-riak-explorer/controllers/cluster', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['cluster_id'],
        cluster_id: null
    });

});
define('ember-riak-explorer/controllers/errors/object-not-found', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id', 'object_key'],
        cluster_id: null,
        bucket_type_id: null,
        bucket_id: null,
        object_key: null
    });

});
define('ember-riak-explorer/controllers/errors/unknown', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Controller.extend({});

});
define('ember-riak-explorer/controllers/explorer-api', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        pageTitle: 'Riak Explorer API'
    });

});
define('ember-riak-explorer/controllers/key-list', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

        queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id'],
        cluster_id: null,
        bucket_type_id: null,
        bucket_id: null,

        // delay in milliseconds
        pollForModel: function pollForModel(bucketList, delay) {
            var self = this;
            Ember['default'].run.later(function () {
                self.refreshModel(bucketList);
            }, delay);
        },

        refreshModel: function refreshModel(keyList) {
            var clusterId = keyList.get('bucket').get('clusterId');
            var bucketTypeId = keyList.get('bucket').get('bucketTypeId');
            var bucketId = keyList.get('bucket').get('bucketId');
            var self = this;

            self.get('explorer').getKeyList(clusterId, bucketTypeId, bucketId, self.store).then(function (updatedModel) {
                updatedModel.then(function (data) {
                    self.set('model', data);
                });
            });
        },

        actions: {
            deleteBucket: function deleteBucket(bucket) {
                this.get('model').set('isLoaded', false);
                this.get('explorer').deleteBucket(bucket);
                // Reload the model after the delete, triggers a cache refresh
                this.pollForModel(this.get('model'), 5000);
                // Reload the second time
                this.pollForModel(this.get('model'), 10000);
            },

            refreshKeys: function refreshKeys(keyList) {
                var clusterId = keyList.get('bucket').get('clusterId');
                var bucketTypeId = keyList.get('bucket').get('bucketTypeId');
                var bucketId = keyList.get('bucket').get('bucketId');

                this.get('model').set('isLoaded', false);
                this.get('explorer').keyCacheRefresh(clusterId, bucketTypeId, bucketId);
                this.pollForModel(this.get('model'), 3000);
            }
        }
    });

});
define('ember-riak-explorer/controllers/node-stats', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['node_id'],
        node_id: null
    });

});
define('ember-riak-explorer/controllers/riak-object-edit', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

        queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id', 'object_key'],
        cluster_id: null,
        bucket_type_id: null,
        bucket_id: null,
        object_key: null,

        actions: {
            saveObject: function saveObject(object) {
                this.get('explorer').saveObject(object);

                this.transitionToRoute('riak-object', { queryParams: {
                        cluster_id: object.get('clusterId'),
                        bucket_type_id: object.get('bucketTypeId'),
                        bucket_id: object.get('bucketId'),
                        object_key: object.get('key')
                    } });
            }
        }
    });

});
define('ember-riak-explorer/controllers/riak-object', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

        queryParams: ['cluster_id', 'bucket_type_id', 'bucket_id', 'object_key'],
        cluster_id: null,
        bucket_type_id: null,
        bucket_id: null,
        object_key: null,

        actions: {
            deleteObject: function deleteObject(object) {
                this.get('explorer').deleteObject(object);
                this.get('explorer').markDeletedKey(object);

                // Once the delete has been issued,
                // return to the bucket's Key List view.
                this.transitionToRoute('key_list', { queryParams: {
                        cluster_id: object.get('clusterId'),
                        bucket_type_id: object.get('bucketTypeId'),
                        bucket_id: object.get('bucketId')
                    } });
            }
        }
    });

});
define('ember-riak-explorer/controllers/riak-ping', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['node_id'],
        node_id: null
    });

});
define('ember-riak-explorer/initializers/app-version', ['exports', 'ember-riak-explorer/config/environment', 'ember'], function (exports, config, Ember) {

  'use strict';

  var classify = Ember['default'].String.classify;
  var registered = false;

  exports['default'] = {
    name: 'App Version',
    initialize: function initialize(container, application) {
      if (!registered) {
        var appName = classify(application.toString());
        Ember['default'].libraries.register(appName, config['default'].APP.version);
        registered = true;
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
    var classifiedName = Ember['default'].String.classify(config['default'].modulePrefix);

    if (config['default'].exportApplicationGlobal && !window[classifiedName]) {
      window[classifiedName] = application;
    }
  }

  ;

  exports['default'] = {
    name: 'export-application-global',

    initialize: initialize
  };

});
define('ember-riak-explorer/initializers/load-bootstrap-config', ['exports', 'ember-riak-explorer/config/environment', 'ember-bootstrap/config'], function (exports, ENV, Config) {

  'use strict';

  exports.initialize = initialize;

  function initialize() {
    Config['default'].load(ENV['default']['ember-bootstrap'] || {});
  }

  exports['default'] = {
    name: 'load-bootstrap-config',
    initialize: initialize
  };
  /* container, application */

});
define('ember-riak-explorer/models/bucket-list', ['exports', 'ember-data', 'ember-riak-explorer/models/cached-list'], function (exports, DS, CachedList) {

    'use strict';

    exports['default'] = CachedList['default'].extend({
        // List of Bucket model instances
        buckets: DS['default'].attr(null, { defaultValue: [] }),

        // Bucket-type model instance
        bucketType: DS['default'].attr(),

        // Cluster model instance
        cluster: DS['default'].attr(),

        bucketTypeId: (function () {
            return this.get('bucketType').get('bucketTypeId');
        }).property('bucketType'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('cluster')
    });

});
define('ember-riak-explorer/models/bucket-type', ['exports', 'ember-data', 'ember-riak-explorer/utils/riak-util'], function (exports, DS, objectToArray) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        name: DS['default'].attr('string'),
        cluster: DS['default'].attr(),

        // {"allow_mult":false, "basic_quorum":false, ... }
        props: DS['default'].attr(),

        bucketTypeId: (function () {
            return this.get('name');
        }).property('name'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('cluster'),

        propsList: (function () {
            if (!this.get('props')) {
                return [];
            }
            return objectToArray['default'](this.get('props'));
        }).property('props')
    });

});
define('ember-riak-explorer/models/bucket', ['exports', 'ember-data', 'ember-riak-explorer/utils/riak-util'], function (exports, DS, objectToArray) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        name: DS['default'].attr('string'),
        cluster: DS['default'].attr(),
        bucketTypeId: DS['default'].attr(),

        // {"allow_mult":false, "basic_quorum":false, ... }
        props: DS['default'].attr(),

        bucketId: (function () {
            return this.get('name');
        }).property('name'),

        clusterId: (function () {
            return this.get('cluster').get('clusterId');
        }).property('cluster'),

        propsList: (function () {
            if (!this.get('props')) {
                return [];
            }
            return objectToArray['default'](this.get('props'));
        }).property('props')
    });

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
define('ember-riak-explorer/models/cluster', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        clusterId: DS['default'].attr('string'),

        // Riak node through which Explorer connects to the riak cluster
        connectedNode: DS['default'].attr('string', { defaultValue: 'riak@127.0.0.1' }),

        // Is this cluster in Dev Mode? Set in the Explorer config file
        // Dev mode allows expensive operations like list keys, delete bucket, etc
        developmentMode: DS['default'].attr('boolean', { defaultValue: false }),

        productionMode: (function () {
            return !this.get('developmentMode');
        }).property('developmentMode'),

        // URL which Explorer uses to forward requests to the Riak cluster
        // Currently in the form of /riak/clusters/$clusterId
        proxyUrl: DS['default'].attr('string')
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

        showDeleteKeys: (function () {
            return this.get('cluster').developmentMode && this.get('keys').length > 0;
        }).property('keys')
    });

});
define('ember-riak-explorer/models/link', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        self: DS['default'].attr(),
        related: DS['default'].attr()
    });

});
define('ember-riak-explorer/models/riak-object', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        key: DS['default'].attr('string'),
        bucket: DS['default'].belongsTo('bucket'),
        headers: DS['default'].attr(),
        contents: DS['default'].attr(),

        // This object was marked as deleted by Explorer UI,
        //  but may show up in key list cache.
        markedDeleted: DS['default'].attr('boolean', { defaultValue: false }),

        bucketId: (function () {
            return this.get('bucket').get('bucketId');
        }).property('bucket'),

        bucketTypeId: (function () {
            return this.get('bucket').get('bucketTypeId');
        }).property('bucket'),

        causalContext: (function () {
            return this.get('headers').other['x-riak-vclock'];
        }).property('headers'),

        clusterId: (function () {
            return this.get('bucket').get('clusterId');
        }).property('bucket'),

        contentType: (function () {
            return this.get('headers').other['content-type'];
        }).property('headers'),

        dateLastModified: (function () {
            return this.get('headers').other['last-modified'];
        }).property('headers'),

        // When this object was loaded from Riak via an HTTP request
        dateLoaded: (function () {
            return this.get('headers').other.date;
        }).property('headers'),

        etag: (function () {
            return this.get('headers').other.etag;
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

});
define('ember-riak-explorer/models/route', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    exports['default'] = DS['default'].Model.extend({
        links: DS['default'].attr(),
        resources: DS['default'].attr()
    });

});
define('ember-riak-explorer/router', ['exports', 'ember', 'ember-riak-explorer/config/environment'], function (exports, Ember, config) {

  'use strict';

  var Router = Ember['default'].Router.extend({
    location: config['default'].locationType
  });

  exports['default'] = Router.map(function () {
    this.route('explorer_api');
    this.route('bucket_type');
    this.route('bucket_list');
    this.route('key_list');
    this.route('riak_ping');
    this.route('node_stats');
    this.route('cluster');
    this.route('riak-object');
    this.route('riak-object-edit');
    this.route('bucket_props');
    this.route('errors', { path: '/errors' }, function () {
      this.route('unknown');
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

        model: function model() {
            return this.explorer.getClusters();
        }
    });

});
define('ember-riak-explorer/routes/bucket-list', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            cluster_id: {
                refreshModel: true
            },
            bucket_type_id: {
                refreshModel: true
            }
        },

        model: function model(params) {
            return this.explorer.getBucketList(params.cluster_id, params.bucket_type_id, this.store);
        },

        setupController: function setupController(controller, model) {
            this._super(controller, model);
            if (!model.get('isLoaded')) {
                controller.pollForModel(model, 3000);
            }
        }
    });

});
define('ember-riak-explorer/routes/bucket-props', ['exports', 'ember'], function (exports, Ember) {

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
            }
        },

        model: function model(params) {
            return this.explorer.getBucketProps(params.cluster_id, params.bucket_type_id, params.bucket_id, this.store);
        }
    });

});
define('ember-riak-explorer/routes/bucket-type', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            cluster_id: {
                refreshModel: true
            },
            bucket_type_id: {
                refreshModel: true
            }
        },

        model: function model(params) {
            return this.explorer.getBucketType(params.cluster_id, params.bucket_type_id, this.store);
        }
    });

});
define('ember-riak-explorer/routes/cluster', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            cluster_id: {
                refreshModel: false
            }
        },

        model: function model(params) {
            return this.explorer.getClusterInfo(params.cluster_id, this.store);
        }
    });

});
define('ember-riak-explorer/routes/errors/object-not-found', ['exports', 'ember'], function (exports, Ember) {

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
define('ember-riak-explorer/routes/errors/unknown', ['exports', 'ember'], function (exports, Ember) {

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
                pingResult: Ember['default'].$.ajax({ url: pingUrl, dataType: 'json' }),
                propsResult: Ember['default'].$.ajax({ url: propsUrl, dataType: 'json' }),
                routes: this.store.find('route')
            });
        }
    });

});
define('ember-riak-explorer/routes/index', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Route.extend({});

});
define('ember-riak-explorer/routes/key-list', ['exports', 'ember'], function (exports, Ember) {

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
            }
        },

        model: function model(params) {
            return this.explorer.getKeyList(params.cluster_id, params.bucket_type_id, params.bucket_id, this.store);
        },

        setupController: function setupController(controller, model) {
            this._super(controller, model);
            if (!model.get('isLoaded')) {
                controller.pollForModel(model, 3000);
            }
        }
    });

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
            var propsResult = Ember['default'].$.ajax(propsUrl, { dataType: 'json' });
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
define('ember-riak-explorer/routes/riak-object-edit', ['exports', 'ember'], function (exports, Ember) {

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
            return this.explorer.getRiakObject(params.cluster_id, params.bucket_type_id, params.bucket_id, params.object_key, this.store);
        }
    });

});
define('ember-riak-explorer/routes/riak-object', ['exports', 'ember'], function (exports, Ember) {

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

        actions: {
            error: function error(_error, transition) {
                if (_error && _error.status === 404) {
                    this.transitionTo('errors.object-not-found', transition);
                } else {
                    // Unknown error, bubble error event up to routes/application.js
                    return true;
                }
            }
        },

        model: function model(params) {
            return this.explorer.getRiakObject(params.cluster_id, params.bucket_type_id, params.bucket_id, params.object_key, this.store);
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
                type: 'POST',
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
        if (contentType.startsWith('text') || contentType === 'application/json' || contentType === 'application/xml' || contentType.startsWith('multipart/mixed')) {
            displayContents = contents;
        }
        return displayContents;
    }

    /**
    * XmlHttpRequest's getAllResponseHeaders() method returns a string of response
    * headers according to the format described here:
    * http://www.w3.org/TR/XMLHttpRequest/#the-getallresponseheaders-method
    *
    * Which we then have to parse. Like savages.
    */
    function parseHeaderString(headerString) {
        var other_headers = {};
        var indexes = [];
        var custom = [];

        if (!headerString) {
            return {
                custom: [], // x-riak-meta-*
                indexes: [], // x-riak-index-*
                other: {} // everything else
            };
        }
        var headerLines = headerString.split('\r\n');

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
    }

    function objectFromAjax(key, bucket, rawHeader, responseText, store) {
        var headers = parseHeaderString(rawHeader);
        var contents = displayContentsForType(headers, responseText);

        return store.createRecord('riak-object', {
            key: key,
            bucket: bucket,
            headers: headers,
            contents: contents
        });
    }

    function deleteBucket(bucket) {
        var url = '/explore/clusters/' + bucket.get('clusterId') + '/bucket_types/' + bucket.get('bucketTypeId') + '/buckets/' + bucket.get('bucketId');

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: 'DELETE',
                url: url
            }).then(function (data, textStatus, jqXHR) {
                resolve(jqXHR.status);
            }, function (jqXHR, textStatus) {
                reject(textStatus);
            });
        });

        return request['catch'](function (error) {
            console.log('Error deleting riak bucket: %O', error);
        });
    }

    function deleteObject(object) {
        var url = getClusterProxyUrl(object.get('clusterId')) + '/types/' + object.get('bucketTypeId') + '/buckets/' + object.get('bucketId') + '/keys/' + object.get('key');

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: 'DELETE',
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

    function createBucketList(data, cluster, bucketTypeId, store) {
        var clusterId = cluster.get('clusterId');
        var bucketList = data.buckets.buckets.map(function (bucketName) {
            return store.createRecord('bucket', {
                name: bucketName,
                clusterId: clusterId,
                bucketTypeId: bucketTypeId
            });
        });
        return store.createRecord('bucket_list', {
            cluster: cluster,
            bucketTypeId: bucketTypeId,
            buckets: bucketList,
            total: data.buckets.total,
            count: data.buckets.count,
            created: data.buckets.created,
            isLoaded: true
        });
    }

    function getBucketList(clusterId, bucketTypeId, store) {
        return getCluster(clusterId, store).then(function (cluster) {
            return getBucketListCache(cluster, bucketTypeId, store);
        });
    }

    function getBucketListCache(cluster, bucketTypeId, store) {
        var clusterId = cluster.get('clusterId');
        var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets';
        var bucketListRequest = Ember['default'].$.ajax(url, { dataType: 'json' });

        return new Ember['default'].RSVP.Promise(function (resolve, reject) {
            return bucketListRequest.then(function (data) {
                // Success, bucket list returned
                resolve(createBucketList(data, cluster, bucketTypeId, store));
            }, function (jqXHR, textStatus) {
                // Fail (likely a 404, cache not yet created)
                if (jqXHR.status === 404) {
                    // Kick off a Cache Refresh, and repeat the getBucketList request
                    bucketCacheRefresh(clusterId, bucketTypeId);
                    // Return an empty (Loading..) list. Controller will poll to
                    // refresh it, later
                    var emptyList = store.createRecord('bucket_list', {
                        cluster: cluster,
                        bucketTypeId: bucketTypeId
                    });
                    resolve(emptyList);
                } else {
                    reject(textStatus);
                }
            });
        });
    }

    function getBucketProps(clusterId, bucketTypeId, bucketId, store) {
        var clusterRequest = getCluster(clusterId, store);
        var propsUrl = getClusterProxyUrl(clusterId) + '/types/' + bucketTypeId + '/buckets/' + bucketId + '/props';
        var propsRequest = Ember['default'].$.ajax(propsUrl, { dataType: 'json' });
        return clusterRequest.then(function (cluster) {
            return propsRequest.then(function (data) {
                return store.createRecord('bucket', {
                    name: bucketId,
                    bucketTypeId: bucketTypeId,
                    cluster: cluster,
                    props: data.props
                });
            });
        });
    }

    function getBucketType(clusterId, bucketTypeId, store) {
        var clusterRequest = getCluster(clusterId, store);
        var propsUrl = getClusterProxyUrl(clusterId) + '/types/' + bucketTypeId + '/props';
        var propsRequest = Ember['default'].$.ajax(propsUrl, { dataType: 'json' });
        return clusterRequest.then(function (cluster) {
            return propsRequest.then(function (data) {
                return store.createRecord('bucket_type', {
                    name: bucketTypeId,
                    cluster: cluster,
                    props: data.props
                });
            });
        });
    }

    function getCluster(clusterId, store) {
        var url = '/explore/clusters/' + clusterId;
        var result = Ember['default'].$.ajax({ url: url }); // returns a Promise obj

        return result.then(function (data) {
            return store.createRecord('cluster', {
                clusterId: data.cluster.id,
                developmentMode: data.cluster.development_mode,
                connectedNode: data.cluster.riak_node,
                proxyUrl: getClusterProxyUrl(clusterId)
            });
        });
    }

    function getClusterInfo(clusterId, store) {
        var cluster = getCluster(clusterId, store);
        var nodes = getNodes(clusterId);
        var indexes = getIndexes(clusterId);
        var bucketTypes = store.find('bucket_type', { cluster_id: clusterId });
        var clusterInfo = {
            cluster: cluster,
            nodes: nodes,
            indexes: indexes,
            bucketTypes: bucketTypes
        };

        return new Ember['default'].RSVP.hash(clusterInfo);
    }

    function getClusters() {
        var url = '/explore/clusters/';
        var result = Ember['default'].$.ajax({ url: url }); // returns a Promise obj
        return result.then(
        // Success
        function (data) {
            return data.clusters;
        },
        // Error
        function (error) {
            console.log('Error fetching clusters: ' + error);
            return [];
        });
    }

    function getClustersAndNodes() {
        return getClusters().then(function (clustersList) {
            return Ember['default'].RSVP.all(clustersList.map(function (cluster) {
                return new Ember['default'].RSVP.hash({
                    id: cluster.id,
                    props: cluster.props,
                    nodes: getNodes(cluster.id)
                });
            }));
        });
    }

    function getClusterProxyUrl(clusterId) {
        return '/riak/clusters/' + clusterId;
    }

    function getIndexes(clusterId) {
        var url = getClusterProxyUrl(clusterId) + '/search/index';

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: 'GET',
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

    function getBucket(clusterId, bucketTypeId, bucketId, store) {
        return getCluster(clusterId, store).then(function (cluster) {
            return store.createRecord('bucket', {
                name: bucketId,
                bucketTypeId: bucketTypeId,
                cluster: cluster,
                clusterId: clusterId
            });
        });
    }

    function getKeyList(clusterId, bucketTypeId, bucketId, store) {
        var url = '/explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets/' + bucketId + '/keys';
        var keyListRequest = new Ember['default'].RSVP.Promise(function (resolve) {
            Ember['default'].$.ajax(url, { dataType: 'json' }).then(function (data) {
                // Success
                resolve(data);
            }, function (jqXHR) {
                // Error
                if (jqXHR.status === 404) {
                    // Empty cache (need to kick off a refresh)
                    keyCacheRefresh(clusterId, bucketTypeId, bucketId);
                    // Results in returning an empty (Loading..) key list
                    resolve(null);
                } else {
                    // Some other error
                    // reject(textStatus);
                    resolve(null);
                }
            });
        });
        var explorerService = this;

        return getBucket(clusterId, bucketTypeId, bucketId, store).then(function (bucket) {
            return keyListRequest.then(function (data) {
                if (!data) {
                    return store.createRecord('key-list', {
                        bucket: bucket,
                        cluster: bucket.get('cluster')
                    });
                }
                var keyList = data.keys.keys.map(function (key) {
                    var obj = store.createRecord('riak-object', {
                        key: key,
                        bucket: bucket
                    });
                    if (explorerService.wasKeyDeleted(obj)) {
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
                    total: data.keys.total,
                    isLoaded: true
                });
            });
        });
    }

    function getNodes(clusterId) {
        var url = '/explore/clusters/' + clusterId + '/nodes';
        var result = Ember['default'].$.ajax({ url: url }); // returns a Promise obj
        return result.then(
        // Success
        function (data) {
            return data.nodes;
        },
        // Error
        function (error) {
            console.log('Error fetching nodes: ' + error);
            return [];
        });
    }

    function getRiakObject(clusterId, bucketTypeId, bucketId, key, store) {
        var url = getClusterProxyUrl(clusterId) + '/types/' + bucketTypeId + '/buckets/' + bucketId + '/keys/' + key;

        var bucket = store.createRecord('bucket', {
            name: bucketId,
            bucketTypeId: bucketTypeId,
            clusterId: clusterId
        });

        var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
            Ember['default'].$.ajax({
                type: 'GET',
                processData: false,
                cache: false,
                url: url,
                headers: { Accept: '*/*, multipart/mixed' }
            }).then(function (data, textStatus, jqXHR) {
                var headerString = jqXHR.getAllResponseHeaders();
                resolve(objectFromAjax(key, bucket, headerString, jqXHR.responseText, store));
            }, function (jqXHR, textStatus) {
                var headerString;
                if (jqXHR.status === 200 && textStatus === 'parsererror') {
                    // jQuery tries to parse JSON objects, and throws
                    // parse errors when they're invalid. Suppress this.
                    headerString = jqXHR.getAllResponseHeaders();
                    resolve(objectFromAjax(key, bucket, headerString, jqXHR.responseText, store));
                }
                if (jqXHR.status === 300) {
                    // Handle 300 Multiple Choices case for siblings
                    headerString = jqXHR.getAllResponseHeaders();
                    resolve(objectFromAjax(key, bucket, headerString, jqXHR.responseText, store));
                } else {
                    reject(jqXHR);
                }
            });
        });

        return request.then(function (request) {
            return new Ember['default'].RSVP.hash({
                obj: request,
                url: url
            });
        });
        // .catch(function(error) {
        //     console.log('Error fetching riak object: %O', error);
        // });
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

    function wasKeyDeleted(object) {
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
                type: 'PUT',
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

        deletedCacheFor: deletedCacheFor,

        deleteObject: deleteObject,

        deleteBucket: deleteBucket,

        getBucketList: getBucketList,

        getBucketProps: getBucketProps,

        getBucketType: getBucketType,

        // Return the details for a single cluster
        getClusterInfo: getClusterInfo,

        // Return all clusters that Explorer knows about
        getClusters: getClusters,

        // Return a list of clusters with their nodes, in the form:
        // [ { id: 'default', props: [], nodes: [ <list of nodes> ] }, ... ]
        getClustersAndNodes: getClustersAndNodes,

        getClusterProxyUrl: getClusterProxyUrl,

        getKeyList: getKeyList,

        // Return all nodes for a particular cluster
        getNodes: getNodes,

        getRiakObject: getRiakObject,

        keyCacheRefresh: keyCacheRefresh,

        markDeletedKey: markDeletedKey,

        saveObject: saveObject,

        wasKeyDeleted: wasKeyDeleted
    });

});
define('ember-riak-explorer/templates/application', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("a");
          dom.setAttribute(el1,"href","/explorer_api");
          var el2 = dom.createTextNode("Explorer API");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child2 = (function() {
      var child0 = (function() {
        var child0 = (function() {
          var child0 = (function() {
            return {
              isHTMLBars: true,
              revision: "Ember@1.11.1",
              blockParams: 0,
              cachedFragment: null,
              hasRendered: false,
              build: function build(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createElement("strong");
                var el2 = dom.createTextNode("(Dev mode)");
                dom.appendChild(el1, el2);
                dom.appendChild(el0, el1);
                return el0;
              },
              render: function render(context, env, contextualElement) {
                var dom = env.dom;
                dom.detectNamespace(contextualElement);
                var fragment;
                if (env.useFragmentCache && dom.canClone) {
                  if (this.cachedFragment === null) {
                    fragment = this.build(dom);
                    if (this.hasRendered) {
                      this.cachedFragment = fragment;
                    } else {
                      this.hasRendered = true;
                    }
                  }
                  if (this.cachedFragment) {
                    fragment = dom.cloneNode(this.cachedFragment, true);
                  }
                } else {
                  fragment = this.build(dom);
                }
                return fragment;
              }
            };
          }());
          return {
            isHTMLBars: true,
            revision: "Ember@1.11.1",
            blockParams: 0,
            cachedFragment: null,
            hasRendered: false,
            build: function build(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("                        ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n                      ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            render: function render(context, env, contextualElement) {
              var dom = env.dom;
              var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
              dom.detectNamespace(contextualElement);
              var fragment;
              if (env.useFragmentCache && dom.canClone) {
                if (this.cachedFragment === null) {
                  fragment = this.build(dom);
                  if (this.hasRendered) {
                    this.cachedFragment = fragment;
                  } else {
                    this.hasRendered = true;
                  }
                }
                if (this.cachedFragment) {
                  fragment = dom.cloneNode(this.cachedFragment, true);
                }
              } else {
                fragment = this.build(dom);
              }
              var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
              var morph1 = dom.createMorphAt(fragment,3,3,contextualElement);
              content(env, morph0, context, "cluster.id");
              block(env, morph1, context, "if", [get(env, context, "cluster.development_mode")], {}, child0, null);
              return fragment;
            }
          };
        }());
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
            dom.insertBoundary(fragment, null);
            dom.insertBoundary(fragment, 0);
            block(env, morph0, context, "link-to", ["cluster", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "cluster.id")})], {}, child0, null);
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, null);
          dom.insertBoundary(fragment, 0);
          set(env, context, "cluster", blockArguments[0]);
          block(env, morph0, context, "link-to", ["cluster"], {"tagName": "li"}, child0, null);
          return fragment;
        }
      };
    }());
    var child3 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("a");
          dom.setAttribute(el1,"href","/ping");
          var el2 = dom.createTextNode("Explorer API");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, block = hooks.block, get = hooks.get, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [2, 1]);
        var element1 = dom.childAt(element0, [1, 1]);
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0, 1, 3, 1]),1,1);
        var morph1 = dom.createMorphAt(element1,1,1);
        var morph2 = dom.createMorphAt(element1,3,3);
        var morph3 = dom.createMorphAt(element1,5,5);
        var morph4 = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
        block(env, morph0, context, "link-to", ["explorer_api"], {"tagName": "li"}, child0, null);
        block(env, morph1, context, "link-to", ["index"], {"tagName": "li"}, child1, null);
        block(env, morph2, context, "each", [get(env, context, "model")], {}, child2, null);
        block(env, morph3, context, "link-to", ["explorer_api"], {"tagName": "li"}, child3, null);
        content(env, morph4, context, "outlet");
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/bucket-list', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, get = hooks.get, inline = hooks.inline;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
            inline(env, morph0, context, "refresh-buckets", [], {"action": "refreshBuckets", "bucketList": get(env, context, "model")});
            return fragment;
          }
        };
      }());
      var child1 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1, 1, 1]);
          var element1 = dom.childAt(fragment, [3]);
          var morph0 = dom.createMorphAt(element0,1,1);
          var morph1 = dom.createMorphAt(element0,3,3);
          var morph2 = dom.createMorphAt(dom.childAt(element1, [1]),0,0);
          var morph3 = dom.createMorphAt(dom.childAt(element1, [3]),0,0);
          var morph4 = dom.createMorphAt(dom.childAt(fragment, [5]),1,1);
          content(env, morph0, context, "model.created");
          block(env, morph1, context, "if", [get(env, context, "model.cluster.developmentMode")], {}, child0, null);
          content(env, morph2, context, "model.count");
          content(env, morph3, context, "model.total");
          block(env, morph4, context, "riak-buckets", [], {"bucketList": get(env, context, "model"), "cluster": get(env, context, "model.cluster"), "bucketTypeI": get(env, context, "model.bucketTypeId")}, child1, null);
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","padding-top: 2em; text-align: center;");
          var el2 = dom.createTextNode("\n    Loading...\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("h4");
        var el2 = dom.createTextNode("Bucket List: ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, inline = hooks.inline, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        var morph1 = dom.createMorphAt(fragment,2,2,contextualElement);
        var morph2 = dom.createMorphAt(fragment,4,4,contextualElement);
        dom.insertBoundary(fragment, null);
        content(env, morph0, context, "model.bucketTypeId");
        inline(env, morph1, context, "crumb-trail", [], {"model": get(env, context, "model")});
        block(env, morph2, context, "if", [get(env, context, "model.isLoaded")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/bucket-props', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          var morph1 = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          set(env, context, "prop", blockArguments[0]);
          content(env, morph0, context, "prop.key");
          content(env, morph1, context, "prop.value");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("h3");
        var el2 = dom.createTextNode("Bucket Properties: ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, inline = hooks.inline, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        var morph1 = dom.createMorphAt(fragment,2,2,contextualElement);
        var morph2 = dom.createMorphAt(dom.childAt(fragment, [4, 3]),1,1);
        content(env, morph0, context, "model.bucketId");
        inline(env, morph1, context, "crumb-trail", [], {"model": get(env, context, "model")});
        block(env, morph2, context, "each", [get(env, context, "model.propsList")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/bucket-type', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          var morph1 = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          set(env, context, "prop", blockArguments[0]);
          content(env, morph0, context, "prop.key");
          content(env, morph1, context, "prop.value");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("h3");
        var el2 = dom.createTextNode("Bucket Type Properties: ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, inline = hooks.inline, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        var morph1 = dom.createMorphAt(fragment,2,2,contextualElement);
        var morph2 = dom.createMorphAt(dom.childAt(fragment, [4, 3]),1,1);
        content(env, morph0, context, "model.bucketTypeId");
        inline(env, morph1, context, "crumb-trail", [], {"model": get(env, context, "model")});
        block(env, morph2, context, "each", [get(env, context, "model.propsList")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/cluster', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("            Development Mode\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("            Production Mode\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child2 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child3 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, get = hooks.get, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,3,3,contextualElement);
          dom.insertBoundary(fragment, null);
          block(env, morph0, context, "search-indexes", [], {"indexes": get(env, context, "model.indexes"), "clusterProxyUrl": get(env, context, "model.cluster.proxyUrl")}, child0, null);
          return fragment;
        }
      };
    }());
    var child4 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                No search indexes found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child5 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("      ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
          set(env, context, "node", blockArguments[0]);
          inline(env, morph0, context, "riak-node", [], {"node": get(env, context, "node")});
          return fragment;
        }
      };
    }());
    var child6 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("      No nodes detected.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createElement("h3");
        var el5 = dom.createTextNode("Cluster: ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
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
        var el4 = dom.createTextNode("\n            Status:\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
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
        var el4 = dom.createElement("h4");
        var el5 = dom.createTextNode("Explore");
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
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("h5");
        var el5 = dom.createTextNode("Bucket Types");
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
        var el2 = dom.createTextNode("\n    ");
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
        var el8 = dom.createTextNode("Node");
        dom.appendChild(el7, el8);
        dom.appendChild(el6, el7);
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0]);
        var morph0 = dom.createMorphAt(dom.childAt(element0, [1, 1, 0]),1,1);
        var morph1 = dom.createMorphAt(dom.childAt(element0, [3, 1, 1]),1,1);
        var morph2 = dom.createMorphAt(dom.childAt(element0, [7, 1]),3,3);
        var morph3 = dom.createMorphAt(dom.childAt(element0, [11, 1]),1,1);
        var morph4 = dom.createMorphAt(dom.childAt(element0, [13, 1, 3, 3]),1,1);
        content(env, morph0, context, "model.cluster.clusterId");
        block(env, morph1, context, "if", [get(env, context, "model.cluster.developmentMode")], {}, child0, child1);
        block(env, morph2, context, "bucket-types", [], {"clusterId": get(env, context, "model.cluster.clusterId"), "bucketTypes": get(env, context, "model.bucketTypes")}, child2, null);
        block(env, morph3, context, "if", [get(env, context, "model.indexes")], {}, child3, child4);
        block(env, morph4, context, "each", [get(env, context, "model.nodes")], {}, child5, child6);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-alert', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
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
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, element = hooks.element;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var element0 = dom.childAt(fragment, [1]);
            element(env, element0, context, "action", ["dismiss"], {});
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, get = hooks.get, block = hooks.block, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
          var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
          dom.insertBoundary(fragment, 0);
          block(env, morph0, context, "if", [get(env, context, "dismissible")], {}, child0, null);
          content(env, morph1, context, "yield");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "unless", [get(env, context, "dismissed")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-button', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createElement("i");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(" ");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [0]);
          element(env, element0, context, "bind-attr", [], {"class": "icon"});
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
        var morph2 = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "icon")], {}, child0, null);
        content(env, morph1, context, "text");
        content(env, morph2, context, "yield");
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-form-group', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          element(env, element0, context, "bind-attr", [], {"class": ":form-control-feedback iconName"});
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        content(env, morph0, context, "yield");
        block(env, morph1, context, "if", [get(env, context, "hasFeedback")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/bs-form', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        content(env, morph0, context, "yield");
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/bucket-types', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                        View Properties");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      var child1 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                        Bucket List");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
          dom.setAttribute(el5,"class","col-md-4");
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n                ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          dom.setAttribute(el5,"class","col-md-3");
          var el6 = dom.createTextNode("\n                    ");
          dom.appendChild(el5, el6);
          var el6 = dom.createElement("strong");
          var el7 = dom.createTextNode("Active:");
          dom.appendChild(el6, el7);
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode(" ");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode(",\n                    ");
          dom.appendChild(el5, el6);
          var el6 = dom.createElement("strong");
          var el7 = dom.createTextNode("allow_mult:");
          dom.appendChild(el6, el7);
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode(" ");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode("\n                ");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n                ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          dom.setAttribute(el5,"class","col-md-5");
          var el6 = dom.createTextNode("\n                    ");
          dom.appendChild(el5, el6);
          var el6 = dom.createElement("strong");
          var el7 = dom.createTextNode("Actions:");
          dom.appendChild(el6, el7);
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode("\n");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
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
          var el4 = dom.createTextNode("\n            ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("div");
          dom.setAttribute(el4,"class","row");
          var el5 = dom.createTextNode("\n                ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          dom.setAttribute(el5,"class","col-md-12");
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, content = hooks.content, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1, 1, 1, 1]);
          var element1 = dom.childAt(element0, [3]);
          var element2 = dom.childAt(element0, [5]);
          var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          var morph1 = dom.createMorphAt(element1,3,3);
          var morph2 = dom.createMorphAt(element1,7,7);
          var morph3 = dom.createMorphAt(element2,3,3);
          var morph4 = dom.createMorphAt(element2,5,5);
          set(env, context, "bt", blockArguments[0]);
          content(env, morph0, context, "bt.id");
          content(env, morph1, context, "bt.props.active");
          content(env, morph2, context, "bt.props.allow_mult");
          block(env, morph3, context, "link-to", ["bucket_type", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "clusterId"), "bucket_type_id": get(env, context, "bt.id")})], {"classNames": "btn btn-xs btn-primary"}, child0, null);
          block(env, morph4, context, "link-to", ["bucket_list", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "clusterId"), "bucket_type_id": get(env, context, "bt.id")})], {"classNames": "btn btn-xs btn-primary"}, child1, null);
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    No bucket types. Have you specified a node?\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0, 1]),1,1);
        block(env, morph0, context, "each", [get(env, context, "bucketTypes")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/crumb-trail', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
          dom.insertBoundary(fragment, null);
          content(env, morph0, context, "model.clusterId");
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, content = hooks.content;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
            dom.insertBoundary(fragment, null);
            content(env, morph0, context, "model.bucketTypeId");
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                /\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(" bucket type\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
          block(env, morph0, context, "link-to", ["bucket_list", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "model.clusterId"), "bucket_type_id": get(env, context, "model.bucketTypeId")})], {}, child0, null);
          return fragment;
        }
      };
    }());
    var child2 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, content = hooks.content;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
            dom.insertBoundary(fragment, null);
            content(env, morph0, context, "model.bucketId");
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                /\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(" bucket\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
          block(env, morph0, context, "link-to", ["key_list", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "model.clusterId"), "bucket_type_id": get(env, context, "model.bucketTypeId"), "bucket_id": get(env, context, "model.bucketId")})], {}, child0, null);
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
        var el4 = dom.createTextNode("\n            in\n");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode(" cluster\n");
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0, 1, 3]);
        var morph0 = dom.createMorphAt(element0,1,1);
        var morph1 = dom.createMorphAt(element0,3,3);
        var morph2 = dom.createMorphAt(element0,4,4);
        block(env, morph0, context, "link-to", ["cluster", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "model.clusterId")})], {}, child0, null);
        block(env, morph1, context, "if", [get(env, context, "model.bucketTypeId")], {}, child1, null);
        block(env, morph2, context, "if", [get(env, context, "model.bucketId")], {}, child2, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/delete-object', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-xs btn-danger");
        var el2 = dom.createTextNode("\n    Delete");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, element = hooks.element;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0]);
        element(env, element0, context, "action", ["deleteObject", get(env, context, "object")], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/edit-object', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    Edit Object");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "link-to", ["riak-object-edit", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "object.clusterId"), "bucket_type_id": get(env, context, "object.bucketTypeId"), "bucket_id": get(env, context, "object.bucketId"), "object_key": get(env, context, "object.key")})], {"classNames": "btn btn-xs btn-primary"}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/errors', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          content(env, morph0, context, "errors.firstObject");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "showErrors")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/feedback-icon', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          element(env, element0, context, "bind-attr", [], {"class": ":form-control-feedback iconName"});
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasFeedback")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/checkbox', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, element = hooks.element, get = hooks.get, inline = hooks.inline, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0]);
        var element1 = dom.childAt(element0, [1, 1]);
        var morph0 = dom.createMorphAt(element1,1,1);
        var morph1 = dom.createMorphAt(element1,3,3);
        var morph2 = dom.createMorphAt(element0,3,3);
        element(env, element0, context, "bind-attr", [], {"class": "horizontalInputGridClass horizontalInputOffsetGridClass"});
        inline(env, morph0, context, "input", [], {"name": get(env, context, "name"), "type": "checkbox", "checked": get(env, context, "value")});
        content(env, morph1, context, "label");
        inline(env, morph2, context, "partial", ["components/form-element/errors"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/default', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, content = hooks.content, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morph0 = dom.createMorphAt(element1,0,0);
          var morph1 = dom.createMorphAt(element2,1,1);
          var morph2 = dom.createMorphAt(element2,3,3);
          var morph3 = dom.createMorphAt(element2,5,5);
          element(env, element1, context, "bind-attr", [], {"class": ":control-label horizontalLabelGridClass"});
          content(env, morph0, context, "label");
          element(env, element2, context, "bind-attr", [], {"class": "horizontalInputGridClass"});
          inline(env, morph1, context, "bs-input", [], {"name": get(env, context, "name"), "type": get(env, context, "controlType"), "value": get(env, context, "value"), "placeholder": get(env, context, "placeholder")});
          inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph3, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(element0,1,1);
          var morph1 = dom.createMorphAt(element0,3,3);
          var morph2 = dom.createMorphAt(element0,5,5);
          element(env, element0, context, "bind-attr", [], {"class": "horizontalInputGridClass horizontalInputOffsetGridClass"});
          inline(env, morph0, context, "bs-input", [], {"name": get(env, context, "name"), "type": get(env, context, "controlType"), "value": get(env, context, "value"), "placeholder": get(env, context, "placeholder")});
          inline(env, morph1, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph2, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/select', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, content = hooks.content, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morph0 = dom.createMorphAt(element1,0,0);
          var morph1 = dom.createMorphAt(element2,1,1);
          var morph2 = dom.createMorphAt(element2,3,3);
          var morph3 = dom.createMorphAt(element2,5,5);
          element(env, element1, context, "bind-attr", [], {"class": ":control-label horizontalLabelGridClass"});
          content(env, morph0, context, "label");
          element(env, element2, context, "bind-attr", [], {"class": "horizontalInputGridClass"});
          inline(env, morph1, context, "bs-select", [], {"name": get(env, context, "name"), "content": get(env, context, "choices"), "optionValuePath": get(env, context, "selectValueProperty"), "optionLabelPath": get(env, context, "selectLabelProperty"), "value": get(env, context, "value")});
          inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph3, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(element0,1,1);
          var morph1 = dom.createMorphAt(element0,3,3);
          var morph2 = dom.createMorphAt(element0,5,5);
          element(env, element0, context, "bind-attr", [], {"class": "horizontalInputGridClass horizontalInputOffsetGridClass"});
          inline(env, morph0, context, "bs-select", [], {"name": get(env, context, "name"), "content": get(env, context, "choices"), "optionValuePath": get(env, context, "selectValueProperty"), "optionLabelPath": get(env, context, "selectLabelProperty"), "value": get(env, context, "value")});
          inline(env, morph1, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph2, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/select2', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, content = hooks.content, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morph0 = dom.createMorphAt(element1,0,0);
          var morph1 = dom.createMorphAt(element2,1,1);
          var morph2 = dom.createMorphAt(element2,3,3);
          var morph3 = dom.createMorphAt(element2,5,5);
          element(env, element1, context, "bind-attr", [], {"class": ":control-label horizontalLabelGridClass"});
          content(env, morph0, context, "label");
          element(env, element2, context, "bind-attr", [], {"class": "horizontalInputGridClass"});
          inline(env, morph1, context, "select-2", [], {"name": get(env, context, "name"), "content": get(env, context, "choices"), "optionValuePath": get(env, context, "choiceValueProperty"), "optionLabelPath": get(env, context, "choiceLabelProperty"), "value": get(env, context, "value"), "searchEnabled": false});
          inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph3, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(element0,1,1);
          var morph1 = dom.createMorphAt(element0,3,3);
          var morph2 = dom.createMorphAt(element0,5,5);
          element(env, element0, context, "bind-attr", [], {"class": "horizontalInputGridClass horizontalInputOffsetGridClass"});
          inline(env, morph0, context, "select-2", [], {"name": get(env, context, "name"), "content": get(env, context, "choices"), "optionValuePath": get(env, context, "choiceValueProperty"), "optionLabelPath": get(env, context, "choiceLabelProperty"), "value": get(env, context, "value"), "searchEnabled": false});
          inline(env, morph1, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph2, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/horizontal/textarea', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, content = hooks.content, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element1 = dom.childAt(fragment, [1]);
          var element2 = dom.childAt(fragment, [3]);
          var morph0 = dom.createMorphAt(element1,0,0);
          var morph1 = dom.createMorphAt(element2,1,1);
          var morph2 = dom.createMorphAt(element2,3,3);
          var morph3 = dom.createMorphAt(element2,5,5);
          element(env, element1, context, "bind-attr", [], {"class": ":control-label horizontalLabelGridClass"});
          content(env, morph0, context, "label");
          element(env, element2, context, "bind-attr", [], {"class": "horizontalInputGridClass"});
          inline(env, morph1, context, "bs-textarea", [], {"name": get(env, context, "name"), "value": get(env, context, "value"), "placeholder": get(env, context, "placeholder"), "cols": get(env, context, "cols"), "rows": get(env, context, "rows")});
          inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph3, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, element = hooks.element, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(element0,1,1);
          var morph1 = dom.createMorphAt(element0,3,3);
          var morph2 = dom.createMorphAt(element0,5,5);
          element(env, element0, context, "bind-attr", [], {"class": "horizontalInputGridClass horizontalInputOffsetGridClass"});
          inline(env, morph0, context, "bs-textarea", [], {"name": get(env, context, "name"), "value": get(env, context, "value"), "placeholder": get(env, context, "placeholder"), "cols": get(env, context, "cols"), "rows": get(env, context, "rows")});
          inline(env, morph1, context, "partial", ["components/form-element/feedback-icon"], {});
          inline(env, morph2, context, "partial", ["components/form-element/errors"], {});
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/checkbox', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, inline = hooks.inline, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0, 1]);
        var morph0 = dom.createMorphAt(element0,1,1);
        var morph1 = dom.createMorphAt(element0,3,3);
        inline(env, morph0, context, "input", [], {"name": get(env, context, "name"), "type": "checkbox", "checked": get(env, context, "value")});
        content(env, morph1, context, "label");
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/default', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          content(env, morph0, context, "label");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
        var morph2 = dom.createMorphAt(fragment,3,3,contextualElement);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, null);
        inline(env, morph1, context, "bs-input", [], {"name": get(env, context, "name"), "type": get(env, context, "controlType"), "value": get(env, context, "value"), "placeholder": get(env, context, "placeholder")});
        inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/select', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          content(env, morph0, context, "label");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
        var morph2 = dom.createMorphAt(fragment,3,3,contextualElement);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, null);
        inline(env, morph1, context, "bs-select", [], {"name": get(env, context, "name"), "content": get(env, context, "choices"), "optionValuePath": get(env, context, "selectValueProperty"), "optionLabelPath": get(env, context, "selectLabelProperty"), "value": get(env, context, "value")});
        inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/inline/textarea', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          content(env, morph0, context, "label");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
        var morph2 = dom.createMorphAt(fragment,3,3,contextualElement);
        var morph3 = dom.createMorphAt(fragment,5,5,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, null);
        inline(env, morph1, context, "bs-textarea", [], {"name": get(env, context, "name"), "value": get(env, context, "value"), "placeholder": get(env, context, "placeholder"), "cols": get(env, context, "cols"), "rows": get(env, context, "rows")});
        inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
        inline(env, morph3, context, "partial", ["components/form-element/errors"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/checkbox', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, inline = hooks.inline, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0, 1]);
        var morph0 = dom.createMorphAt(element0,1,1);
        var morph1 = dom.createMorphAt(element0,3,3);
        var morph2 = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        inline(env, morph0, context, "input", [], {"name": get(env, context, "name"), "type": "checkbox", "checked": get(env, context, "value")});
        content(env, morph1, context, "label");
        inline(env, morph2, context, "partial", ["components/form-element/errors"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/default', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          content(env, morph0, context, "label");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
        var morph2 = dom.createMorphAt(fragment,3,3,contextualElement);
        var morph3 = dom.createMorphAt(fragment,5,5,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, null);
        inline(env, morph1, context, "bs-input", [], {"name": get(env, context, "name"), "type": get(env, context, "controlType"), "value": get(env, context, "value"), "placeholder": get(env, context, "placeholder")});
        inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
        inline(env, morph3, context, "partial", ["components/form-element/errors"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/select', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          content(env, morph0, context, "label");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
        var morph2 = dom.createMorphAt(fragment,3,3,contextualElement);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, null);
        inline(env, morph1, context, "bs-select", [], {"name": get(env, context, "name"), "content": get(env, context, "choices"), "optionValuePath": get(env, context, "selectValueProperty"), "optionLabelPath": get(env, context, "selectLabelProperty"), "value": get(env, context, "value")});
        inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/form-element/vertical/textarea', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          content(env, morph0, context, "label");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(fragment,1,1,contextualElement);
        var morph2 = dom.createMorphAt(fragment,3,3,contextualElement);
        var morph3 = dom.createMorphAt(fragment,5,5,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "hasLabel")], {}, child0, null);
        inline(env, morph1, context, "bs-textarea", [], {"value": get(env, context, "value"), "name": get(env, context, "name"), "placeholder": get(env, context, "placeholder"), "cols": get(env, context, "cols"), "rows": get(env, context, "rows")});
        inline(env, morph2, context, "partial", ["components/form-element/feedback-icon"], {});
        inline(env, morph3, context, "partial", ["components/form-element/errors"], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-headers-edit', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 1,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
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
          render: function render(context, env, contextualElement, blockArguments) {
            var dom = env.dom;
            var hooks = env.hooks, set = hooks.set, content = hooks.content, get = hooks.get, inline = hooks.inline;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var element0 = dom.childAt(fragment, [1]);
            var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
            var morph1 = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
            set(env, context, "header", blockArguments[0]);
            content(env, morph0, context, "header.key");
            inline(env, morph1, context, "input", [], {"value": get(env, context, "header.value"), "id": get(env, context, "header.key"), "class": "form-control"});
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          var morph1 = dom.createMorphAt(dom.childAt(fragment, [3, 1]),1,1);
          content(env, morph0, context, "title");
          block(env, morph1, context, "each", [get(env, context, "headers")], {}, child0, null);
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "headers")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-headers', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 1,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
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
          render: function render(context, env, contextualElement, blockArguments) {
            var dom = env.dom;
            var hooks = env.hooks, set = hooks.set, content = hooks.content;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var element0 = dom.childAt(fragment, [1]);
            var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
            var morph1 = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
            set(env, context, "header", blockArguments[0]);
            content(env, morph0, context, "header.key");
            content(env, morph1, context, "header.value");
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          var morph1 = dom.createMorphAt(dom.childAt(fragment, [3, 1]),1,1);
          content(env, morph0, context, "title");
          block(env, morph1, context, "each", [get(env, context, "headers")], {}, child0, null);
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, null);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "if", [get(env, context, "headers")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-location', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                    Cancel");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,3,3,contextualElement);
          block(env, morph0, context, "link-to", ["riak-object", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "object.clusterId"), "bucket_type_id": get(env, context, "object.bucketTypeId"), "bucket_id": get(env, context, "object.bucketId"), "object_key": get(env, context, "object.key")})], {"classNames": "btn btn-xs btn-default"}, child0, null);
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, get = hooks.get, inline = hooks.inline;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
          inline(env, morph0, context, "edit-object", [], {"object": get(env, context, "object")});
          return fragment;
        }
      };
    }());
    var child2 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child3 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                    ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, get = hooks.get, inline = hooks.inline;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
            inline(env, morph0, context, "delete-object", [], {"action": "deleteObject", "object": get(env, context, "object")});
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                No\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, get = hooks.get, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
          dom.insertBoundary(fragment, null);
          block(env, morph0, context, "unless", [get(env, context, "isEditing")], {}, child0, null);
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
        dom.setAttribute(el3,"class","col-md-8");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
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
        dom.setAttribute(el3,"class","col-md-4");
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
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createElement("br");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0, 1]);
        var element1 = dom.childAt(element0, [1]);
        var element2 = dom.childAt(fragment, [8, 1]);
        var morph0 = dom.createMorphAt(dom.childAt(element1, [1]),0,0);
        var morph1 = dom.createMorphAt(element1,3,3);
        var morph2 = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
        var morph3 = dom.createMorphAt(fragment,4,4,contextualElement);
        var morph4 = dom.createMorphAt(dom.childAt(element2, [1]),3,3);
        var morph5 = dom.createMorphAt(dom.childAt(element2, [3]),1,1);
        content(env, morph0, context, "object.key");
        block(env, morph1, context, "if", [get(env, context, "isEditing")], {}, child0, child1);
        block(env, morph2, context, "if", [get(env, context, "object.isDeleted")], {}, child2, child3);
        inline(env, morph3, context, "crumb-trail", [], {"model": get(env, context, "object")});
        content(env, morph4, context, "object.dateLastModified");
        content(env, morph5, context, "object.dateLoaded");
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-version', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [2, 3]);
        var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),3,3);
        var morph1 = dom.createMorphAt(dom.childAt(element0, [3]),3,3);
        content(env, morph0, context, "object.causalContext");
        content(env, morph1, context, "object.etag");
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/refresh-buckets', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-xs btn-primary");
        var el2 = dom.createTextNode("\n    Refresh Bucket Cache");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, element = hooks.element;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0]);
        element(env, element0, context, "action", ["refreshBuckets", get(env, context, "bucketList")], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/refresh-keys', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-xs btn-primary");
        var el2 = dom.createTextNode("\n    Refresh Key Cache");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, element = hooks.element;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0]);
        element(env, element0, context, "action", ["refreshKeys", get(env, context, "keyList")], {});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/riak-buckets', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                        View Properties");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      var child1 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                        Key List");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
          dom.setAttribute(el5,"class","col-md-6");
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
          dom.appendChild(el4, el5);
          var el5 = dom.createTextNode("\n                ");
          dom.appendChild(el4, el5);
          var el5 = dom.createElement("div");
          dom.setAttribute(el5,"class","col-md-4");
          var el6 = dom.createTextNode("\n                    ");
          dom.appendChild(el5, el6);
          var el6 = dom.createElement("strong");
          var el7 = dom.createTextNode("Actions:");
          dom.appendChild(el6, el7);
          dom.appendChild(el5, el6);
          var el6 = dom.createTextNode("\n");
          dom.appendChild(el5, el6);
          var el6 = dom.createComment("");
          dom.appendChild(el5, el6);
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, content = hooks.content, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1, 1, 1, 1]);
          var element1 = dom.childAt(element0, [3]);
          var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          var morph1 = dom.createMorphAt(element1,3,3);
          var morph2 = dom.createMorphAt(element1,5,5);
          set(env, context, "bucket", blockArguments[0]);
          content(env, morph0, context, "bucket.name");
          block(env, morph1, context, "link-to", ["bucket_props", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "cluster.clusterId"), "bucket_type_id": get(env, context, "bucket.bucketTypeId"), "bucket_id": get(env, context, "bucket.bucketId")})], {"classNames": "btn btn-xs btn-primary"}, child0, null);
          block(env, morph2, context, "link-to", ["key_list", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "bucket.clusterId"), "bucket_type_id": get(env, context, "bucket.bucketTypeId"), "bucket_id": get(env, context, "bucket.bucketId")})], {"classNames": "btn btn-xs btn-primary"}, child1, null);
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    No buckets found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0, 1]),1,1);
        block(env, morph0, context, "each", [get(env, context, "bucketList.buckets")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/riak-keys', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
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
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, content = hooks.content;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
            content(env, morph0, context, "obj.key");
            return fragment;
          }
        };
      }());
      var child1 = (function() {
        var child0 = (function() {
          return {
            isHTMLBars: true,
            revision: "Ember@1.11.1",
            blockParams: 0,
            cachedFragment: null,
            hasRendered: false,
            build: function build(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("                            ");
              dom.appendChild(el0, el1);
              var el1 = dom.createComment("");
              dom.appendChild(el0, el1);
              return el0;
            },
            render: function render(context, env, contextualElement) {
              var dom = env.dom;
              var hooks = env.hooks, content = hooks.content;
              dom.detectNamespace(contextualElement);
              var fragment;
              if (env.useFragmentCache && dom.canClone) {
                if (this.cachedFragment === null) {
                  fragment = this.build(dom);
                  if (this.hasRendered) {
                    this.cachedFragment = fragment;
                  } else {
                    this.hasRendered = true;
                  }
                }
                if (this.cachedFragment) {
                  fragment = dom.cloneNode(this.cachedFragment, true);
                }
              } else {
                fragment = this.build(dom);
              }
              var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
              dom.insertBoundary(fragment, null);
              content(env, morph0, context, "obj.key");
              return fragment;
            }
          };
        }());
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
            dom.insertBoundary(fragment, 0);
            block(env, morph0, context, "link-to", ["riak-object", subexpr(env, context, "query-params", [], {"cluster_id": get(env, context, "obj.bucket.clusterId"), "bucket_type_id": get(env, context, "obj.bucket.bucketTypeId"), "bucket_id": get(env, context, "obj.bucket.name"), "object_key": get(env, context, "obj.key")})], {}, child0, null);
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, get = hooks.get, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [1, 1, 1, 1, 1]),1,1);
          set(env, context, "obj", blockArguments[0]);
          block(env, morph0, context, "if", [get(env, context, "obj.markedDeleted")], {}, child0, child1);
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    No keys found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0, 1]),1,1);
        block(env, morph0, context, "each", [get(env, context, "keys")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/riak-node', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("Node Ping");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("Node Stats");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, subexpr = hooks.subexpr, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
        var morph1 = dom.createMorphAt(dom.childAt(fragment, [3]),0,0);
        var morph2 = dom.createMorphAt(dom.childAt(fragment, [5]),0,0);
        content(env, morph0, context, "node.id");
        block(env, morph1, context, "link-to", ["riak_ping", subexpr(env, context, "query-params", [], {"node_id": get(env, context, "node.id")})], {"classNames": "btn btn-sm btn-default"}, child0, null);
        block(env, morph2, context, "link-to", ["node_stats", subexpr(env, context, "query-params", [], {"node_id": get(env, context, "node.id")})], {"classNames": "btn btn-sm btn-primary"}, child1, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/components/search-indexes', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, content = hooks.content, get = hooks.get, concat = hooks.concat, attribute = hooks.attribute;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var element1 = dom.childAt(element0, [3, 1]);
          var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          var morph1 = dom.createMorphAt(element1,0,0);
          var attrMorph0 = dom.createAttrMorph(element1, 'href');
          var morph2 = dom.createMorphAt(dom.childAt(element0, [5]),0,0);
          set(env, context, "index", blockArguments[0]);
          content(env, morph0, context, "index.name");
          attribute(env, attrMorph0, element1, "href", concat(env, [get(env, context, "clusterProxyUrl"), "/search/schema/", get(env, context, "index.schema")]));
          content(env, morph1, context, "index.schema");
          content(env, morph2, context, "index.n_val");
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    No indexes found.\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0, 3]),1,1);
        block(env, morph0, context, "each", [get(env, context, "indexes")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/errors/object-not-found', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, inline = hooks.inline;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [0]);
        var morph0 = dom.createMorphAt(dom.childAt(element0, [3, 1, 1]),0,0);
        var morph1 = dom.createMorphAt(element0,5,5);
        content(env, morph0, context, "model.key");
        inline(env, morph1, context, "crumb-trail", [], {"model": get(env, context, "model")});
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/errors/unknown', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/explorer-api', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("Available (");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode(")\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
          content(env, morph0, context, "model.pingResult.ping.message");
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("Unavailable");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child2 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          var morph1 = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
          set(env, context, "route", blockArguments[0]);
          content(env, morph0, context, "route.links.self");
          content(env, morph1, context, "route.resources");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0]),0,0);
        var morph1 = dom.createMorphAt(fragment,6,6,contextualElement);
        var morph2 = dom.createMorphAt(fragment,11,11,contextualElement);
        var morph3 = dom.createMorphAt(fragment,16,16,contextualElement);
        var morph4 = dom.createMorphAt(dom.childAt(fragment, [21, 3]),1,1);
        content(env, morph0, context, "pageTitle");
        content(env, morph1, context, "model.service");
        block(env, morph2, context, "if", [get(env, context, "model.pingResult.ping")], {}, child0, child1);
        content(env, morph3, context, "model.propsResult.props.development_mode");
        block(env, morph4, context, "each", [get(env, context, "model.routes")], {}, child2, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/index', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createTextNode("Select cluster from the nav bar on the left.\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/key-list', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                ");
            dom.appendChild(el0, el1);
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, get = hooks.get, inline = hooks.inline;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var morph0 = dom.createMorphAt(fragment,1,1,contextualElement);
            inline(env, morph0, context, "refresh-keys", [], {"action": "refreshKeys", "keyList": get(env, context, "model")});
            return fragment;
          }
        };
      }());
      var child1 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("        ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("button");
            dom.setAttribute(el1,"type","button");
            dom.setAttribute(el1,"class","btn btn-xs btn-danger");
            var el2 = dom.createTextNode("\n            Delete Listed Objects");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n");
            dom.appendChild(el0, el1);
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            var hooks = env.hooks, get = hooks.get, element = hooks.element;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            var element0 = dom.childAt(fragment, [1]);
            element(env, element0, context, "action", ["deleteBucket", get(env, context, "model.bucket")], {});
            return fragment;
          }
        };
      }());
      var child2 = (function() {
        return {
          isHTMLBars: true,
          revision: "Ember@1.11.1",
          blockParams: 0,
          cachedFragment: null,
          hasRendered: false,
          build: function build(dom) {
            var el0 = dom.createDocumentFragment();
            return el0;
          },
          render: function render(context, env, contextualElement) {
            var dom = env.dom;
            dom.detectNamespace(contextualElement);
            var fragment;
            if (env.useFragmentCache && dom.canClone) {
              if (this.cachedFragment === null) {
                fragment = this.build(dom);
                if (this.hasRendered) {
                  this.cachedFragment = fragment;
                } else {
                  this.hasRendered = true;
                }
              }
              if (this.cachedFragment) {
                fragment = dom.cloneNode(this.cachedFragment, true);
              }
            } else {
              fragment = this.build(dom);
            }
            return fragment;
          }
        };
      }());
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element1 = dom.childAt(fragment, [1, 1, 1]);
          var element2 = dom.childAt(fragment, [3]);
          var morph0 = dom.createMorphAt(element1,1,1);
          var morph1 = dom.createMorphAt(element1,3,3);
          var morph2 = dom.createMorphAt(dom.childAt(element2, [1]),0,0);
          var morph3 = dom.createMorphAt(dom.childAt(element2, [3]),0,0);
          var morph4 = dom.createMorphAt(element2,5,5);
          var morph5 = dom.createMorphAt(dom.childAt(fragment, [5]),1,1);
          content(env, morph0, context, "model.created");
          block(env, morph1, context, "if", [get(env, context, "model.cluster.developmentMode")], {}, child0, null);
          content(env, morph2, context, "model.count");
          content(env, morph3, context, "model.total");
          block(env, morph4, context, "if", [get(env, context, "model.showDeleteKeys")], {}, child1, null);
          block(env, morph5, context, "riak-keys", [], {"deleteObject": "deleteObject", "keys": get(env, context, "model.keys")}, child2, null);
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","container");
          dom.setAttribute(el1,"style","padding-top: 2em; text-align: center;");
          var el2 = dom.createTextNode("\n    Loading...\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("h4");
        var el2 = dom.createTextNode("Key List: ");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        var el1 = dom.createTextNode("\n\n");
        dom.appendChild(el0, el1);
        var el1 = dom.createComment("");
        dom.appendChild(el0, el1);
        return el0;
      },
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, inline = hooks.inline, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        var morph1 = dom.createMorphAt(fragment,2,2,contextualElement);
        var morph2 = dom.createMorphAt(fragment,4,4,contextualElement);
        dom.insertBoundary(fragment, null);
        content(env, morph0, context, "model.bucketId");
        inline(env, morph1, context, "crumb-trail", [], {"model": get(env, context, "model")});
        block(env, morph2, context, "if", [get(env, context, "model.isLoaded")], {}, child0, child1);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/loading', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
        dom.setAttribute(el2,"class","container");
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/node-stats', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 1,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
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
        render: function render(context, env, contextualElement, blockArguments) {
          var dom = env.dom;
          var hooks = env.hooks, set = hooks.set, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var element0 = dom.childAt(fragment, [1]);
          var morph0 = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
          var morph1 = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          set(env, context, "stat", blockArguments[0]);
          content(env, morph0, context, "stat.key");
          content(env, morph1, context, "stat.value");
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content, get = hooks.get, block = hooks.block;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var morph0 = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        var morph1 = dom.createMorphAt(dom.childAt(fragment, [2, 3]),1,1);
        content(env, morph0, context, "model.node");
        block(env, morph1, context, "each", [get(env, context, "model.stats")], {}, child0, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/riak-object-edit', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child2 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child3 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, inline = hooks.inline, element = hooks.element;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [4]);
        var element1 = dom.childAt(fragment, [14, 1]);
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(dom.childAt(element0, [1, 1]),1,1);
        var morph2 = dom.createMorphAt(dom.childAt(element0, [3, 1]),1,1);
        var morph3 = dom.createMorphAt(dom.childAt(fragment, [8, 3, 1]),3,3);
        var morph4 = dom.createMorphAt(dom.childAt(fragment, [12]),1,1);
        var morph5 = dom.createMorphAt(fragment,16,16,contextualElement);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "object-location", [], {"object": get(env, context, "model.obj"), "isEditing": true}, child0, null);
        block(env, morph1, context, "object-headers-edit", [], {"headers": get(env, context, "model.obj.headersIndexes"), "title": "Secondary Indexes"}, child1, null);
        block(env, morph2, context, "object-headers-edit", [], {"headers": get(env, context, "model.obj.headersCustom"), "title": "Custom Headers"}, child2, null);
        inline(env, morph3, context, "input", [], {"value": get(env, context, "model.obj.contentType"), "id": "contentType", "class": "form-control"});
        inline(env, morph4, context, "textarea", [], {"value": get(env, context, "model.obj.contents"), "autofocus": true, "rows": 20, "cols": 100});
        element(env, element1, context, "action", ["saveObject", get(env, context, "model.obj")], {});
        block(env, morph5, context, "object-version", [], {"object": get(env, context, "model.obj")}, child3, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/riak-object', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child1 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child2 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    var child3 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
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
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          var hooks = env.hooks, content = hooks.content;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          var morph0 = dom.createMorphAt(dom.childAt(fragment, [0, 0]),0,0);
          content(env, morph0, context, "model.obj.contents");
          return fragment;
        }
      };
    }());
    var child4 = (function() {
      return {
        isHTMLBars: true,
        revision: "Ember@1.11.1",
        blockParams: 0,
        cachedFragment: null,
        hasRendered: false,
        build: function build(dom) {
          var el0 = dom.createDocumentFragment();
          return el0;
        },
        render: function render(context, env, contextualElement) {
          var dom = env.dom;
          dom.detectNamespace(contextualElement);
          var fragment;
          if (env.useFragmentCache && dom.canClone) {
            if (this.cachedFragment === null) {
              fragment = this.build(dom);
              if (this.hasRendered) {
                this.cachedFragment = fragment;
              } else {
                this.hasRendered = true;
              }
            }
            if (this.cachedFragment) {
              fragment = dom.cloneNode(this.cachedFragment, true);
            }
          } else {
            fragment = this.build(dom);
          }
          return fragment;
        }
      };
    }());
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
        var el3 = dom.createTextNode("Contents");
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
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("a");
        dom.setAttribute(el4,"class","btn btn-xs btn-primary");
        var el5 = dom.createTextNode("View raw");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-10");
        var el4 = dom.createTextNode("\n            Content Type: ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("b");
        var el5 = dom.createComment("");
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
        var el1 = dom.createElement("br");
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, get = hooks.get, block = hooks.block, concat = hooks.concat, attribute = hooks.attribute, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [4]);
        var element1 = dom.childAt(fragment, [8, 3]);
        var element2 = dom.childAt(element1, [1, 1]);
        var morph0 = dom.createMorphAt(fragment,0,0,contextualElement);
        var morph1 = dom.createMorphAt(dom.childAt(element0, [1, 1]),1,1);
        var morph2 = dom.createMorphAt(dom.childAt(element0, [3, 1]),1,1);
        var attrMorph0 = dom.createAttrMorph(element2, 'href');
        var morph3 = dom.createMorphAt(dom.childAt(element1, [3, 1]),0,0);
        var morph4 = dom.createMorphAt(fragment,12,12,contextualElement);
        var morph5 = dom.createMorphAt(fragment,14,14,contextualElement);
        dom.insertBoundary(fragment, 0);
        block(env, morph0, context, "object-location", [], {"object": get(env, context, "model.obj"), "isEditing": false}, child0, null);
        block(env, morph1, context, "object-headers", [], {"headers": get(env, context, "model.obj.headersIndexes"), "title": "Secondary Indexes"}, child1, null);
        block(env, morph2, context, "object-headers", [], {"headers": get(env, context, "model.obj.headersCustom"), "title": "Custom Headers"}, child2, null);
        attribute(env, attrMorph0, element2, "href", concat(env, [get(env, context, "model.url")]));
        content(env, morph3, context, "model.obj.contentType");
        block(env, morph4, context, "if", [get(env, context, "model.obj.contents")], {}, child3, null);
        block(env, morph5, context, "object-version", [], {"object": get(env, context, "model.obj")}, child4, null);
        return fragment;
      }
    };
  }()));

});
define('ember-riak-explorer/templates/riak-ping', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    return {
      isHTMLBars: true,
      revision: "Ember@1.11.1",
      blockParams: 0,
      cachedFragment: null,
      hasRendered: false,
      build: function build(dom) {
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
      render: function render(context, env, contextualElement) {
        var dom = env.dom;
        var hooks = env.hooks, content = hooks.content;
        dom.detectNamespace(contextualElement);
        var fragment;
        if (env.useFragmentCache && dom.canClone) {
          if (this.cachedFragment === null) {
            fragment = this.build(dom);
            if (this.hasRendered) {
              this.cachedFragment = fragment;
            } else {
              this.hasRendered = true;
            }
          }
          if (this.cachedFragment) {
            fragment = dom.cloneNode(this.cachedFragment, true);
          }
        } else {
          fragment = this.build(dom);
        }
        var element0 = dom.childAt(fragment, [1]);
        var morph0 = dom.createMorphAt(dom.childAt(element0, [1, 3]),0,0);
        var morph1 = dom.createMorphAt(dom.childAt(element0, [3, 3]),0,0);
        content(env, morph0, context, "node_id");
        content(env, morph1, context, "model.message");
        return fragment;
      }
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
define('ember-riak-explorer/tests/app.jshint', function () {

  'use strict';

  module('JSHint - .');
  test('app.js should pass jshint', function() { 
    ok(true, 'app.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/controllers/bucket-list.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/bucket-list.js should pass jshint', function() { 
    ok(true, 'controllers/bucket-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/bucket-props.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/bucket-props.js should pass jshint', function() { 
    ok(true, 'controllers/bucket-props.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/bucket-type.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/bucket-type.js should pass jshint', function() { 
    ok(true, 'controllers/bucket-type.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/cluster.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/cluster.js should pass jshint', function() { 
    ok(true, 'controllers/cluster.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/errors/object-not-found.jshint', function () {

  'use strict';

  module('JSHint - controllers/errors');
  test('controllers/errors/object-not-found.js should pass jshint', function() { 
    ok(true, 'controllers/errors/object-not-found.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/errors/unknown.jshint', function () {

  'use strict';

  module('JSHint - controllers/errors');
  test('controllers/errors/unknown.js should pass jshint', function() { 
    ok(true, 'controllers/errors/unknown.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/explorer-api.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/explorer-api.js should pass jshint', function() { 
    ok(true, 'controllers/explorer-api.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/key-list.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/key-list.js should pass jshint', function() { 
    ok(true, 'controllers/key-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/node-stats.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/node-stats.js should pass jshint', function() { 
    ok(true, 'controllers/node-stats.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/riak-object-edit.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/riak-object-edit.js should pass jshint', function() { 
    ok(true, 'controllers/riak-object-edit.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/controllers/riak-object.jshint', function () {

  'use strict';

  module('JSHint - controllers');
  test('controllers/riak-object.js should pass jshint', function() { 
    ok(true, 'controllers/riak-object.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/helpers/start-app', ['exports', 'ember', 'ember-riak-explorer/app', 'ember-riak-explorer/router', 'ember-riak-explorer/config/environment'], function (exports, Ember, Application, Router, config) {

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
define('ember-riak-explorer/tests/models/bucket-type.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/bucket-type.js should pass jshint', function() { 
    ok(true, 'models/bucket-type.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/bucket.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/bucket.js should pass jshint', function() { 
    ok(true, 'models/bucket.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/cached-list.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/cached-list.js should pass jshint', function() { 
    ok(true, 'models/cached-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/cluster.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/cluster.js should pass jshint', function() { 
    ok(true, 'models/cluster.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/models/riak-object.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/riak-object.js should pass jshint', function() { 
    ok(true, 'models/riak-object.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/route.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/route.js should pass jshint', function() { 
    ok(true, 'models/route.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/routes/bucket-list.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/bucket-list.js should pass jshint', function() { 
    ok(true, 'routes/bucket-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/bucket-props.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/bucket-props.js should pass jshint', function() { 
    ok(true, 'routes/bucket-props.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/bucket-type.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/bucket-type.js should pass jshint', function() { 
    ok(true, 'routes/bucket-type.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/cluster.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/cluster.js should pass jshint', function() { 
    ok(true, 'routes/cluster.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/errors/object-not-found.jshint', function () {

  'use strict';

  module('JSHint - routes/errors');
  test('routes/errors/object-not-found.js should pass jshint', function() { 
    ok(true, 'routes/errors/object-not-found.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/errors/unknown.jshint', function () {

  'use strict';

  module('JSHint - routes/errors');
  test('routes/errors/unknown.js should pass jshint', function() { 
    ok(true, 'routes/errors/unknown.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/routes/key-list.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/key-list.js should pass jshint', function() { 
    ok(true, 'routes/key-list.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/node-stats.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/node-stats.js should pass jshint', function() { 
    ok(true, 'routes/node-stats.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/riak-object-edit.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/riak-object-edit.js should pass jshint', function() { 
    ok(true, 'routes/riak-object-edit.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/riak-object.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/riak-object.js should pass jshint', function() { 
    ok(true, 'routes/riak-object.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/routes/riak-ping.jshint', function () {

  'use strict';

  module('JSHint - routes');
  test('routes/riak-ping.js should pass jshint', function() { 
    ok(true, 'routes/riak-ping.js should pass jshint.'); 
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

  ember_qunit.moduleForComponent('object-headers', {});

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Creates the component instance
    var component = this.subject();
    assert.equal(component._state, 'preRender');

    // Renders the component to the page
    this.render();
    assert.equal(component._state, 'inDOM');
  });

  // Specify the other units that are required for this test
  // needs: ['component:foo', 'helper:bar']

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

  ember_qunit.moduleForComponent('riak-node', {});

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Creates the component instance
    var component = this.subject();
    assert.equal(component._state, 'preRender');

    // Renders the component to the page
    this.render();
    assert.equal(component._state, 'inDOM');
  });

  // Specify the other units that are required for this test
  // needs: ['component:foo', 'helper:bar']

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

  ember_qunit.moduleFor('controller:bucket-types', {});

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('controller:ping', {});

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('controller:riak-object', {});

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('controller:riak-ping', {});

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('route:application', {});

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('route:bucket-types', {});

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('route:ping', {});

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('route:riak-object', {});

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('route:riak-ping', {});

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

  // Specify the other units that are required for this test.
  // needs: ['controller:foo']

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

  ember_qunit.moduleFor('service:explorer', {});

  // Replace this with your real tests.
  ember_qunit.test('it exists', function (assert) {
    var service = this.subject();
    assert.ok(service);
  });

  // Specify the other units that are required for this test.
  // needs: ['service:foo']

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
                propsStr += prop + ": " + obj[prop];
                i++;
            }
        }
        return propsStr + "}";
    }
    function objectToArray(obj) {
        var propsArray = [];
        for (var prop in obj) {
            if (obj.hasOwnProperty(prop)) {
                if (Object.prototype.toString.call(obj[prop]) === "[object Object]") {
                    propsArray.push({ key: prop, value: objectToString(obj[prop]) });
                } else if (Object.prototype.toString.call(obj[prop]) === "[object Array]") {
                    var arrStr = "[" + obj[prop].join(", ") + "]";
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
  require("ember-riak-explorer/app")["default"].create({"name":"ember-riak-explorer","version":"0.0.0.8a7957f5"});
}

/* jshint ignore:end */
//# sourceMappingURL=ember-riak-explorer.map