"use strict";
/* jshint ignore:start */

/* jshint ignore:end */

define('ember-riak-explorer/adapters/application', ['exports', 'ember-riak-explorer/adapters/explorer-resource'], function (exports, ExplorerResourceAdapter) {

	'use strict';

	exports['default'] = ExplorerResourceAdapter['default'].extend({});

});
define('ember-riak-explorer/adapters/explorer-resource', ['exports', 'ember-data', 'ember', 'ember-riak-explorer/config/environment'], function (exports, DS, Ember, config) {

    'use strict';

    var ExplorerResourceAdapter = DS['default'].RESTAdapter.extend({
        namespace: config['default'].baseURL + 'explore',

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
define('ember-riak-explorer/components/breadcrumb-component', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

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
define('ember-riak-explorer/components/button/delete-object', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span',

        actions: {
            deleteObject: function deleteObject(object) {
                // Send its primary action to riak-object controller
                this.sendAction('action', object);
            }
        }
    });

});
define('ember-riak-explorer/components/button/edit-object', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/button/object-view-raw', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/button/refresh-buckets', ['exports', 'ember'], function (exports, Ember) {

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
define('ember-riak-explorer/components/button/refresh-keys', ['exports', 'ember'], function (exports, Ember) {

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
define('ember-riak-explorer/components/button/set-element-remove', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span',

        actions: {
            removeElement: function removeElement(model, item) {
                // Send its action to parent controller
                this.sendAction('removeElement', model, item);
            }
        }
    });

});
define('ember-riak-explorer/components/dashboard-module', ['exports', 'ember'], function (exports, Ember) {

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
define('ember-riak-explorer/components/link/bucket-type', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/link/link-bucket', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/link/link-cluster', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Component.extend({
        tagName: 'span'
    });

});
define('ember-riak-explorer/components/link/link-object', ['exports', 'ember'], function (exports, Ember) {

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
define('ember-riak-explorer/components/object-actions', ['exports', 'ember'], function (exports, Ember) {

  'use strict';

  exports['default'] = Ember['default'].Component.extend({
    actions: {
      deleteObject: function deleteObject(object) {
        // Send action to parent controller
        this.sendAction('deleteObject', object);
      }
    }
  });

});
define('ember-riak-explorer/components/object-contents-counter', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var ObjectContentsCounterComponent = Ember['default'].Component.extend({
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
define('ember-riak-explorer/components/object-contents-counters-embedded', ['exports', 'ember-riak-explorer/components/object-contents-counter'], function (exports, ObjectContentsCounterComponent) {

    'use strict';

    var ObjectContentsCountersEmbeddedComponent = ObjectContentsCounterComponent['default'].extend({
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
define('ember-riak-explorer/components/object-contents-flags', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var ObjectContentsFlagsComponent = Ember['default'].Component.extend({
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
define('ember-riak-explorer/components/object-contents-map', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var ObjectContentsMapComponent = Ember['default'].Component.extend({
        explorer: Ember['default'].inject.service('explorer'),
        store: Ember['default'].inject.service('store'),

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
define('ember-riak-explorer/components/object-contents-maps-embedded', ['exports', 'ember-riak-explorer/components/object-contents-set'], function (exports, ObjectContentsMapComponent) {

    'use strict';

    var ObjectContentsMapsEmbeddedComponent = ObjectContentsMapComponent['default'].extend({
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
define('ember-riak-explorer/components/object-contents-registers', ['exports', 'ember'], function (exports, Ember) {

  'use strict';

  var ObjectContentsRegistersComponent = Ember['default'].Component.extend({
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
define('ember-riak-explorer/components/object-contents-set-elements', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var ObjectContentsSetElementsComponent = Ember['default'].Component.extend({
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
define('ember-riak-explorer/components/object-contents-set', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var ObjectContentsSetComponent = Ember['default'].Component.extend({
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
define('ember-riak-explorer/components/object-contents-sets-embedded', ['exports', 'ember-riak-explorer/components/object-contents-set'], function (exports, ObjectContentsSetComponent) {

    'use strict';

    var ObjectContentsSetsEmbeddedComponent = ObjectContentsSetComponent['default'].extend({
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
define('ember-riak-explorer/components/object-contents', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var ObjectContentsComponent = Ember['default'].Component.extend({
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
define('ember-riak-explorer/components/object-metadata', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/object-version', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/pagination-component', ['exports', 'ember'], function (exports, Ember) {

  'use strict';

  exports['default'] = Ember['default'].Component.extend({
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
        this.sendAction('section-request', requestedRange.low);
      },

      prevLinkClick: function prevLinkClick() {
        if (!this.get('shouldPrevBeHidden')) {
          var currentChunk = this.get('currentChunk');
          var newChunk = currentChunk - 1;

          this.set('currentChunk', newChunk);
          this.sendAction('section-request', newChunk);
        }
      },

      nextLinkClick: function nextLinkClick() {
        if (!this.get('shouldNextBeHidden')) {
          var currentChunk = this.get('currentChunk');
          var newChunk = currentChunk + 1;

          this.set('currentChunk', newChunk);
          this.sendAction('section-request', newChunk);
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
      var chunkSize = this.get('chunk-length');

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
      var linkCount = Math.ceil(this.get('total-length') / this.get('chunk-length'));

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
define('ember-riak-explorer/components/results-panel', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

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
define('ember-riak-explorer/components/sidebar-panel', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/topbar-panel', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/view-label', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Component.extend({});

});
define('ember-riak-explorer/components/wrapper-panel', ['exports', 'ember'], function (exports, Ember) {

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
        queryParams: ['clusterId', 'bucketTypeId', 'bucketId', 'key'],
        clusterId: null,
        bucketTypeId: null,
        bucketId: null,
        key: null
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
        queryParams: ['nodeId', 'clusterId'],
        nodeId: null,
        clusterId: null
    });

});
define('ember-riak-explorer/controllers/object', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Controller;

});
define('ember-riak-explorer/controllers/riak-ping', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Controller.extend({
        queryParams: ['nodeId', 'clusterId'],
        nodeId: null,
        clusterId: null
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

  function initialize() {
    var application = arguments[1] || arguments[0];
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

  var BucketList = CachedList['default'].extend({
    /**
     * List of Bucket model instances (loaded from the server)
     * @property buckets
     * @type Array<Bucket>
     * @default []
     */
    buckets: DS['default'].attr(null, { defaultValue: [] }),

    /**
     * The bucket type that owns this bucket list.
     * @property bucketType
     * @type BucketType
     */
    bucketType: DS['default'].belongsTo('bucket-type'),

    /**
     * The cluster in which this bucket type resides.
     * @property cluster
     * @type Cluster
     */
    cluster: DS['default'].belongsTo('cluster'),

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
define('ember-riak-explorer/models/bucket-props', ['exports', 'ember-data', 'ember', 'ember-riak-explorer/utils/riak-util'], function (exports, DS, Ember, objectToArray) {

    'use strict';

    var BucketProps = DS['default'].Model.extend({
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
        props: DS['default'].attr(),

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
            return objectToArray['default'](this.get('props'));
        }).property('props'),

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
            var hasPrecommit = !Ember['default'].isEmpty(this.get('props').precommit);
            var hasPostcommit = !Ember['default'].isEmpty(this.get('props').postcommit);

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
         * Returns a human-readable description of what type of objects are stored
         *    in this bucket type (default, search indexed, CRDTs, etc)
         *
         * @method objectType
         * @return {String}
         */
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
define('ember-riak-explorer/models/cached-list', ['exports', 'ember-data'], function (exports, DS) {

  'use strict';

  var CachedList = DS['default'].Model.extend({

    /**
     * Number of items displayed on the current page of the list
     * @property count
     * @type Number
     * @default 0
     */
    count: DS['default'].attr('number', { defaultValue: 0 }),

    /**
     * Timestamp of when the cached list was generated on the server side
     * @property created
     * @type String
     */
    created: DS['default'].attr(),

    /**
     * Is the List operation waiting for a cache to be generated?
     * @property isLoaded
     * @type Boolean
     */
    isLoaded: DS['default'].attr('boolean', { defaultValue: false }),

    // Total number of items in the list
    /**
     * Total number of items in the cached list
     * @property total
     * @type Number
     * @default 0
     */
    total: DS['default'].attr('number', { defaultValue: 0 })
  });
  exports['default'] = CachedList;

});
define('ember-riak-explorer/models/key-list', ['exports', 'ember-data', 'ember-riak-explorer/models/cached-list'], function (exports, DS, CachedList) {

  'use strict';

  var KeyList = CachedList['default'].extend({
    /**
     * Bucket for which this key list was generated.
     * @property bucket
     * @type Bucket
     */
    bucket: DS['default'].attr(),

    /**
     * Cluster in which the bucket resides.
     * @property cluster
     * @type Cluster
     */
    cluster: DS['default'].attr(),

    /**
     * List of keys (actually, RiakObject instances) for this page
     * @property keys
     * @type Array<RiakObject>
     * @default []
     */
    keys: DS['default'].attr(null, { defaultValue: [] }),

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
define('ember-riak-explorer/models/link', ['exports', 'ember-data'], function (exports, DS) {

  'use strict';

  exports['default'] = DS['default'].Model.extend({
    self: DS['default'].attr(),
    related: DS['default'].attr()
  });

});
define('ember-riak-explorer/models/object-metadata', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    var ObjectMetadata = DS['default'].Model.extend({
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
        headers: DS['default'].attr(null, {
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
define('ember-riak-explorer/models/route', ['exports', 'ember-data'], function (exports, DS) {

  'use strict';

  exports['default'] = DS['default'].Model.extend({
    links: DS['default'].attr(),
    resources: DS['default'].attr()
  });

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
            retrieveRequestedKeys: function retrieveRequestedKeys(startIndex) {
                var service = this.get('explorer');
                var bucket = this.get('model');
                var store = this.get('store');
                var start = startIndex;
                var rows = bucket.get('keyList').get('count');

                return service.getBucketWithKeyList(bucket, store, start, rows);
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
define('ember-riak-explorer/pods/bucket/model', ['exports', 'ember-data'], function (exports, DS) {

  'use strict';

  var Bucket = DS['default'].Model.extend({
    /**
     * Riak cluster in which this bucket lives.
     *
     * @property cluster
     * @type Cluster
     * @writeOnce
     */
    cluster: DS['default'].belongsTo('cluster'),

    /**
     * Riak Bucket Type in which this bucket lives.
     *
     * @property bucketType
     * @type BucketType
     * @writeOnce
     */
    bucketType: DS['default'].belongsTo('bucket-type'),

    /**
     * Has the keyList been loaded from the server?
     *
     * @property isKeyListLoaded
     * @type Boolean
     * @default false
     */
    isKeyListLoaded: DS['default'].attr('boolean', { defaultValue: false }),

    /**
     * Contains the results of cached key lists for this bucket,
     * fetched from the API.
     *
     * @property key-list
     * @type KeyList
     */
    keyList: DS['default'].belongsTo('key-list'),

    /**
     * Bucket name (unique within a cluster and bucket type)
     *
     * @property name
     * @type String
     */
    name: DS['default'].attr('string'),

    /**
     * Bucket Properties object. Note: Bucket Types and Buckets share the
     *    same Properties format.
     * When not specified, buckets inherit their properties from the Bucket Type
     *
     * @property props
     * @type BucketProps
     */
    props: DS['default'].belongsTo('bucket-props'),

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
     * Returns the name of the Search Index associated with this bucket
     * (or its parent bucket type), if applicable.
     *
     * @property index
     * @type String
     */
    index: (function () {
      return this.get('cluster').get('indexes').findBy('name', this.get('props').get('searchIndexName'));
    }).property('cluster'),

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
      var child0 = (function() {
        var child0 = (function() {
          var child0 = (function() {
            var child0 = (function() {
              return {
                meta: {
                  "revision": "Ember@1.13.5",
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
                      "line": 18,
                      "column": 10
                    },
                    "end": {
                      "line": 18,
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
                    "line": 16,
                    "column": 8
                  },
                  "end": {
                    "line": 19,
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
                morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
                morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
                return morphs;
              },
              statements: [
                ["block","em-tab",[],[],0,null,["loc",[null,[17,10],[17,40]]]],
                ["block","em-tab",[],[],1,null,["loc",[null,[18,10],[18,51]]]]
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
                    "line": 21,
                    "column": 8
                  },
                  "end": {
                    "line": 23,
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
                morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
                return morphs;
              },
              statements: [
                ["inline","bucket-properties",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[22,36],[22,41]]]]],[],[]]],["loc",[null,[22,10],[22,43]]]]
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
                      "line": 27,
                      "column": 12
                    },
                    "end": {
                      "line": 32,
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
                  dom.setAttribute(el2,"class","key");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2,"class","value");
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
                  var element3 = dom.childAt(fragment, [1]);
                  var morphs = new Array(2);
                  morphs[0] = dom.createMorphAt(dom.childAt(element3, [1]),0,0);
                  morphs[1] = dom.createMorphAt(dom.childAt(element3, [3]),0,0);
                  return morphs;
                },
                statements: [
                  ["content","prop.key",["loc",[null,[29,32],[29,44]]]],
                  ["content","prop.value",["loc",[null,[30,34],[30,48]]]]
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
                    "line": 25,
                    "column": 8
                  },
                  "end": {
                    "line": 34,
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
                dom.setAttribute(el1,"class","key-value-table");
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
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
                return morphs;
              },
              statements: [
                ["block","each",[["get","model.props.propsList",["loc",[null,[27,20],[27,41]]]]],[],0,null,["loc",[null,[27,12],[32,21]]]]
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
                  "line": 15,
                  "column": 6
                },
                "end": {
                  "line": 35,
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
              morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
              morphs[1] = dom.createMorphAt(fragment,2,2,contextualElement);
              morphs[2] = dom.createMorphAt(fragment,4,4,contextualElement);
              dom.insertBoundary(fragment, 0);
              dom.insertBoundary(fragment, null);
              return morphs;
            },
            statements: [
              ["block","em-tab-list",[],[],0,null,["loc",[null,[16,8],[19,24]]]],
              ["block","em-tab-panel",[],[],1,null,["loc",[null,[21,8],[23,25]]]],
              ["block","em-tab-panel",[],[],2,null,["loc",[null,[25,8],[34,25]]]]
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
                "line": 14,
                "column": 4
              },
              "end": {
                "line": 36,
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
            morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [
            ["block","em-tabs",[],["class","half-width"],0,null,["loc",[null,[15,6],[35,18]]]]
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
                "line": 36,
                "column": 4
              },
              "end": {
                "line": 38,
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
              "line": 13,
              "column": 2
            },
            "end": {
              "line": 39,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.props",["loc",[null,[14,10],[14,21]]]]],[],0,1,["loc",[null,[14,4],[38,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
      };
    }());
    var child1 = (function() {
      var child0 = (function() {
        var child0 = (function() {
          return {
            meta: {
              "revision": "Ember@1.13.5",
              "loc": {
                "source": null,
                "start": {
                  "line": 57,
                  "column": 14
                },
                "end": {
                  "line": 61,
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
              var el1 = dom.createElement("li");
              var el2 = dom.createTextNode("\n                  ");
              dom.appendChild(el1, el2);
              var el2 = dom.createComment("");
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n                ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
              return morphs;
            },
            statements: [
              ["inline","button.refresh-keys",[],["action","refreshKeys","bucket",["subexpr","@mut",[["get","model",["loc",[null,[59,68],[59,73]]]]],[],[]]],["loc",[null,[59,18],[59,75]]]]
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
                  "line": 62,
                  "column": 14
                },
                "end": {
                  "line": 69,
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
              var el1 = dom.createElement("li");
              var el2 = dom.createTextNode("\n                  ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("button");
              dom.setAttribute(el2,"type","button");
              dom.setAttribute(el2,"class","btn btn-xs btn-danger");
              var el3 = dom.createTextNode("\n                    ");
              dom.appendChild(el2, el3);
              var el3 = dom.createElement("span");
              dom.setAttribute(el3,"class","glyphicon glyphicon-trash");
              dom.setAttribute(el3,"aria-hidden","true");
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("\n                    Delete All Keys\n                  ");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n                ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var element0 = dom.childAt(fragment, [1, 1]);
              var morphs = new Array(1);
              morphs[0] = dom.createElementMorph(element0);
              return morphs;
            },
            statements: [
              ["element","action",["deleteBucket",["get","model",["loc",[null,[64,94],[64,99]]]]],[],["loc",[null,[64,70],[64,101]]]]
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
                    "line": 81,
                    "column": 16
                  },
                  "end": {
                    "line": 85,
                    "column": 16
                  }
                },
                "moduleName": "ember-riak-explorer/pods/bucket/template.hbs"
              },
              arity: 1,
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
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
                return morphs;
              },
              statements: [
                ["inline","link.link-object",[],["obj",["subexpr","@mut",[["get","obj",["loc",[null,[83,43],[83,46]]]]],[],[]]],["loc",[null,[83,20],[83,48]]]]
              ],
              locals: ["obj"],
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
                    "line": 85,
                    "column": 16
                  },
                  "end": {
                    "line": 87,
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
                var el1 = dom.createTextNode("                  No keys found\n");
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
                  "line": 76,
                  "column": 12
                },
                "end": {
                  "line": 89,
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
              var el1 = dom.createTextNode("              ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("ul");
              dom.setAttribute(el1,"class","button-list small");
              var el2 = dom.createTextNode("\n");
              dom.appendChild(el1, el2);
              var el2 = dom.createComment("");
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("              ");
              dom.appendChild(el1, el2);
              dom.appendChild(el0, el1);
              var el1 = dom.createTextNode("\n");
              dom.appendChild(el0, el1);
              return el0;
            },
            buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
              var morphs = new Array(1);
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
              return morphs;
            },
            statements: [
              ["block","each",[["get","model.keyList.keys",["loc",[null,[81,24],[81,42]]]]],[],0,1,["loc",[null,[81,16],[87,25]]]]
            ],
            locals: [],
            templates: [child0, child1]
          };
        }());
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 42,
                "column": 4
              },
              "end": {
                "line": 94,
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
            var el1 = dom.createElement("table");
            dom.setAttribute(el1,"class","key-value-table");
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("tbody");
            var el3 = dom.createTextNode("\n        ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("tr");
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","key");
            var el5 = dom.createTextNode("Key cache created:");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","value");
            var el5 = dom.createComment("");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n        ");
            dom.appendChild(el3, el4);
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("\n        ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("tr");
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","key");
            var el5 = dom.createTextNode("Key count:");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","value");
            var el5 = dom.createComment("");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n        ");
            dom.appendChild(el3, el4);
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("\n        ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("tr");
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","key");
            var el5 = dom.createTextNode("Available actions:");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","value");
            var el5 = dom.createTextNode("\n            ");
            dom.appendChild(el4, el5);
            var el5 = dom.createElement("ul");
            dom.setAttribute(el5,"class","button-list");
            var el6 = dom.createTextNode("\n");
            dom.appendChild(el5, el6);
            var el6 = dom.createComment("");
            dom.appendChild(el5, el6);
            var el6 = dom.createComment("");
            dom.appendChild(el5, el6);
            var el6 = dom.createTextNode("            ");
            dom.appendChild(el5, el6);
            dom.appendChild(el4, el5);
            var el5 = dom.createTextNode("\n          ");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n        ");
            dom.appendChild(el3, el4);
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("\n        ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("tr");
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","key");
            var el5 = dom.createTextNode("Stored keys:");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","value");
            var el5 = dom.createTextNode("\n");
            dom.appendChild(el4, el5);
            var el5 = dom.createComment("");
            dom.appendChild(el4, el5);
            var el5 = dom.createTextNode("          ");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n        ");
            dom.appendChild(el3, el4);
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("\n        ");
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
            var element1 = dom.childAt(fragment, [1, 1]);
            var element2 = dom.childAt(element1, [5, 3, 1]);
            var morphs = new Array(5);
            morphs[0] = dom.createMorphAt(dom.childAt(element1, [1, 3]),0,0);
            morphs[1] = dom.createMorphAt(dom.childAt(element1, [3, 3]),0,0);
            morphs[2] = dom.createMorphAt(element2,1,1);
            morphs[3] = dom.createMorphAt(element2,2,2);
            morphs[4] = dom.createMorphAt(dom.childAt(element1, [7, 3]),1,1);
            return morphs;
          },
          statements: [
            ["content","model.keyList.created",["loc",[null,[47,28],[47,53]]]],
            ["content","model.keyList.count",["loc",[null,[51,28],[51,51]]]],
            ["block","if",[["get","model.cluster.developmentMode",["loc",[null,[57,20],[57,49]]]]],[],0,null,["loc",[null,[57,14],[61,21]]]],
            ["block","if",[["get","model.keyList.showDeleteKeys",["loc",[null,[62,20],[62,48]]]]],[],1,null,["loc",[null,[62,14],[69,21]]]],
            ["block","pagination-component",[],["chunk-length",["subexpr","@mut",[["get","model.keyList.count",["loc",[null,[77,25],[77,44]]]]],[],[]],"total-length",["subexpr","@mut",[["get","model.keyList.total",["loc",[null,[78,25],[78,44]]]]],[],[]],"section-request","retrieveRequestedKeys"],2,null,["loc",[null,[76,12],[89,37]]]]
          ],
          locals: [],
          templates: [child0, child1, child2]
        };
      }());
      var child1 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 94,
                "column": 4
              },
              "end": {
                "line": 96,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["content","loading-spinner",["loc",[null,[95,6],[95,25]]]]
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
              "column": 2
            },
            "end": {
              "line": 97,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.isKeyListLoaded",["loc",[null,[42,10],[42,31]]]]],[],0,1,["loc",[null,[42,4],[96,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
            "line": 99,
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
        dom.setAttribute(el1,"class","view-header");
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
        dom.setAttribute(el1,"class","bucket-types-container");
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
        var element4 = dom.childAt(fragment, [0]);
        var element5 = dom.childAt(fragment, [2]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(element4,1,1);
        morphs[1] = dom.createMorphAt(element4,3,3);
        morphs[2] = dom.createMorphAt(element5,1,1);
        morphs[3] = dom.createMorphAt(element5,3,3);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[3,12],[3,27]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[4,15],[4,33]]]]],[],[]],"bucketId",["subexpr","@mut",[["get","model.bucketId",["loc",[null,[5,11],[5,25]]]]],[],[]]],["loc",[null,[2,2],[6,4]]]],
        ["inline","view-label",[],["pre-label","Bucket","label",["subexpr","@mut",[["get","model.bucketId",["loc",[null,[9,8],[9,22]]]]],[],[]]],["loc",[null,[7,2],[9,24]]]],
        ["block","dashboard-module",[],[],0,null,["loc",[null,[13,2],[39,23]]]],
        ["block","dashboard-module",[],["label","Keys"],1,null,["loc",[null,[41,2],[97,23]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

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
            retrieveRequestedBuckets: function retrieveRequestedBuckets(startIndex) {
                var service = this.get('explorer');
                var bucketType = this.get('model');
                var cluster = bucketType.get('cluster');
                var store = this.get('store');
                var start = startIndex;
                var rows = bucketType.get('bucketList').get('count');

                return service.getBucketTypeWithBucketList(bucketType, cluster, store, start, rows);
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
define('ember-riak-explorer/pods/bucket-type/model', ['exports', 'ember-data'], function (exports, DS) {

  'use strict';

  var BucketType = DS['default'].Model.extend({
    /**
     * Riak cluster in which this bucket type lives.
     * @property cluster
     * @type Cluster
     * @writeOnce
     */
    cluster: DS['default'].belongsTo('cluster'),

    /**
     * Contains the results of cached bucket lists for this bucket type,
     * fetched from the API.
     * @property bucket-list
     * @type BucketList
     */
    bucketList: DS['default'].belongsTo('bucket-list'),

    /**
     * Has the bucketList been loaded from the server?
     * @property isBucketListLoaded
     * @type Boolean
     * @default false
     */
    isBucketListLoaded: DS['default'].attr('boolean', { defaultValue: false }),

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
     * Returns the name of the Search Index associated with this bucket type,
     *     if applicable.
     * @property index
     * @type String
     */
    index: (function () {
      return this.get('cluster').get('indexes').findBy('name', this.get('props').get('searchIndexName'));
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
    }).property('id'),

    /**
     * Bucket Type name (unique per cluster),
     *    as appears on `riak-admin bucket-type list`
     * @property originalId
     * @type String
     */
    originalId: DS['default'].attr('string'),

    /**
     * Bucket Type Properties object.
     * Note: Bucket Types and Buckets share the same Properties format.
     * When not specified, buckets inherit their properties from the Bucket Type
     * @property props
     * @type BucketProps
     */
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
      var child0 = (function() {
        var child0 = (function() {
          var child0 = (function() {
            var child0 = (function() {
              return {
                meta: {
                  "revision": "Ember@1.13.5",
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
                morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
                morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
                return morphs;
              },
              statements: [
                ["block","em-tab",[],[],0,null,["loc",[null,[17,10],[17,40]]]],
                ["block","em-tab",[],[],1,null,["loc",[null,[18,10],[18,51]]]]
              ],
              locals: [],
              templates: [child0, child1]
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
                  dom.setAttribute(el1,"class","label label-success");
                  var el2 = dom.createTextNode("Active");
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
                  dom.setAttribute(el1,"class","label label-default");
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
              return {
                meta: {
                  "revision": "Ember@1.13.5",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 42,
                      "column": 16
                    },
                    "end": {
                      "line": 51,
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
                  var element3 = dom.childAt(fragment, [13]);
                  var morphs = new Array(7);
                  morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
                  morphs[1] = dom.createMorphAt(fragment,5,5,contextualElement);
                  morphs[2] = dom.createMorphAt(fragment,7,7,contextualElement);
                  morphs[3] = dom.createMorphAt(fragment,9,9,contextualElement);
                  morphs[4] = dom.createMorphAt(fragment,11,11,contextualElement);
                  morphs[5] = dom.createMorphAt(element3,1,1);
                  morphs[6] = dom.createMorphAt(element3,3,3);
                  return morphs;
                },
                statements: [
                  ["content","model.props.quorum.r",["loc",[null,[44,21],[44,45]]]],
                  ["content","model.props.quorum.w",["loc",[null,[44,50],[44,74]]]],
                  ["content","model.props.quorum.pr",["loc",[null,[45,22],[45,47]]]],
                  ["content","model.props.quorum.pw",["loc",[null,[45,53],[45,78]]]],
                  ["content","model.props.quorum.dw",["loc",[null,[46,22],[46,47]]]],
                  ["content","model.props.quorum.basic_quorum",["loc",[null,[48,35],[48,70]]]],
                  ["content","model.props.quorum.basic_quorum",["loc",[null,[49,33],[49,68]]]]
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
                        "line": 60,
                        "column": 14
                      },
                      "end": {
                        "line": 73,
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
                    var el1 = dom.createElement("tr");
                    var el2 = dom.createTextNode("\n                  ");
                    dom.appendChild(el1, el2);
                    var el2 = dom.createElement("td");
                    dom.setAttribute(el2,"class","key");
                    var el3 = dom.createTextNode("Search Schema:");
                    dom.appendChild(el2, el3);
                    dom.appendChild(el1, el2);
                    var el2 = dom.createTextNode("\n                  ");
                    dom.appendChild(el1, el2);
                    var el2 = dom.createElement("td");
                    dom.setAttribute(el2,"class","value");
                    var el3 = dom.createTextNode("\n                    ");
                    dom.appendChild(el2, el3);
                    var el3 = dom.createElement("a");
                    var el4 = dom.createTextNode("\n                      ");
                    dom.appendChild(el3, el4);
                    var el4 = dom.createComment("");
                    dom.appendChild(el3, el4);
                    var el4 = dom.createTextNode("\n                    ");
                    dom.appendChild(el3, el4);
                    dom.appendChild(el2, el3);
                    var el3 = dom.createTextNode("\n                  ");
                    dom.appendChild(el2, el3);
                    dom.appendChild(el1, el2);
                    var el2 = dom.createTextNode("\n                ");
                    dom.appendChild(el1, el2);
                    dom.appendChild(el0, el1);
                    var el1 = dom.createTextNode("\n                ");
                    dom.appendChild(el0, el1);
                    var el1 = dom.createElement("tr");
                    var el2 = dom.createTextNode("\n                  ");
                    dom.appendChild(el1, el2);
                    var el2 = dom.createElement("td");
                    dom.setAttribute(el2,"class","key");
                    var el3 = dom.createTextNode("Index N_Val:");
                    dom.appendChild(el2, el3);
                    dom.appendChild(el1, el2);
                    var el2 = dom.createTextNode("\n                  ");
                    dom.appendChild(el1, el2);
                    var el2 = dom.createElement("td");
                    dom.setAttribute(el2,"class","value");
                    var el3 = dom.createComment("");
                    dom.appendChild(el2, el3);
                    dom.appendChild(el1, el2);
                    var el2 = dom.createTextNode("\n                ");
                    dom.appendChild(el1, el2);
                    dom.appendChild(el0, el1);
                    var el1 = dom.createTextNode("\n");
                    dom.appendChild(el0, el1);
                    return el0;
                  },
                  buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                    var element2 = dom.childAt(fragment, [1, 3, 1]);
                    var morphs = new Array(3);
                    morphs[0] = dom.createAttrMorph(element2, 'href');
                    morphs[1] = dom.createMorphAt(element2,1,1);
                    morphs[2] = dom.createMorphAt(dom.childAt(fragment, [3, 3]),0,0);
                    return morphs;
                  },
                  statements: [
                    ["attribute","href",["concat",[["get","model.cluster.proxyUrl",["loc",[null,[64,31],[64,53]]]],"/search/schema/",["get","model.index.schema",["loc",[null,[64,72],[64,90]]]]]]],
                    ["content","model.index.schema",["loc",[null,[65,22],[65,44]]]],
                    ["content","model.index.n_val",["loc",[null,[71,36],[71,57]]]]
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
                        "line": 73,
                        "column": 14
                      },
                      "end": {
                        "line": 81,
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
                    var el1 = dom.createElement("tr");
                    var el2 = dom.createTextNode("\n                    ");
                    dom.appendChild(el1, el2);
                    var el2 = dom.createElement("td");
                    dom.setAttribute(el2,"class","key");
                    dom.appendChild(el1, el2);
                    var el2 = dom.createTextNode("\n                    ");
                    dom.appendChild(el1, el2);
                    var el2 = dom.createElement("td");
                    dom.setAttribute(el2,"class","value");
                    var el3 = dom.createTextNode("\n                      ");
                    dom.appendChild(el2, el3);
                    var el3 = dom.createElement("em");
                    var el4 = dom.createTextNode("Warning: Index with id ");
                    dom.appendChild(el3, el4);
                    var el4 = dom.createElement("code");
                    var el5 = dom.createComment("");
                    dom.appendChild(el4, el5);
                    dom.appendChild(el3, el4);
                    var el4 = dom.createTextNode("\n                        has not been created on this cluster!");
                    dom.appendChild(el3, el4);
                    dom.appendChild(el2, el3);
                    var el3 = dom.createTextNode("\n                    ");
                    dom.appendChild(el2, el3);
                    dom.appendChild(el1, el2);
                    var el2 = dom.createTextNode("\n                ");
                    dom.appendChild(el1, el2);
                    dom.appendChild(el0, el1);
                    var el1 = dom.createTextNode("\n");
                    dom.appendChild(el0, el1);
                    return el0;
                  },
                  buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                    var morphs = new Array(1);
                    morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1, 1]),0,0);
                    return morphs;
                  },
                  statements: [
                    ["content","model.props.searchIndexName",["loc",[null,[77,55],[77,86]]]]
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
                      "line": 55,
                      "column": 12
                    },
                    "end": {
                      "line": 82,
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
                  dom.setAttribute(el2,"class","key");
                  var el3 = dom.createTextNode("Search Index:");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2,"class","value");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n              ");
                  dom.appendChild(el1, el2);
                  dom.appendChild(el0, el1);
                  var el1 = dom.createTextNode("\n");
                  dom.appendChild(el0, el1);
                  var el1 = dom.createComment("");
                  dom.appendChild(el0, el1);
                  return el0;
                },
                buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
                  var morphs = new Array(2);
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3]),0,0);
                  morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
                  dom.insertBoundary(fragment, null);
                  return morphs;
                },
                statements: [
                  ["content","model.props.searchIndexName",["loc",[null,[58,34],[58,65]]]],
                  ["block","if",[["get","model.index",["loc",[null,[60,20],[60,31]]]]],[],0,1,["loc",[null,[60,14],[81,21]]]]
                ],
                locals: [],
                templates: [child0, child1]
              };
            }());
            var child4 = (function() {
              return {
                meta: {
                  "revision": "Ember@1.13.5",
                  "loc": {
                    "source": null,
                    "start": {
                      "line": 82,
                      "column": 12
                    },
                    "end": {
                      "line": 87,
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
                  dom.setAttribute(el2,"class","key");
                  var el3 = dom.createTextNode("Search Index:");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2,"class","value");
                  var el3 = dom.createTextNode("Not available (not being indexed)");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n              ");
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
            var child5 = (function() {
              var child0 = (function() {
                return {
                  meta: {
                    "revision": "Ember@1.13.5",
                    "loc": {
                      "source": null,
                      "start": {
                        "line": 93,
                        "column": 20
                      },
                      "end": {
                        "line": 95,
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
                    morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
                    return morphs;
                  },
                  statements: [
                    ["content","warning",["loc",[null,[94,26],[94,37]]]]
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
                      "line": 88,
                      "column": 12
                    },
                    "end": {
                      "line": 99,
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
                  dom.setAttribute(el2,"class","key");
                  var el3 = dom.createTextNode("Warnings:");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2,"class","value");
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
                  morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1]),1,1);
                  return morphs;
                },
                statements: [
                  ["block","each",[["get","model.props.warnings",["loc",[null,[93,28],[93,48]]]]],[],0,null,["loc",[null,[93,20],[95,29]]]]
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
                    "line": 21,
                    "column": 8
                  },
                  "end": {
                    "line": 101,
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
                dom.setAttribute(el1,"class","key-value-table");
                var el2 = dom.createTextNode("\n            ");
                dom.appendChild(el1, el2);
                var el2 = dom.createElement("tr");
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3,"class","key");
                var el4 = dom.createTextNode("Object type:");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3,"class","value");
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
                dom.setAttribute(el3,"class","key");
                var el4 = dom.createTextNode("Conflict Res. Strategy:");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3,"class","value");
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
                dom.setAttribute(el3,"class","key");
                var el4 = dom.createTextNode("Quorum:");
                dom.appendChild(el3, el4);
                dom.appendChild(el2, el3);
                var el3 = dom.createTextNode("\n              ");
                dom.appendChild(el2, el3);
                var el3 = dom.createElement("td");
                dom.setAttribute(el3,"class","value");
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
                var el2 = dom.createTextNode("\n\n");
                dom.appendChild(el1, el2);
                var el2 = dom.createComment("");
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
                var element4 = dom.childAt(fragment, [1]);
                var element5 = dom.childAt(element4, [1, 3]);
                var element6 = dom.childAt(element4, [5, 3]);
                var morphs = new Array(7);
                morphs[0] = dom.createMorphAt(element5,1,1);
                morphs[1] = dom.createMorphAt(element5,3,3);
                morphs[2] = dom.createMorphAt(dom.childAt(element4, [3, 3]),0,0);
                morphs[3] = dom.createMorphAt(element6,1,1);
                morphs[4] = dom.createMorphAt(element6,3,3);
                morphs[5] = dom.createMorphAt(element4,7,7);
                morphs[6] = dom.createMorphAt(element4,8,8);
                return morphs;
              },
              statements: [
                ["content","model.props.objectType",["loc",[null,[26,16],[26,42]]]],
                ["block","if",[["get","model.isActive",["loc",[null,[27,22],[27,36]]]]],[],0,1,["loc",[null,[27,16],[31,23]]]],
                ["content","model.props.resolutionStrategy",["loc",[null,[36,32],[36,66]]]],
                ["content","model.props.nVal",["loc",[null,[41,23],[41,43]]]],
                ["block","if",[["get","model.props.quorumRelevant",["loc",[null,[42,22],[42,48]]]]],[],2,null,["loc",[null,[42,16],[51,23]]]],
                ["block","if",[["get","model.props.isSearchIndexed",["loc",[null,[55,18],[55,45]]]]],[],3,4,["loc",[null,[55,12],[87,19]]]],
                ["block","if",[["get","model.props.warnings",["loc",[null,[88,18],[88,38]]]]],[],5,null,["loc",[null,[88,12],[99,19]]]]
              ],
              locals: [],
              templates: [child0, child1, child2, child3, child4, child5]
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
                      "line": 105,
                      "column": 12
                    },
                    "end": {
                      "line": 110,
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
                  dom.setAttribute(el2,"class","key");
                  var el3 = dom.createComment("");
                  dom.appendChild(el2, el3);
                  dom.appendChild(el1, el2);
                  var el2 = dom.createTextNode("\n                ");
                  dom.appendChild(el1, el2);
                  var el2 = dom.createElement("td");
                  dom.setAttribute(el2,"class","value");
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
                  var element1 = dom.childAt(fragment, [1]);
                  var morphs = new Array(2);
                  morphs[0] = dom.createMorphAt(dom.childAt(element1, [1]),0,0);
                  morphs[1] = dom.createMorphAt(dom.childAt(element1, [3]),0,0);
                  return morphs;
                },
                statements: [
                  ["content","prop.key",["loc",[null,[107,32],[107,44]]]],
                  ["content","prop.value",["loc",[null,[108,34],[108,48]]]]
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
                    "line": 103,
                    "column": 8
                  },
                  "end": {
                    "line": 112,
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
                dom.setAttribute(el1,"class","key-value-table");
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
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
                return morphs;
              },
              statements: [
                ["block","each",[["get","model.props.propsList",["loc",[null,[105,20],[105,41]]]]],[],0,null,["loc",[null,[105,12],[110,21]]]]
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
                  "line": 15,
                  "column": 6
                },
                "end": {
                  "line": 113,
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
              morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
              morphs[1] = dom.createMorphAt(fragment,2,2,contextualElement);
              morphs[2] = dom.createMorphAt(fragment,4,4,contextualElement);
              dom.insertBoundary(fragment, 0);
              dom.insertBoundary(fragment, null);
              return morphs;
            },
            statements: [
              ["block","em-tab-list",[],[],0,null,["loc",[null,[16,8],[19,24]]]],
              ["block","em-tab-panel",[],[],1,null,["loc",[null,[21,8],[101,25]]]],
              ["block","em-tab-panel",[],[],2,null,["loc",[null,[103,8],[112,25]]]]
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
                "line": 14,
                "column": 4
              },
              "end": {
                "line": 114,
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
            morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [
            ["block","em-tabs",[],["class","half-width"],0,null,["loc",[null,[15,6],[113,18]]]]
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
                "line": 114,
                "column": 4
              },
              "end": {
                "line": 116,
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
              "line": 12,
              "column": 2
            },
            "end": {
              "line": 117,
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
          var el1 = dom.createTextNode("\n");
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
          ["block","if",[["get","model.props",["loc",[null,[14,10],[14,21]]]]],[],0,1,["loc",[null,[14,4],[116,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
      };
    }());
    var child1 = (function() {
      var child0 = (function() {
        var child0 = (function() {
          return {
            meta: {
              "revision": "Ember@1.13.5",
              "loc": {
                "source": null,
                "start": {
                  "line": 131,
                  "column": 10
                },
                "end": {
                  "line": 144,
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
              dom.setAttribute(el2,"class","key");
              var el3 = dom.createTextNode("Available actions:");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n              ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("td");
              dom.setAttribute(el2,"class","value");
              var el3 = dom.createTextNode("\n                ");
              dom.appendChild(el2, el3);
              var el3 = dom.createElement("ul");
              dom.setAttribute(el3,"class","button-list");
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
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1, 1]),1,1);
              return morphs;
            },
            statements: [
              ["inline","button.refresh-buckets",[],["action","refreshBuckets","bucketType",["subexpr","@mut",[["get","model",["loc",[null,[139,31],[139,36]]]]],[],[]]],["loc",[null,[137,20],[139,38]]]]
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
                    "line": 153,
                    "column": 18
                  },
                  "end": {
                    "line": 157,
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
                morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
                return morphs;
              },
              statements: [
                ["inline","link.link-bucket",[],["bucket",["subexpr","@mut",[["get","bucket",["loc",[null,[155,48],[155,54]]]]],[],[]]],["loc",[null,[155,22],[155,56]]]]
              ],
              locals: ["bucket"],
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
                    "line": 157,
                    "column": 18
                  },
                  "end": {
                    "line": 159,
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
                  "line": 148,
                  "column": 14
                },
                "end": {
                  "line": 161,
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
              dom.setAttribute(el1,"class","button-list small");
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
              morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
              return morphs;
            },
            statements: [
              ["block","each",[["get","model.bucketList.buckets",["loc",[null,[153,26],[153,50]]]]],[],0,1,["loc",[null,[153,18],[159,27]]]]
            ],
            locals: [],
            templates: [child0, child1]
          };
        }());
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 120,
                "column": 4
              },
              "end": {
                "line": 166,
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
            var el1 = dom.createElement("table");
            dom.setAttribute(el1,"class","key-value-table");
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("tbody");
            var el3 = dom.createTextNode("\n          ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("tr");
            var el4 = dom.createTextNode("\n            ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","key");
            var el5 = dom.createTextNode("Bucket List cache created:");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n            ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","value");
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
            dom.setAttribute(el4,"class","key");
            var el5 = dom.createTextNode("Bucket List count:");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n            ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","value");
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
            dom.setAttribute(el4,"class","key");
            var el5 = dom.createTextNode("Stored buckets");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n            ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("td");
            dom.setAttribute(el4,"class","value");
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
            var el3 = dom.createTextNode("\n        ");
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
            var element0 = dom.childAt(fragment, [1, 1]);
            var morphs = new Array(4);
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]),0,0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]),0,0);
            morphs[2] = dom.createMorphAt(element0,5,5);
            morphs[3] = dom.createMorphAt(dom.childAt(element0, [7, 3]),1,1);
            return morphs;
          },
          statements: [
            ["content","model.bucketList.created",["loc",[null,[125,30],[125,58]]]],
            ["content","model.bucketList.total",["loc",[null,[129,30],[129,56]]]],
            ["block","if",[["get","model.cluster.developmentMode",["loc",[null,[131,16],[131,45]]]]],[],0,null,["loc",[null,[131,10],[144,17]]]],
            ["block","pagination-component",[],["chunk-length",["subexpr","@mut",[["get","model.bucketList.count",["loc",[null,[149,27],[149,49]]]]],[],[]],"total-length",["subexpr","@mut",[["get","model.bucketList.total",["loc",[null,[150,27],[150,49]]]]],[],[]],"section-request","retrieveRequestedBuckets"],1,null,["loc",[null,[148,14],[161,39]]]]
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
                "line": 166,
                "column": 4
              },
              "end": {
                "line": 168,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["content","loading-spinner",["loc",[null,[167,6],[167,25]]]]
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
              "line": 119,
              "column": 2
            },
            "end": {
              "line": 169,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.isBucketListLoaded",["loc",[null,[120,10],[120,34]]]]],[],0,1,["loc",[null,[120,4],[168,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
            "line": 171,
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
        dom.setAttribute(el1,"class","view-header");
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
        dom.setAttribute(el1,"class","bucket-types-container");
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
        morphs[0] = dom.createMorphAt(element7,1,1);
        morphs[1] = dom.createMorphAt(element7,3,3);
        morphs[2] = dom.createMorphAt(element8,1,1);
        morphs[3] = dom.createMorphAt(element8,3,3);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[3,12],[3,27]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[4,15],[4,33]]]]],[],[]]],["loc",[null,[2,2],[5,4]]]],
        ["inline","view-label",[],["pre-label","Bucket-Type","label",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[8,8],[8,26]]]]],[],[]]],["loc",[null,[6,2],[8,28]]]],
        ["block","dashboard-module",[],[],0,null,["loc",[null,[12,2],[117,23]]]],
        ["block","dashboard-module",[],["label","Buckets"],1,null,["loc",[null,[119,2],[169,23]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/pods/cluster/model', ['exports', 'ember-data', 'ember-riak-explorer/config/environment'], function (exports, DS, config) {

  'use strict';

  var Cluster = DS['default'].Model.extend({
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
     * Bucket types created on the cluster
     * @property bucketTypes
     * @type Array<BucketType>
     */
    bucketTypes: DS['default'].hasMany('bucket-type'),

    /**
     * Returns the name of the cluster
     * (As specified in the `riak_explorer.conf` file)
     * Note: Currently unrelated to the source/datacenter name used by MDC Repl
     * @property clusterId
     * @type String
     */
    clusterId: (function () {
      return this.get('id');
    }).property('id'),

    /**
     * Riak node through which Explorer connects to this riak cluster,
     * in Erlang node id format.
     * @type String
     * @default null
     * @example
     *    'riak@127.0.0.1'
     */
    riakNode: DS['default'].attr('string', { defaultValue: null }),

    /**
     * Is this cluster in Dev Mode? Set in the Explorer config file.
     * Dev mode allows expensive operations like list keys, delete bucket, etc.
     * @property developmentMode
     * @type Boolean
     * @default false
     */
    developmentMode: DS['default'].attr('boolean', { defaultValue: false }),

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
     * List of Search Indexes that have been created on this cluster.
     * @see http://docs.basho.com/riak/latest/dev/using/search/
     * @property indexes
     * @type Array<Hash>
     * @example
     *    [{"name":"customers","n_val":3,"schema":"_yz_default"}]
     */
    indexes: DS['default'].attr(),

    /**
     * List of Riak nodes belonging to the cluster
     * @property nodes
     * @type Array<Hash>
     * @example
     *    [{"id":"riak@127.0.0.1"}]
     */
    nodes: DS['default'].attr(),

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
      return config['default'].baseURL + 'riak/clusters/' + this.get('id');
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
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 10,
                "column": 4
              },
              "end": {
                "line": 14,
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
            ["inline","bucket-types",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[12,16],[12,31]]]]],[],[]],"bucketTypes",["subexpr","@mut",[["get","model.activeBucketTypes",["loc",[null,[13,18],[13,41]]]]],[],[]]],["loc",[null,[11,6],[13,43]]]]
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
                "column": 4
              },
              "end": {
                "line": 16,
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
              "line": 9,
              "column": 2
            },
            "end": {
              "line": 17,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
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
          ["block","if",[["get","model.activeBucketTypes",["loc",[null,[10,10],[10,33]]]]],[],0,1,["loc",[null,[10,4],[16,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
                "line": 20,
                "column": 4
              },
              "end": {
                "line": 24,
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
            ["inline","bucket-types",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[22,16],[22,31]]]]],[],[]],"bucketTypes",["subexpr","@mut",[["get","model.inactiveBucketTypes",["loc",[null,[23,18],[23,43]]]]],[],[]]],["loc",[null,[21,6],[23,45]]]]
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
                "column": 4
              },
              "end": {
                "line": 26,
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
              "column": 2
            },
            "end": {
              "line": 27,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
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
          ["block","if",[["get","model.inactiveBucketTypes",["loc",[null,[20,10],[20,35]]]]],[],0,1,["loc",[null,[20,4],[26,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
                "line": 30,
                "column": 4
              },
              "end": {
                "line": 34,
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
            ["inline","search-indexes",[],["indexes",["subexpr","@mut",[["get","model.indexes",["loc",[null,[32,14],[32,27]]]]],[],[]],"clusterProxyUrl",["subexpr","@mut",[["get","model.proxyUrl",["loc",[null,[33,22],[33,36]]]]],[],[]]],["loc",[null,[31,6],[33,38]]]]
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
                "line": 34,
                "column": 4
              },
              "end": {
                "line": 36,
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
              "column": 2
            },
            "end": {
              "line": 37,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
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
          ["block","if",[["get","model.indexes",["loc",[null,[30,10],[30,23]]]]],[],0,1,["loc",[null,[30,4],[36,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
      };
    }());
    var child3 = (function() {
      var child0 = (function() {
        var child0 = (function() {
          var child0 = (function() {
            return {
              meta: {
                "revision": "Ember@1.13.5",
                "loc": {
                  "source": null,
                  "start": {
                    "line": 53,
                    "column": 14
                  },
                  "end": {
                    "line": 56,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                Node Ping\n");
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
                    "line": 59,
                    "column": 14
                  },
                  "end": {
                    "line": 62,
                    "column": 14
                  }
                },
                "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
              },
              arity: 0,
              cachedFragment: null,
              hasRendered: false,
              buildFragment: function buildFragment(dom) {
                var el0 = dom.createDocumentFragment();
                var el1 = dom.createTextNode("                Node Stats\n");
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
                  "line": 50,
                  "column": 8
                },
                "end": {
                  "line": 68,
                  "column": 8
                }
              },
              "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
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
              var el3 = dom.createTextNode("\n");
              dom.appendChild(el2, el3);
              var el3 = dom.createComment("");
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("            ");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n            ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("td");
              var el3 = dom.createTextNode("\n");
              dom.appendChild(el2, el3);
              var el3 = dom.createComment("");
              dom.appendChild(el2, el3);
              var el3 = dom.createTextNode("            ");
              dom.appendChild(el2, el3);
              dom.appendChild(el1, el2);
              var el2 = dom.createTextNode("\n            ");
              dom.appendChild(el1, el2);
              var el2 = dom.createElement("td");
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
              var element0 = dom.childAt(fragment, [1]);
              var morphs = new Array(3);
              morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),1,1);
              morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
              morphs[2] = dom.createMorphAt(dom.childAt(element0, [5]),1,1);
              return morphs;
            },
            statements: [
              ["block","link-to",["riak-ping",["subexpr","query-params",[],["nodeId",["get","node.id",["loc",[null,[53,58],[53,65]]]],"clusterId",["get","model.clusterId",["loc",[null,[53,76],[53,91]]]]],["loc",[null,[53,37],[53,92]]]]],["classNames","btn btn-sm btn-primary"],0,null,["loc",[null,[53,14],[56,26]]]],
              ["block","link-to",["node-stats",["subexpr","query-params",[],["nodeId",["get","node.id",["loc",[null,[59,59],[59,66]]]],"clusterId",["get","model.clusterId",["loc",[null,[59,77],[59,92]]]]],["loc",[null,[59,38],[59,93]]]]],["classNames","btn btn-sm btn-primary"],1,null,["loc",[null,[59,14],[62,26]]]],
              ["content","node.id",["loc",[null,[65,14],[65,25]]]]
            ],
            locals: ["node"],
            templates: [child0, child1]
          };
        }());
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 40,
                "column": 4
              },
              "end": {
                "line": 71,
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
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("table");
            dom.setAttribute(el1,"class","table");
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("thead");
            var el3 = dom.createTextNode("\n        ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("tr");
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("th");
            var el5 = dom.createTextNode("Ping");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("th");
            var el5 = dom.createTextNode("Stats");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("th");
            var el5 = dom.createTextNode("Node");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n        ");
            dom.appendChild(el3, el4);
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("\n        ");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
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
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3]),1,1);
            return morphs;
          },
          statements: [
            ["block","each",[["get","model.nodes",["loc",[null,[50,16],[50,27]]]]],[],0,null,["loc",[null,[50,8],[68,17]]]]
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
                "line": 71,
                "column": 4
              },
              "end": {
                "line": 73,
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
              "line": 39,
              "column": 2
            },
            "end": {
              "line": 74,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/pods/cluster/template.hbs"
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
          ["block","if",[["get","model.nodes.length",["loc",[null,[40,10],[40,28]]]]],[],0,1,["loc",[null,[40,4],[73,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
        dom.setAttribute(el1,"class","view-header");
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
        dom.setAttribute(el1,"class","cluster-information-container");
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
        var element2 = dom.childAt(fragment, [2]);
        var morphs = new Array(6);
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element1,3,3);
        morphs[2] = dom.createMorphAt(element2,1,1);
        morphs[3] = dom.createMorphAt(element2,3,3);
        morphs[4] = dom.createMorphAt(element2,5,5);
        morphs[5] = dom.createMorphAt(element2,7,7);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[2,35],[2,50]]]]],[],[]]],["loc",[null,[2,2],[2,52]]]],
        ["inline","view-label",[],["pre-label","Cluster","label",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[5,8],[5,23]]]]],[],[]]],["loc",[null,[3,2],[5,25]]]],
        ["block","dashboard-module",[],["label","Active Bucket Types"],0,null,["loc",[null,[9,2],[17,23]]]],
        ["block","dashboard-module",[],["label","Inactive Bucket Types"],1,null,["loc",[null,[19,2],[27,23]]]],
        ["block","dashboard-module",[],["label","Search Indexes"],2,null,["loc",[null,[29,2],[37,23]]]],
        ["block","dashboard-module",[],["label","Nodes"],3,null,["loc",[null,[39,2],[74,23]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/pods/riak-object/controller', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var RiakObjectController = Ember['default'].Controller.extend({
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
    exports['default'] = RiakObjectController;

});
define('ember-riak-explorer/pods/riak-object/counter/controller', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/controller'], function (exports, Ember, RiakObjectController) {

    'use strict';

    var RiakObjectCounterController = RiakObjectController['default'].extend({
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
                Ember['default'].run.later(function () {
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
define('ember-riak-explorer/pods/riak-object/counter/model', ['exports', 'ember-data', 'ember-riak-explorer/pods/riak-object/model'], function (exports, DS, RiakObject) {

  'use strict';

  var RiakObjectCounter = RiakObject['default'].extend({
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
    },

    /**
     * The amount to decrement the counter by.
     * @property decrementBy
     * @type {Number}
     * @private
     */
    decrementBy: DS['default'].attr('integer', { defaultValue: 1 }),

    /**
     * The amount to increment the counter by.
     * @property incrementBy
     * @type {Number}
     * @private
     */
    incrementBy: DS['default'].attr('integer', { defaultValue: 1 })
  });
  exports['default'] = RiakObjectCounter;

});
define('ember-riak-explorer/pods/riak-object/counter/route', ['exports', 'ember-riak-explorer/pods/riak-object/route'], function (exports, RiakObjectRoute) {

	'use strict';

	var RiakObjectCounterRoute = RiakObjectRoute['default'].extend({});
	exports['default'] = RiakObjectCounterRoute;

});
define('ember-riak-explorer/pods/riak-object/counter/template', ['exports'], function (exports) {

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
                "column": 2
              },
              "end": {
                "line": 19,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","object-contents-counter",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[16,10],[16,15]]]]],[],[]],"deleteObject","deleteObject","incrementCounter","incrementCounter","decrementCounter","decrementCounter"],["loc",[null,[15,4],[18,41]]]]
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
                "line": 19,
                "column": 2
              },
              "end": {
                "line": 21,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["content","loading-spinner",["loc",[null,[20,4],[20,23]]]]
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
              "line": 13,
              "column": 0
            },
            "end": {
              "line": 22,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.isLoaded",["loc",[null,[14,8],[14,22]]]]],[],0,1,["loc",[null,[14,2],[21,9]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
        "moduleName": "ember-riak-explorer/pods/riak-object/counter/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(element0,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[3,12],[3,27]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[4,15],[4,33]]]]],[],[]],"bucketId",["subexpr","@mut",[["get","model.bucketId",["loc",[null,[5,11],[5,25]]]]],[],[]],"keyId",["subexpr","@mut",[["get","model.key",["loc",[null,[6,8],[6,17]]]]],[],[]]],["loc",[null,[2,2],[7,4]]]],
        ["inline","view-label",[],["pre-label","Set Object","label",["subexpr","@mut",[["get","model.key",["loc",[null,[10,8],[10,17]]]]],[],[]]],["loc",[null,[8,2],[10,19]]]],
        ["block","dashboard-module",[],["label","Counter info","class","object-counter-container"],0,null,["loc",[null,[13,0],[22,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/pods/riak-object/edit/controller', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var RiakObjectEditController = Ember['default'].Controller.extend({
        explorer: Ember['default'].inject.service('explorer'),

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
define('ember-riak-explorer/pods/riak-object/edit/route', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var RiakObjectEditRoute = Ember['default'].Route.extend({
        model: function model(params) {
            var explorer = this.explorer;
            var store = this.store;
            return explorer.getBucket(params.clusterId, params.bucketTypeId, params.bucketId, store).then(function (bucket) {
                return explorer.getRiakObject(bucket, params.key, store);
            });
        }
    });
    exports['default'] = RiakObjectEditRoute;

});
define('ember-riak-explorer/pods/riak-object/edit/template', ['exports'], function (exports) {

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
                "column": 2
              },
              "end": {
                "line": 19,
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
            dom.setAttribute(el1,"class","riak-object");
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
            morphs[0] = dom.createMorphAt(element0,1,1);
            morphs[1] = dom.createMorphAt(element0,3,3);
            return morphs;
          },
          statements: [
            ["inline","object-metadata",[],["metadata",["subexpr","@mut",[["get","model.metadata",["loc",[null,[16,33],[16,47]]]]],[],[]],"isEditing",true],["loc",[null,[16,6],[16,64]]]],
            ["inline","object-contents",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[17,30],[17,35]]]]],[],[]],"isEditing",true,"saveObject","saveObject"],["loc",[null,[17,6],[17,76]]]]
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
                "line": 19,
                "column": 2
              },
              "end": {
                "line": 21,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["content","loading-spinner",["loc",[null,[20,4],[20,23]]]]
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
              "line": 13,
              "column": 0
            },
            "end": {
              "line": 22,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.isLoaded",["loc",[null,[14,8],[14,22]]]]],[],0,1,["loc",[null,[14,2],[21,9]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
        "moduleName": "ember-riak-explorer/pods/riak-object/edit/template.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element1,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[3,12],[3,27]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[4,15],[4,33]]]]],[],[]],"bucketId",["subexpr","@mut",[["get","model.bucketId",["loc",[null,[5,11],[5,25]]]]],[],[]],"keyId",["subexpr","@mut",[["get","model.key",["loc",[null,[6,8],[6,17]]]]],[],[]]],["loc",[null,[2,2],[7,4]]]],
        ["inline","view-label",[],["pre-label","Riak Object","label",["subexpr","@mut",[["get","model.key",["loc",[null,[10,8],[10,17]]]]],[],[]]],["loc",[null,[8,2],[10,19]]]],
        ["block","dashboard-module",[],[],0,null,["loc",[null,[13,0],[22,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/pods/riak-object/embedded-map/model', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/map-field/model'], function (exports, Ember, RiakObjectMapField) {

    'use strict';

    var RiakObjectEmbeddedMap = RiakObjectMapField['default'].extend({

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
         * Hashmap of counters (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property counters
         * @type {Object}
         */
        counters: Ember['default'].computed('value', {
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
         * Hashmap of flags (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property flags
         * @type {Object}
         */
        flags: Ember['default'].computed('value', {
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
         * Returns a list of Flags for this map, sorted by field name.
         *
         * @method flagsList
         * @return {Array<RiakObjectMapField>}
         */
        flagsList: (function flagsList() {
            return this.fieldList('flags');
        }).property('flags'),

        isTopLevel: (function isTopLevel() {
            return false;
        }).property(),

        /**
         * Hashmap of embedded Maps (`RiakObjectEmbeddedMap` instances) in this map,
         * keyed by field name.
         *
         * @property maps
         * @type {Object}
         */
        maps: Ember['default'].computed('value', {
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
         * Returns a list of embedded Maps in this map, sorted by field name.
         *
         * @method mapsList
         * @return {Array<RiakObjectEmbeddedMap>}
         */
        mapsList: (function mapsList() {
            return this.fieldList('maps');
        }).property('maps'),

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
         * Hashmap of registers (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property registers
         * @type {Object}
         */
        registers: Ember['default'].computed('value', {
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
         * Returns a list of Registers for this map, sorted by field name.
         *
         * @method registersList
         * @return {Array<RiakObjectMapField>}
         */
        registersList: (function registersList() {
            return this.fieldList('registers');
        }).property('registers'),

        /**
         * Hashmap of sets (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property sets
         * @type {Object}
         */
        sets: Ember['default'].computed('value', {
            get: function get() {
                return this.get('value').sets;
            },
            set: function set(key, val) {
                var contents = this.get('value');
                contents.sets = val;
                this.set('value', contents);
            }
        }),

        /**
         * Returns a list of Sets for this map, sorted by field name.
         *
         * @method setsList
         * @return {Array<RiakObjectMapField>}
         */
        setsList: (function setsList() {
            return this.fieldList('sets');
        }).property('sets')
    });
    exports['default'] = RiakObjectEmbeddedMap;

});
define('ember-riak-explorer/pods/riak-object/map/controller', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/controller'], function (exports, Ember, RiakObjectController) {

    'use strict';

    var RiakObjectMapController = RiakObjectController['default'].extend({
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
                Ember['default'].run.later(function () {
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
define('ember-riak-explorer/pods/riak-object/map/model', ['exports', 'ember-riak-explorer/pods/riak-object/model', 'ember'], function (exports, RiakObject, Ember) {

    'use strict';

    var RiakObjectMap = RiakObject['default'].extend({
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
         * Hashmap of counters (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property counters
         * @type {Object}
         */
        counters: Ember['default'].computed('contents', {
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
         * Hashmap of flags (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property flags
         * @type {Object}
         */
        flags: Ember['default'].computed('contents', {
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
         * Hashmap of embedded Maps (`RiakObjectEmbeddedMap` instances) in this map,
         * keyed by field name.
         *
         * @property maps
         * @type {Object}
         */
        maps: Ember['default'].computed('contents', {
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
         * Hashmap of registers (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property registers
         * @type {Object}
         */
        registers: Ember['default'].computed('contents', {
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
         * Hashmap of sets (`RiakObjectMapField` instances) for this map,
         * keyed by field name.
         *
         * @property sets
         * @type {Object}
         */
        sets: Ember['default'].computed('contents', {
            get: function get() {
                return this.get('contents').sets;
            },
            set: function set(key, value) {
                var contents = this.get('contents');
                contents.sets = value;
                this.set('contents', contents);
            }
        }),

        /**
         * Returns a list of Sets for this map, sorted by field name.
         *
         * @method setsList
         * @return {Array<RiakObjectMapField>}
         */
        setsList: (function setsList() {
            return this.fieldList('sets');
        }).property('sets')
    });
    exports['default'] = RiakObjectMap;

});
define('ember-riak-explorer/pods/riak-object/map/route', ['exports', 'ember-riak-explorer/pods/riak-object/route'], function (exports, RiakObjectRoute) {

	'use strict';

	var RiakObjectMapRoute = RiakObjectRoute['default'].extend({});
	exports['default'] = RiakObjectMapRoute;

});
define('ember-riak-explorer/pods/riak-object/map/template', ['exports'], function (exports) {

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
                "line": 15,
                "column": 2
              },
              "end": {
                "line": 17,
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
            ["inline","object-contents-map",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[16,34],[16,39]]]]],[],[]],"deleteObject","deleteObject"],["loc",[null,[16,6],[16,69]]]]
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
                "line": 17,
                "column": 2
              },
              "end": {
                "line": 19,
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
            ["content","loading-spinner",["loc",[null,[18,6],[18,25]]]]
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
              "line": 14,
              "column": 0
            },
            "end": {
              "line": 20,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.isLoaded",["loc",[null,[15,8],[15,22]]]]],[],0,1,["loc",[null,[15,2],[19,9]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(element0,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[3,12],[3,27]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[4,15],[4,33]]]]],[],[]],"bucketId",["subexpr","@mut",[["get","model.bucketId",["loc",[null,[5,11],[5,25]]]]],[],[]],"keyId",["subexpr","@mut",[["get","model.key",["loc",[null,[6,8],[6,17]]]]],[],[]]],["loc",[null,[2,2],[7,4]]]],
        ["inline","view-label",[],["pre-label","Map Object","label",["subexpr","@mut",[["get","model.key",["loc",[null,[10,8],[10,17]]]]],[],[]]],["loc",[null,[8,2],[10,19]]]],
        ["block","dashboard-module",[],[],0,null,["loc",[null,[14,0],[20,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/pods/riak-object/map-field/model', ['exports', 'ember-data'], function (exports, DS) {

    'use strict';

    var RiakObjectMapField = DS['default'].Model.extend({
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

        /**
         * Field type (one of: `register`, `flag`, `map`, `counter`, `set`)
         *
         * @property fieldType
         * @type String
         * @readOnly
         */
        fieldType: DS['default'].attr('string'),

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
         * Name of the map field (has to end in `_<field type>`).
         *
         * @property name
         * @type String
         * @readOnly
         */
        name: DS['default'].attr('string'),

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
         * Parent map (embedded or top-level) in which this field resides.
         *
         * @property parent
         * @type (RiakObjectMap|RiakObjectMapField)
         */
        parent: DS['default'].attr(),

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

        /**
         * The actual map containing these fields (this may be a Standalone top-level
         * map, or a nested map field. When a map is nested just one level deep, the
         * parentMap is same as rootMap. For fields nested several levels deep, the
         * parent map will be an embedded map field.
         *
         * @property parentMap
         * @type (RiakObjectMap|RiakObjectEmbeddedMap)
         */
        parentMap: DS['default'].attr(),

        /**
         * Top-level standalone map in which this field lives.
         *
         * @property rootMap
         * @type RiakObjectMap
         */
        rootMap: DS['default'].attr(),

        /**
         * Value/contents of the map field.
         * String values for Registers, boolean values for Flags,
         * arrays for Sets, numbers for Counters, and Object for Maps.
         *
         * @property value
         * @type (String|Boolean|Array|Object)
         */
        value: DS['default'].attr(),

        valueForDisplay: (function valueForDisplay() {
            return JSON.stringify(this.get('value'));
        }).property('value')
    });
    exports['default'] = RiakObjectMapField;

});
define('ember-riak-explorer/pods/riak-object/model', ['exports', 'ember-data'], function (exports, DS) {

  'use strict';

  var RiakObject = DS['default'].Model.extend({
    /**
     * Riak Bucket in which this object lives.
     * @property bucket
     * @type Bucket
     * @writeOnce
     */
    bucket: DS['default'].belongsTo('bucket'),

    /**
     * Riak Bucket Type in which this object lives.
     * @property bucketType
     * @type BucketType
     * @writeOnce
     */
    bucketType: DS['default'].belongsTo('bucket-type'),

    /**
     * Riak cluster in which this object lives.
     * @property cluster
     * @type Cluster
     * @writeOnce
     */
    cluster: DS['default'].belongsTo('cluster'),

    /**
     * The value/contents of the object.
     * @property contents
     * @type Object
     */
    contents: DS['default'].attr(),

    /**
     * Has the object been fully loaded from the server?
     * @property isLoaded
     * @type Boolean
     * @default false
     */
    isLoaded: DS['default'].attr('boolean', { defaultValue: false }),

    /**
     * The object's primary key.
     * @property key
     * @type String
     * @writeOnce
     */
    key: DS['default'].attr('string'),

    /**
     * Was this object marked as deleted by Explorer UI?
     * Note: Deleted objects may still show up in the API-side key list cache.
     * @property markedDeleted
     * @type Boolean
     * @default false
     */
    markedDeleted: DS['default'].attr('boolean', { defaultValue: false }),

    /**
     * Riak object headers/metadata.
     * @property metadata
     * @type ObjectMetadata
     */
    metadata: DS['default'].belongsTo('object-metadata'),

    /**
     * The URL to fetch the raw contents of the object directly from server.
     * Used with the 'View Raw' button.
     * @property rawUrl
     * @type String
     * @writeOnce
     */
    rawUrl: DS['default'].attr('string'),

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
define('ember-riak-explorer/pods/riak-object/route', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    var RiakObjectRoute = Ember['default'].Route.extend({
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
            var store = this.store;
            return explorer.getBucket(params.clusterId, params.bucketTypeId, params.bucketId, store).then(function (bucket) {
                return explorer.getRiakObject(bucket, params.key, store);
            });
        },

        setupController: function setupController(controller, model) {
            this._super(controller, model);
            if (!model.get('isLoaded')) {
                this.explorer.getRiakObject(model.get('bucket'), model.get('key'), this.store).then(function (object) {
                    controller.set('model', object);
                });
            }
        }
    });
    exports['default'] = RiakObjectRoute;

});
define('ember-riak-explorer/pods/riak-object/set/controller', ['exports', 'ember', 'ember-riak-explorer/pods/riak-object/controller'], function (exports, Ember, RiakObjectController) {

    'use strict';

    var RiakObjectSetController = RiakObjectController['default'].extend({
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
                Ember['default'].run.later(function () {
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
define('ember-riak-explorer/pods/riak-object/set/model', ['exports', 'ember-riak-explorer/pods/riak-object/model'], function (exports, RiakObject) {

    'use strict';

    var RiakObjectSet = RiakObject['default'].extend({
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
define('ember-riak-explorer/pods/riak-object/set/route', ['exports', 'ember-riak-explorer/pods/riak-object/route'], function (exports, RiakObjectRoute) {

	'use strict';

	var RiakObjectSetRoute = RiakObjectRoute['default'].extend({});
	exports['default'] = RiakObjectSetRoute;

});
define('ember-riak-explorer/pods/riak-object/set/template', ['exports'], function (exports) {

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
                "column": 2
              },
              "end": {
                "line": 17,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","object-contents-set",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[15,32],[15,37]]]]],[],[]],"deleteObject","deleteObject","removeElement","removeElement","addElement","addElement"],["loc",[null,[15,4],[16,59]]]]
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
                "line": 17,
                "column": 2
              },
              "end": {
                "line": 19,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["content","loading-spinner",["loc",[null,[18,4],[18,23]]]]
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
              "line": 13,
              "column": 0
            },
            "end": {
              "line": 20,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.isLoaded",["loc",[null,[14,8],[14,22]]]]],[],0,1,["loc",[null,[14,2],[19,9]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(element0,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[3,12],[3,27]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[4,15],[4,33]]]]],[],[]],"bucketId",["subexpr","@mut",[["get","model.bucketId",["loc",[null,[5,11],[5,25]]]]],[],[]],"keyId",["subexpr","@mut",[["get","model.key",["loc",[null,[6,8],[6,17]]]]],[],[]]],["loc",[null,[2,2],[7,4]]]],
        ["inline","view-label",[],["pre-label","Set Object","label",["subexpr","@mut",[["get","model.key",["loc",[null,[10,8],[10,17]]]]],[],[]]],["loc",[null,[8,2],[10,19]]]],
        ["block","dashboard-module",[],["label","Set data"],0,null,["loc",[null,[13,0],[20,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/pods/riak-object/template', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        var child0 = (function() {
          return {
            meta: {
              "revision": "Ember@1.13.5",
              "loc": {
                "source": null,
                "start": {
                  "line": 18,
                  "column": 6
                },
                "end": {
                  "line": 18,
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
                "line": 14,
                "column": 2
              },
              "end": {
                "line": 20,
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
            dom.setAttribute(el1,"class","riak-object");
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
            morphs[0] = dom.createMorphAt(element0,1,1);
            morphs[1] = dom.createMorphAt(element0,3,3);
            morphs[2] = dom.createMorphAt(element0,5,5);
            return morphs;
          },
          statements: [
            ["inline","object-metadata",[],["metadata",["subexpr","@mut",[["get","model.metadata",["loc",[null,[16,33],[16,47]]]]],[],[]],"isEditing",false],["loc",[null,[16,6],[16,65]]]],
            ["inline","object-contents",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[17,30],[17,35]]]]],[],[]],"isEditing",false,"deleteObject","deleteObject"],["loc",[null,[17,6],[17,81]]]],
            ["block","object-version",[],["object",["subexpr","@mut",[["get","model",["loc",[null,[18,31],[18,36]]]]],[],[]]],0,null,["loc",[null,[18,6],[18,57]]]]
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
                "line": 20,
                "column": 2
              },
              "end": {
                "line": 22,
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["content","loading-spinner",["loc",[null,[21,4],[21,23]]]]
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
              "line": 13,
              "column": 0
            },
            "end": {
              "line": 23,
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.isLoaded",["loc",[null,[14,8],[14,22]]]]],[],0,1,["loc",[null,[14,2],[22,9]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
            "line": 26,
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
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element1,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","model.clusterId",["loc",[null,[3,12],[3,27]]]]],[],[]],"bucketTypeId",["subexpr","@mut",[["get","model.bucketTypeId",["loc",[null,[4,15],[4,33]]]]],[],[]],"bucketId",["subexpr","@mut",[["get","model.bucketId",["loc",[null,[5,11],[5,25]]]]],[],[]],"keyId",["subexpr","@mut",[["get","model.key",["loc",[null,[6,8],[6,17]]]]],[],[]]],["loc",[null,[2,2],[7,4]]]],
        ["inline","view-label",[],["pre-label","Riak Object","label",["subexpr","@mut",[["get","model.key",["loc",[null,[10,8],[10,17]]]]],[],[]]],["loc",[null,[8,2],[10,19]]]],
        ["block","dashboard-module",[],[],0,null,["loc",[null,[13,0],[23,21]]]]
      ],
      locals: [],
      templates: [child0]
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
        this.route('riak-object', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/key/:key' });
        this.route('riak-object.edit', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/key/:key/edit' });
        this.route('riak-object.counter', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/counter/:key' });
        this.route('riak-object.set', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/set/:key' });
        this.route('riak-object.map', { path: '/cluster/:clusterId/bucket_type/:bucketTypeId/bucket/:bucketId/map/:key' });
        this.route('riak-ping');
        this.route('node-stats');
        this.route('error', { path: '/error' }, function () {
            this.route('unknown');
            this.route('cluster-not-found');
            this.route('object-not-found');
            this.route('service-not-found');
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
            var self = this;

            return this.store.findAll('cluster').then(function (data) {
                return data;
            }, function (error) {
                self.transitionTo('error.service-not-found');
            });
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
define('ember-riak-explorer/routes/error/service-not-found', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Route.extend({});

});
define('ember-riak-explorer/routes/error/unknown', ['exports', 'ember'], function (exports, Ember) {

	'use strict';

	exports['default'] = Ember['default'].Route.extend({});

});
define('ember-riak-explorer/routes/explorer-api', ['exports', 'ember', 'ember-riak-explorer/config/environment'], function (exports, Ember, config) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        model: function model() {
            var serviceName = 'Riak Explorer';
            var pingUrl = config['default'].baseURL + 'explore/ping';
            var propsUrl = config['default'].baseURL + 'explore/props';

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
define('ember-riak-explorer/routes/node-stats', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            nodeId: {
                refreshModel: true
            }
        },

        model: function model(params) {
            return this.explorer.getNodeStats(params.nodeId);
        }
    });

});
define('ember-riak-explorer/routes/riak-ping', ['exports', 'ember'], function (exports, Ember) {

    'use strict';

    exports['default'] = Ember['default'].Route.extend({
        queryParams: {
            nodeId: {
                refreshModel: true
            }
        },

        model: function model(params) {
            return this.explorer.getNodePing(params.nodeId);
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
        isNewSerializerAPI: true,

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
define('ember-riak-explorer/services/explorer', ['exports', 'ember', 'ember-riak-explorer/config/environment', 'ember-riak-explorer/utils/riak-util'], function (exports, Ember, config, objectToArray) {

    'use strict';

    exports['default'] = Ember['default'].Service.extend({
        /**
         * User-configurable URL prefix for the Explorer GUI.
         * (Also the URL prefix for the Explorer API).
         * Currently, the options are: '/' or '/admin/'.
         *
         * @property apiURL
         * @type String
         * @default '/'
         */
        apiURL: config['default'].baseURL,

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
         * @param store {DS.Store} Ember Data store, used to instantiate field models
         * @return {Object} A hash of fields indexed by CRDT type and field name.
         */
        collectMapFields: function collectMapFields(rootMap, parentMap, payload, store) {
            var contents = {
                counters: {},
                flags: {},
                registers: {},
                sets: {},
                maps: {}
            };
            var field;

            for (var fieldName in payload) {
                if (fieldName.endsWith('_counter')) {
                    field = store.createRecord('riak-object.map-field', {
                        fieldType: 'counter',
                        name: fieldName,
                        rootMap: rootMap,
                        parentMap: parentMap,
                        value: payload[fieldName]
                    });
                    contents.counters[fieldName] = field;
                }
                if (fieldName.endsWith('_flag')) {
                    field = store.createRecord('riak-object.map-field', {
                        fieldType: 'flag',
                        name: fieldName,
                        rootMap: rootMap,
                        parentMap: parentMap,
                        value: payload[fieldName]
                    });
                    contents.flags[fieldName] = field;
                }
                if (fieldName.endsWith('_register')) {
                    field = store.createRecord('riak-object.map-field', {
                        fieldType: 'register',
                        name: fieldName,
                        rootMap: rootMap,
                        parentMap: parentMap,
                        value: payload[fieldName]
                    });
                    contents.registers[fieldName] = field;
                }
                if (fieldName.endsWith('_set')) {
                    field = store.createRecord('riak-object.map-field', {
                        fieldType: 'set',
                        name: fieldName,
                        rootMap: rootMap,
                        parentMap: parentMap,
                        value: payload[fieldName]
                    });
                    contents.sets[fieldName] = field;
                }
                if (fieldName.endsWith('_map')) {
                    field = store.createRecord('riak-object.embedded-map', {
                        fieldType: 'map',
                        name: fieldName,
                        rootMap: rootMap,
                        parentMap: parentMap
                    });
                    // This `field` becomes the `parentMap` for the nested fields.
                    // `rootMap` stays the same
                    var mapFields = this.collectMapFields(rootMap, field, payload[fieldName], store);
                    field.value = mapFields;
                    contents.maps[fieldName] = field;
                }
            }
            return contents;
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
         * @param store {DS.Store}
         * @return {BucketList}
         */
        createBucketList: function createBucketList(data, cluster, bucketType, store) {
            // Turn a list of bucket names into a list of actual bucket instances
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

        /**
         * Creates and returns a KeyList instance, given the results of a
         * 'fetch cached Key List' call to the Explorer API.
         * @see ExplorerService.getBucketWithKeyList
         *
         * @method createKeyList
         * @param data {Hash}
         * @param bucket {Bucket}
         * @param store {DS.Store}
         * @return {KeyList}
         */
        createKeyList: function createKeyList(data, bucket, store) {
            var explorer = this;
            if (!data) {
                // No data, return an empty KeyList
                return store.createRecord('key-list', {
                    bucket: bucket,
                    cluster: bucket.get('cluster')
                });
            }
            // The model name depends on the "object type" - plain Object, CRDT, etc
            var modelName = bucket.get('objectModelName');

            // Cycle through the list of keys and create actual RiakObject instances
            var keyList = data.keys.keys.map(function (key) {
                var obj = store.createRecord(modelName, {
                    key: key,
                    bucket: bucket,
                    bucketType: bucket.get('bucketType'),
                    cluster: bucket.get('cluster'),
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

        /**
         * Parses and returns the contents/value of a Riak Object, depending on
         * whether it's a CRDT or a plain object.
         *
         * @method createObjectContents
         * @param {Bucket} bucket
         * @param {RiakObject} newObject
         * @param {Object} payload
         * @param {DS.Store} store
         * @return {Object}
         */
        createObjectContents: function createObjectContents(bucket, newObject, payload, store) {
            var contents;
            if (bucket.get('props').get('isMap')) {
                contents = this.collectMapFields(newObject, newObject, payload.value, store);
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
         * @param store {DS.Store}
         * @param url {String} The URL to download the "raw" object payload
         *          (via an Explorer proxy request direct to Riak)
         * @return {RiakObject|RiakObjectCounter|RiakObjectMap|RiakObjectSet}
         */
        createObjectFromAjax: function createObjectFromAjax(key, bucket, rawHeader, payload, store, url) {
            var metadata = this.createObjectMetadata(rawHeader, store);
            var modelName = bucket.get('objectModelName');
            var newObject = store.createRecord(modelName, {
                key: key,
                bucket: bucket,
                bucketType: bucket.get('bucketType'),
                cluster: bucket.get('cluster'),
                metadata: metadata,
                isLoaded: true,
                rawUrl: url
            });
            var contents = this.createObjectContents(bucket, newObject, payload, store);
            newObject.set('contents', contents);
            return newObject;
        },

        /**
         * Creates and returns an ObjectMetadata instance by parsing the raw
         * header string returned by AJAX calls, if available.
         *
         * @method createObjectMetadata
         * @param rawHeader {String}
         * @param store {DS.Store}
         * @return {ObjectMetadata}
         */
        createObjectMetadata: function createObjectMetadata(rawHeader, store) {
            if (!rawHeader) {
                return store.createRecord('object-metadata');
            }
            return store.createRecord('object-metadata', {
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
                throw new Ember['default'].Error('Invalid data type or unsupported operation: ' + operationType);
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
                    throw new Ember['default'].Error('Unsupported Update Map operation: ' + operationType);
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
            var url = this.apiURL + 'explore/clusters/' + bucket.get('clusterId') + '/bucket_types/' + bucket.get('bucketTypeId') + '/buckets/' + bucket.get('bucketId');

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
            var url = this.getClusterProxyUrl(object.get('clusterId')) + '/types/' + object.get('bucketTypeId') + '/buckets/' + object.get('bucketId') + '/keys/' + object.get('key');

            object.set('markedDeleted', true);

            var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
                Ember['default'].$.ajax({
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
         * @param {DS.Store} store
         * @return {Bucket}
         */
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

        /**
         * Performs a 'Fetch cached Bucket List' API call to Explorer.
         * If the call encounters a 404 Not Found (meaning, the bucket list cache
         * is empty), it proactively kicks off a Bucket Cache Refresh operation.
         * @see ExplorerService.bucketCacheRefresh
         *
         * @method getBucketList
         * @param {Cluster} cluster
         * @param {BucketType} bucketType
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<BucketList>} Result of the AJAX request
         */
        getBucketList: function getBucketList(cluster, bucketType, store) {
            var start = arguments.length <= 3 || arguments[3] === undefined ? 1 : arguments[3];
            var rows = arguments.length <= 4 || arguments[4] === undefined ? this.pageSize : arguments[4];

            var explorer = this;
            var clusterId = cluster.get('clusterId');
            var bucketTypeId = bucketType.get('bucketTypeId');
            var url = this.apiURL + 'explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets?start=' + start + '&rows=' + rows;

            return new Ember['default'].RSVP.Promise(function (resolve, reject) {
                var xhrConfig = {
                    url: url,
                    dataType: 'json',
                    type: 'GET',
                    success: function success(data) {
                        bucketType.set('isBucketListLoaded', true);
                        resolve(explorer.createBucketList(data, cluster, bucketType, store));
                    },
                    error: function error(jqXHR, textStatus) {
                        // Fail (likely a 404, cache not yet created)
                        if (jqXHR.status === 404) {
                            // Kick off a Cache Refresh, and repeat the getBucketList request
                            console.log("kicking off cache refresh...");
                            explorer.bucketCacheRefresh(clusterId, bucketTypeId);
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
                    }
                };

                Ember['default'].$.ajax(xhrConfig);
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
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<BucketProps>}
         */
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
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<BucketType>}
         */
        getBucketType: function getBucketType(clusterId, bucketTypeId, store) {
            var self = this;
            return self.getCluster(clusterId, store).then(function (cluster) {
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
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<BucketType>}
         */
        getBucketTypeWithBucketList: function getBucketTypeWithBucketList(bucketType, cluster, store, start, row) {
            return this.getBucketList(cluster, bucketType, store, start, row).then(function (bucketList) {
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
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<Array<BucketType>>|Array<BucketType>}
         */
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

        /**
         * Initializes a given bucket with its Key List (by fetching it via AJAX),
         * and returns that bucket instance.
         *
         * @method getBucketWithKeyList
         * @param {Bucket} bucket
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<Bucket>}
         */
        getBucketWithKeyList: function getBucketWithKeyList(bucket, store, start, rows) {
            return this.getKeyList(bucket, store, start, rows).then(function (keyList) {
                bucket.set('keyList', keyList);
                return bucket;
            });
        },

        /**
         * Creates and returns a Cluster object and initializes it with dependent
         * data (including all its Bucket Types and Search Indexes).
         *
         * @method getCluster
         * @param {String} clusterId
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<Cluster>}
         */
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
         * @method getIndexes
         * @return {Array<Hash>}
         * @example
         *    [{"name":"customers","n_val":3,"schema":"_yz_default"}]
         */
        getIndexes: function getIndexes(clusterId) {
            var url = this.getClusterProxyUrl(clusterId) + '/search/index';

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
        },

        /**
         * Fetches and creates a cached Key List for a given bucket.
         *
         * @method getKeyList
         * @param {Bucket} bucket
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise} result of the AJAX call
         */
        getKeyList: function getKeyList(bucket, store) {
            var start = arguments.length <= 2 || arguments[2] === undefined ? 1 : arguments[2];
            var rows = arguments.length <= 3 || arguments[3] === undefined ? this.pageSize : arguments[3];

            var clusterId = bucket.get('clusterId');
            var bucketTypeId = bucket.get('bucketTypeId');
            var bucketId = bucket.get('bucketId');
            var explorer = this;

            var url = this.apiURL + 'explore/clusters/' + clusterId + '/bucket_types/' + bucketTypeId + '/buckets/' + bucketId + '/keys?start=' + start + '&rows=' + rows;

            return new Ember['default'].RSVP.Promise(function (resolve, reject) {
                var xhrConfig = {
                    url: url,
                    dataType: 'json',
                    type: 'GET',
                    success: function success(data) {
                        bucket.set('isKeyListLoaded', true);
                        resolve(explorer.createKeyList(data, bucket, store));
                    },
                    error: function error(jqXHR, textStatus) {
                        if (jqXHR.status === 404) {
                            // Empty cache (need to kick off a refresh)
                            explorer.keyCacheRefresh(clusterId, bucketTypeId, bucketId);
                            // Results in returning an empty (Loading..) key list
                            Ember['default'].run(null, resolve, null);
                        } else {
                            // Some other error
                            Ember['default'].run(null, reject, textStatus);
                        }
                    }
                };

                Ember['default'].$.ajax(xhrConfig);
            });
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
        },

        /**
         * Returns all reachable nodes for a given cluster id
         *
         * @method getNodes
         * @param clusterId {String} Cluster ID (as specified in the explorer config)
         * @return {Ember.RSVP.Promise<Array<Object>>}
         * @example Sample response
         *    {"nodes":[{"id":"riak@127.0.0.1"}],"links":{"self":"/explore/clusters/default/nodes"}}
         */
        getNodes: function getNodes(clusterId) {
            var url = this.apiURL + 'explore/clusters/' + clusterId + '/nodes';

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
        },

        /**
         * Returns the results of a GET Node Stats HTTP call.
         *
         * @method getNodeStats
         * @param {String} nodeId
         * @return {Ember.RSVP.Promise<Array<Hash>>}
         */
        getNodeStats: function getNodeStats(nodeId) {
            var propsUrl = this.apiURL + 'riak/nodes/' + nodeId + '/stats';
            var propsResult = Ember['default'].$.ajax(propsUrl, { dataType: "json" });
            return propsResult.then(function (data) {
                var statsArray = objectToArray['default'](data);
                return {
                    node: nodeId,
                    stats: statsArray
                };
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
         * @param {DS.Store} store
         * @return {Ember.RSVP.Promise<RiakObject>} RiakObject or its subclasses (CRDTs)
         */
        getRiakObject: function getRiakObject(bucket, key, store) {
            var explorer = this;

            return new Ember['default'].RSVP.Promise(function (resolve, reject) {
                var ajaxHash = {
                    type: "GET",
                    cache: false,
                    headers: { 'Accept': '*/*, multipart/mixed' }
                };

                var processData;
                var headerString;
                var contents;
                var url = explorer.getClusterProxyUrl(bucket.get('clusterId')) + '/types/' + bucket.get('bucketTypeId') + '/buckets/' + bucket.get('bucketId');
                if (bucket.get('props').get('isCRDT')) {
                    url = url + '/datatypes/' + key;
                    processData = true; // Parse the payload as JSON
                    ajaxHash.dataType = 'json';
                    ajaxHash.success = function (data, textStatus, jqXHR) {
                        headerString = jqXHR.getAllResponseHeaders();
                        contents = data; // Parsed json
                        resolve(explorer.createObjectFromAjax(key, bucket, headerString, contents, store, url));
                    };
                } else {
                    // Regular Riak object
                    url = url + '/keys/' + key;
                    processData = false;
                    ajaxHash.success = function (data, textStatus, jqXHR) {
                        headerString = jqXHR.getAllResponseHeaders();
                        contents = jqXHR.responseText; // Unparsed payload
                        resolve(explorer.createObjectFromAjax(key, bucket, headerString, contents, store, url));
                    };
                }
                ajaxHash.processData = processData;
                ajaxHash.url = url;

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
         * Updates a RiakObject via an HTTP Store Object request to the cluster.
         *
         * @method saveObject
         * @param {RiakObject} object
         * @return {Ember.RSVP.Promise} Result of the AJAX request.
         */
        saveObject: function saveObject(object) {
            var url = this.getClusterProxyUrl(object.get('clusterId')) + '/types/' + object.get('bucketTypeId') + '/buckets/' + object.get('bucketId') + '/keys/' + object.get('key');

            var request = new Ember['default'].RSVP.Promise(function (resolve, reject) {
                Ember['default'].$.ajax({
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
            var url = this.getClusterProxyUrl(bucket.get('clusterId')) + '/types/' + bucket.get('bucketTypeId') + '/buckets/' + bucket.get('bucketId') + '/datatypes/' + object.get('key');
            var self = this;

            return new Ember['default'].RSVP.Promise(function (resolve, reject) {
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
                Ember['default'].$.ajax(ajaxHash);
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
              "line": 2,
              "column": 0
            },
            "end": {
              "line": 5,
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
          var el1 = dom.createTextNode("  ");
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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
          return morphs;
        },
        statements: [
          ["inline","sidebar-panel",[],["clusters",["subexpr","@mut",[["get","model",["loc",[null,[3,27],[3,32]]]]],[],[]]],["loc",[null,[3,2],[3,34]]]],
          ["content","results-panel",["loc",[null,[4,2],[4,19]]]]
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
        ["content","topbar-panel",["loc",[null,[1,0],[1,16]]]],
        ["block","wrapper-panel",[],[],0,null,["loc",[null,[2,0],[5,18]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/breadcrumb-component', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 4
            },
            "end": {
              "line": 6,
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["inline","link-to",[["get","clusterId",["loc",[null,[5,20],[5,29]]]],"cluster",["get","clusterId",["loc",[null,[5,40],[5,49]]]]],[],["loc",[null,[5,10],[5,51]]]]
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
              "column": 4
            },
            "end": {
              "line": 9,
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["inline","link-to",[["get","bucketTypeId",["loc",[null,[8,20],[8,32]]]],"bucket-type",["get","clusterId",["loc",[null,[8,47],[8,56]]]],["get","bucketTypeId",["loc",[null,[8,57],[8,69]]]]],[],["loc",[null,[8,10],[8,71]]]]
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
              "line": 10,
              "column": 4
            },
            "end": {
              "line": 12,
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["inline","link-to",[["get","bucketId",["loc",[null,[11,20],[11,28]]]],"bucket",["get","clusterId",["loc",[null,[11,38],[11,47]]]],["get","bucketTypeId",["loc",[null,[11,48],[11,60]]]],["get","bucketId",["loc",[null,[11,61],[11,69]]]]],[],["loc",[null,[11,10],[11,71]]]]
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
              "line": 13,
              "column": 4
            },
            "end": {
              "line": 15,
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["inline","link-to",[["get","keyId",["loc",[null,[14,20],[14,25]]]],"riak-object",["get","clusterId",["loc",[null,[14,40],[14,49]]]],["get","bucketTypeId",["loc",[null,[14,50],[14,62]]]],["get","bucketId",["loc",[null,[14,63],[14,71]]]],["get","keyId",["loc",[null,[14,72],[14,77]]]]],[],["loc",[null,[14,10],[14,79]]]]
        ],
        locals: [],
        templates: []
      };
    }());
    var child4 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 16,
              "column": 4
            },
            "end": {
              "line": 18,
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","pageTitle",["loc",[null,[17,10],[17,23]]]]
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
            "line": 21,
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","breadcrumb-container");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("ol");
        dom.setAttribute(el2,"class","breadcrumb");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("li");
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
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
        var element0 = dom.childAt(fragment, [0, 1]);
        var morphs = new Array(6);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
        morphs[1] = dom.createMorphAt(element0,3,3);
        morphs[2] = dom.createMorphAt(element0,4,4);
        morphs[3] = dom.createMorphAt(element0,5,5);
        morphs[4] = dom.createMorphAt(element0,6,6);
        morphs[5] = dom.createMorphAt(element0,7,7);
        return morphs;
      },
      statements: [
        ["inline","link-to",["overview","index"],[],["loc",[null,[3,8],[3,38]]]],
        ["block","if",[["get","clusterId",["loc",[null,[4,10],[4,19]]]]],[],0,null,["loc",[null,[4,4],[6,11]]]],
        ["block","if",[["get","bucketTypeId",["loc",[null,[7,10],[7,22]]]]],[],1,null,["loc",[null,[7,4],[9,11]]]],
        ["block","if",[["get","bucketId",["loc",[null,[10,10],[10,18]]]]],[],2,null,["loc",[null,[10,4],[12,11]]]],
        ["block","if",[["get","keyId",["loc",[null,[13,10],[13,15]]]]],[],3,null,["loc",[null,[13,4],[15,11]]]],
        ["block","if",[["get","pageTitle",["loc",[null,[16,10],[16,19]]]]],[],4,null,["loc",[null,[16,4],[18,11]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3, child4]
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
          var element1 = dom.childAt(fragment, [13]);
          var morphs = new Array(7);
          morphs[0] = dom.createMorphAt(fragment,3,3,contextualElement);
          morphs[1] = dom.createMorphAt(fragment,5,5,contextualElement);
          morphs[2] = dom.createMorphAt(fragment,7,7,contextualElement);
          morphs[3] = dom.createMorphAt(fragment,9,9,contextualElement);
          morphs[4] = dom.createMorphAt(fragment,11,11,contextualElement);
          morphs[5] = dom.createMorphAt(element1,1,1);
          morphs[6] = dom.createMorphAt(element1,3,3);
          return morphs;
        },
        statements: [
          ["content","model.props.quorum.r",["loc",[null,[22,11],[22,35]]]],
          ["content","model.props.quorum.w",["loc",[null,[22,40],[22,64]]]],
          ["content","model.props.quorum.pr",["loc",[null,[23,12],[23,37]]]],
          ["content","model.props.quorum.pw",["loc",[null,[23,43],[23,68]]]],
          ["content","model.props.quorum.dw",["loc",[null,[24,12],[24,37]]]],
          ["content","model.props.quorum.basic_quorum",["loc",[null,[26,25],[26,60]]]],
          ["content","model.props.quorum.basic_quorum",["loc",[null,[27,23],[27,58]]]]
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
                "line": 37,
                "column": 4
              },
              "end": {
                "line": 50,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
          },
          arity: 0,
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
            dom.setAttribute(el2,"class","key");
            var el3 = dom.createTextNode("Search Schema:");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"class","value");
            var el3 = dom.createTextNode("\n          ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("a");
            var el4 = dom.createTextNode("\n            ");
            dom.appendChild(el3, el4);
            var el4 = dom.createComment("");
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n          ");
            dom.appendChild(el3, el4);
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("\n        ");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n      ");
            dom.appendChild(el1, el2);
            dom.appendChild(el0, el1);
            var el1 = dom.createTextNode("\n      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("tr");
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"class","key");
            var el3 = dom.createTextNode("Index N_Val:");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"class","value");
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
            var element0 = dom.childAt(fragment, [1, 3, 1]);
            var morphs = new Array(3);
            morphs[0] = dom.createAttrMorph(element0, 'href');
            morphs[1] = dom.createMorphAt(element0,1,1);
            morphs[2] = dom.createMorphAt(dom.childAt(fragment, [3, 3]),0,0);
            return morphs;
          },
          statements: [
            ["attribute","href",["concat",[["get","model.cluster.proxyUrl",["loc",[null,[41,21],[41,43]]]],"/search/schema/",["get","model.index.schema",["loc",[null,[41,62],[41,80]]]]]]],
            ["content","model.index.schema",["loc",[null,[42,12],[42,34]]]],
            ["content","model.index.n_val",["loc",[null,[48,26],[48,47]]]]
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
                "line": 50,
                "column": 4
              },
              "end": {
                "line": 58,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/bucket-properties.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("      ");
            dom.appendChild(el0, el1);
            var el1 = dom.createElement("tr");
            var el2 = dom.createTextNode("\n          ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"class","key");
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n          ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"class","value");
            var el3 = dom.createTextNode("\n            ");
            dom.appendChild(el2, el3);
            var el3 = dom.createElement("em");
            var el4 = dom.createTextNode("Warning: Index with id ");
            dom.appendChild(el3, el4);
            var el4 = dom.createElement("code");
            var el5 = dom.createComment("");
            dom.appendChild(el4, el5);
            dom.appendChild(el3, el4);
            var el4 = dom.createTextNode("\n              has not been created on this cluster!");
            dom.appendChild(el3, el4);
            dom.appendChild(el2, el3);
            var el3 = dom.createTextNode("\n          ");
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
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1, 1]),0,0);
            return morphs;
          },
          statements: [
            ["content","model.props.searchIndexName",["loc",[null,[54,45],[54,76]]]]
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
              "line": 32,
              "column": 2
            },
            "end": {
              "line": 59,
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
          dom.setAttribute(el2,"class","key");
          var el3 = dom.createTextNode("Search Index:");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          dom.setAttribute(el2,"class","value");
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n");
          dom.appendChild(el0, el1);
          var el1 = dom.createComment("");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3]),0,0);
          morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["content","model.props.searchIndexName",["loc",[null,[35,24],[35,55]]]],
          ["block","if",[["get","model.index",["loc",[null,[37,10],[37,21]]]]],[],0,1,["loc",[null,[37,4],[58,11]]]]
        ],
        locals: [],
        templates: [child0, child1]
      };
    }());
    var child2 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 59,
              "column": 2
            },
            "end": {
              "line": 64,
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
          dom.setAttribute(el2,"class","key");
          var el3 = dom.createTextNode("Search Index:");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          dom.setAttribute(el2,"class","value");
          var el3 = dom.createTextNode("Not available (not being indexed)");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
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
                "line": 70,
                "column": 10
              },
              "end": {
                "line": 72,
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
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
            return morphs;
          },
          statements: [
            ["content","warning",["loc",[null,[71,16],[71,27]]]]
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
              "line": 65,
              "column": 2
            },
            "end": {
              "line": 76,
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
          dom.setAttribute(el2,"class","key");
          var el3 = dom.createTextNode("Warnings:");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n      ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("td");
          dom.setAttribute(el2,"class","value");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3, 1]),1,1);
          return morphs;
        },
        statements: [
          ["block","each",[["get","model.props.warnings",["loc",[null,[70,18],[70,38]]]]],[],0,null,["loc",[null,[70,10],[72,19]]]]
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
            "line": 78,
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
        dom.setAttribute(el1,"class","key-value-table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tr");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3,"class","key");
        var el4 = dom.createTextNode("Bucket Name:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3,"class","value");
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
        dom.setAttribute(el3,"class","key");
        var el4 = dom.createTextNode("Object type:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3,"class","value");
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
        dom.setAttribute(el3,"class","key");
        var el4 = dom.createTextNode("Conflict Res. Strategy:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3,"class","value");
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
        dom.setAttribute(el3,"class","key");
        var el4 = dom.createTextNode("Quorum:");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("td");
        dom.setAttribute(el3,"class","value");
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
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
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
        var element3 = dom.childAt(element2, [7, 3]);
        var morphs = new Array(7);
        morphs[0] = dom.createMorphAt(dom.childAt(element2, [1, 3]),1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element2, [3, 3]),0,0);
        morphs[2] = dom.createMorphAt(dom.childAt(element2, [5, 3]),0,0);
        morphs[3] = dom.createMorphAt(element3,1,1);
        morphs[4] = dom.createMorphAt(element3,3,3);
        morphs[5] = dom.createMorphAt(element2,9,9);
        morphs[6] = dom.createMorphAt(element2,10,10);
        return morphs;
      },
      statements: [
        ["content","model.name",["loc",[null,[5,6],[5,20]]]],
        ["content","model.props.objectType",["loc",[null,[10,22],[10,48]]]],
        ["content","model.props.resolutionStrategy",["loc",[null,[14,22],[14,56]]]],
        ["content","model.props.nVal",["loc",[null,[19,13],[19,33]]]],
        ["block","if",[["get","model.props.quorumRelevant",["loc",[null,[20,12],[20,38]]]]],[],0,null,["loc",[null,[20,6],[29,13]]]],
        ["block","if",[["get","model.props.isSearchIndexed",["loc",[null,[32,8],[32,35]]]]],[],1,2,["loc",[null,[32,2],[64,9]]]],
        ["block","if",[["get","model.props.warnings",["loc",[null,[65,8],[65,28]]]]],[],3,null,["loc",[null,[65,2],[76,9]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
    };
  }()));

});
define('ember-riak-explorer/templates/components/bucket-types', ['exports'], function (exports) {

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
              "line": 26,
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
          ["inline","link.bucket-type",[],["bucketType",["subexpr","@mut",[["get","bt",["loc",[null,[14,42],[14,44]]]]],[],[]],"btnBlock",true],["loc",[null,[14,12],[14,60]]]],
          ["content","bt.props.objectType",["loc",[null,[17,12],[17,35]]]],
          ["content","bt.props.nVal",["loc",[null,[20,12],[20,29]]]],
          ["content","bt.props.resolutionStrategy",["loc",[null,[23,12],[23,43]]]]
        ],
        locals: ["bt"],
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
        ["block","each",[["get","bucketTypes",["loc",[null,[11,8],[11,19]]]]],[],0,null,["loc",[null,[11,0],[26,9]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/button/delete-object', ['exports'], function (exports) {

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
        "moduleName": "ember-riak-explorer/templates/components/button/delete-object.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-sm btn-danger");
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
define('ember-riak-explorer/templates/components/button/edit-object', ['exports'], function (exports) {

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
          "moduleName": "ember-riak-explorer/templates/components/button/edit-object.hbs"
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        return morphs;
      },
      statements: [
        ["block","link-to",["riak-object.edit",["get","object",["loc",[null,[1,30],[1,36]]]]],["classNames","btn btn-sm btn-primary"],0,null,["loc",[null,[1,0],[4,27]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/button/object-view-raw', ['exports'], function (exports) {

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
        "moduleName": "ember-riak-explorer/templates/components/button/object-view-raw.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("a");
        dom.setAttribute(el1,"class","btn btn-sm btn-primary");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2,"class","glyphicon glyphicon-download");
        dom.setAttribute(el2,"aria-hidden","true");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    View raw\n");
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
      statements: [
        ["attribute","href",["concat",[["get","object.rawUrl",["loc",[null,[1,11],[1,24]]]]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/button/refresh-buckets', ['exports'], function (exports) {

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
        "moduleName": "ember-riak-explorer/templates/components/button/refresh-buckets.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("button");
        dom.setAttribute(el1,"type","button");
        dom.setAttribute(el1,"class","btn btn-xs btn-primary");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("span");
        dom.setAttribute(el2,"class","glyphicon glyphicon-refresh");
        dom.setAttribute(el2,"aria-hidden","true");
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
      statements: [
        ["element","action",["refreshBuckets",["get","bucketType",["loc",[null,[1,79],[1,89]]]]],[],["loc",[null,[1,53],[1,91]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/button/refresh-keys', ['exports'], function (exports) {

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
        "moduleName": "ember-riak-explorer/templates/components/button/refresh-keys.hbs"
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
define('ember-riak-explorer/templates/components/button/set-element-remove', ['exports'], function (exports) {

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
        "moduleName": "ember-riak-explorer/templates/components/button/set-element-remove.hbs"
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
      statements: [
        ["element","action",["removeElement",["get","model",["loc",[null,[2,33],[2,38]]]],["get","item",["loc",[null,[2,39],[2,43]]]]],[],["loc",[null,[2,8],[2,45]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/dashboard-module', ['exports'], function (exports) {

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
          dom.setAttribute(el1,"class","module-label");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
          return morphs;
        },
        statements: [
          ["content","label",["loc",[null,[4,6],[4,15]]]]
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
        dom.setAttribute(el2,"class","module-content");
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
        morphs[1] = dom.createMorphAt(element0,1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
        return morphs;
      },
      statements: [
        ["attribute","class",["concat",["dashboard-module ",["get","class",["loc",[null,[1,31],[1,36]]]]]]],
        ["block","if",[["get","label",["loc",[null,[2,8],[2,13]]]]],[],0,null,["loc",[null,[2,2],[6,9]]]],
        ["content","yield",["loc",[null,[8,4],[8,13]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/link/bucket-type', ['exports'], function (exports) {

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
          "moduleName": "ember-riak-explorer/templates/components/link/bucket-type.hbs"
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
        "moduleName": "ember-riak-explorer/templates/components/link/bucket-type.hbs"
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
        ["block","link-to",["bucket-type",["get","bucketType",["loc",[null,[1,25],[1,35]]]]],["class","btn btn-sm btn-primary btn-block cluster-resource-link"],0,null,["loc",[null,[1,0],[5,43]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/link/link-bucket', ['exports'], function (exports) {

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
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["content","bucket.bucketId",["loc",[null,[2,2],[2,21]]]]
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","link-to",["bucket",["get","bucket",["loc",[null,[1,20],[1,26]]]]],[],0,null,["loc",[null,[1,0],[3,12]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/link/link-cluster', ['exports'], function (exports) {

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
          "moduleName": "ember-riak-explorer/templates/components/link/link-cluster.hbs"
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
define('ember-riak-explorer/templates/components/link/link-object', ['exports'], function (exports) {

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
          "moduleName": "ember-riak-explorer/templates/components/link/link-object.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
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
          ["content","obj.key",["loc",[null,[2,12],[2,23]]]]
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
                "line": 4,
                "column": 4
              },
              "end": {
                "line": 4,
                "column": 58
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
            morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
            dom.insertBoundary(fragment, 0);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [
            ["content","obj.key",["loc",[null,[4,47],[4,58]]]]
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
          ["block","link-to",[["get","obj.bucket.objectModelName",["loc",[null,[4,15],[4,41]]]],["get","obj",["loc",[null,[4,42],[4,45]]]]],[],0,null,["loc",[null,[4,4],[4,70]]]]
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","obj.markedDeleted",["loc",[null,[1,6],[1,23]]]]],[],0,1,["loc",[null,[1,0],[5,7]]]]
      ],
      locals: [],
      templates: [child0, child1]
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
            "line": 5,
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
        dom.setAttribute(el1,"class","spinner");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","double-bounce1");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","double-bounce2");
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
define('ember-riak-explorer/templates/components/object-actions', ['exports'], function (exports) {

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
                "column": 4
              },
              "end": {
                "line": 5,
                "column": 14
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("        Cancel");
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
          var el1 = dom.createTextNode("    (Editing)\n");
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
          ["block","link-to",["riak-object",["get","model",["loc",[null,[3,29],[3,34]]]]],["classNames","btn btn-xs btn-default"],0,null,["loc",[null,[3,4],[5,26]]]]
        ],
        locals: [],
        templates: [child0]
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
                "line": 7,
                "column": 4
              },
              "end": {
                "line": 9,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","button.object-view-raw",[],["object",["subexpr","@mut",[["get","model",["loc",[null,[8,40],[8,45]]]]],[],[]]],["loc",[null,[8,8],[8,47]]]]
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
                "line": 10,
                "column": 4
              },
              "end": {
                "line": 12,
                "column": 4
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-actions.hbs"
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
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            return morphs;
          },
          statements: [
            ["inline","button.edit-object",[],["object",["subexpr","@mut",[["get","model",["loc",[null,[11,36],[11,41]]]]],[],[]]],["loc",[null,[11,8],[11,43]]]]
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
          var el1 = dom.createTextNode("    ");
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
          ["block","if",[["get","model.canBeViewedRaw",["loc",[null,[7,10],[7,30]]]]],[],0,null,["loc",[null,[7,4],[9,11]]]],
          ["block","if",[["get","model.canBeEdited",["loc",[null,[10,10],[10,27]]]]],[],1,null,["loc",[null,[10,4],[12,11]]]],
          ["inline","button.delete-object",[],["action","deleteObject","object",["subexpr","@mut",[["get","model",["loc",[null,[14,15],[14,20]]]]],[],[]]],["loc",[null,[13,4],[14,22]]]]
        ],
        locals: [],
        templates: [child0, child1]
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","if",[["get","isEditing",["loc",[null,[1,6],[1,15]]]]],[],0,1,["loc",[null,[1,0],[15,7]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-counter', ['exports'], function (exports) {

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
        dom.setAttribute(el1,"class","key-value-table");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("tbody");
        var el3 = dom.createTextNode("\n  ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("tr");
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4,"class","key");
        var el5 = dom.createTextNode("Current Value:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4,"class","value counter-value");
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
        dom.setAttribute(el4,"class","key");
        var el5 = dom.createTextNode("\n      Change Value:\n    ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4,"class","value");
        var el5 = dom.createTextNode("\n      ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("form");
        dom.setAttribute(el5,"class","form-inline");
        var el6 = dom.createTextNode("\n        ");
        dom.appendChild(el5, el6);
        var el6 = dom.createElement("button");
        dom.setAttribute(el6,"type","button");
        dom.setAttribute(el6,"class","btn btn-primary");
        var el7 = dom.createTextNode("\n          ");
        dom.appendChild(el6, el7);
        var el7 = dom.createElement("span");
        dom.setAttribute(el7,"class","glyphicon glyphicon-minus");
        dom.setAttribute(el7,"aria-hidden","true");
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
        dom.setAttribute(el6,"type","button");
        dom.setAttribute(el6,"class","btn btn-primary");
        var el7 = dom.createTextNode("\n          ");
        dom.appendChild(el6, el7);
        var el7 = dom.createElement("span");
        dom.setAttribute(el7,"class","glyphicon glyphicon-plus");
        dom.setAttribute(el7,"aria-hidden","true");
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
        dom.setAttribute(el4,"class","key");
        var el5 = dom.createTextNode("Delete Object");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n    ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("td");
        dom.setAttribute(el4,"class","value");
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
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]),0,0);
        morphs[1] = dom.createElementMorph(element2);
        morphs[2] = dom.createMorphAt(element1,3,3);
        morphs[3] = dom.createElementMorph(element3);
        morphs[4] = dom.createMorphAt(dom.childAt(element0, [5, 3]),0,0);
        return morphs;
      },
      statements: [
        ["content","model.contentsForDisplay",["loc",[null,[5,36],[5,64]]]],
        ["element","action",["decrementCounter",["get","model",["loc",[null,[13,82],[13,87]]]]],[],["loc",[null,[13,54],[13,89]]]],
        ["inline","input",[],["value",["subexpr","@mut",[["get","model.valueChanger",["loc",[null,[16,22],[16,40]]]]],[],[]],"id","valueChanger","class","form-control value-changer"],["loc",[null,[16,8],[16,95]]]],
        ["element","action",["incrementCounter",["get","model",["loc",[null,[17,82],[17,87]]]]],[],["loc",[null,[17,54],[17,89]]]],
        ["inline","object-actions",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[25,45],[25,50]]]]],[],[]],"deleteObject","deleteObject"],["loc",[null,[25,22],[25,80]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-counters-embedded', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 4
            },
            "end": {
              "line": 21,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-counters-embedded.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row set-element-row");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-1");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3,"type","button");
          dom.setAttribute(el3,"class","btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n                  ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4,"class","glyphicon glyphicon-trash");
          dom.setAttribute(el4,"aria-hidden","true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-3 field-label");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-8");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("code");
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
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
          var element1 = dom.childAt(element0, [1, 1]);
          var morphs = new Array(3);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]),0,0);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 1]),1,1);
          return morphs;
        },
        statements: [
          ["element","action",["removeField",["get","model",["loc",[null,[8,45],[8,50]]]],["get","counter",["loc",[null,[8,51],[8,58]]]]],[],["loc",[null,[8,22],[8,60]]]],
          ["content","counter.name",["loc",[null,[13,24],[13,40]]]],
          ["content","counter.valueForDisplay",["loc",[null,[17,16],[17,43]]]]
        ],
        locals: ["counter"],
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
        "moduleName": "ember-riak-explorer/templates/components/object-contents-counters-embedded.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container set-contents-list");
        var el2 = dom.createTextNode("\n    ");
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
        morphs[0] = dom.createMorphAt(dom.childAt(element2, [1]),1,1);
        morphs[1] = dom.createMorphAt(element2,3,3);
        return morphs;
      },
      statements: [
        ["content","model.countersList.length",["loc",[null,[2,18],[2,47]]]],
        ["block","each",[["get","model.countersList",["loc",[null,[4,12],[4,30]]]]],[],0,null,["loc",[null,[4,4],[21,13]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-flags', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 4
            },
            "end": {
              "line": 21,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-flags.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row set-element-row");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-1");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3,"type","button");
          dom.setAttribute(el3,"class","btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n                  ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4,"class","glyphicon glyphicon-trash");
          dom.setAttribute(el4,"aria-hidden","true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-3 field-label");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-8");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("code");
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
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
          var element1 = dom.childAt(element0, [1, 1]);
          var morphs = new Array(3);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]),0,0);
          morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 1]),1,1);
          return morphs;
        },
        statements: [
          ["element","action",["removeField",["get","model",["loc",[null,[8,45],[8,50]]]],["get","flag",["loc",[null,[8,51],[8,55]]]]],[],["loc",[null,[8,22],[8,57]]]],
          ["content","flag.name",["loc",[null,[13,24],[13,37]]]],
          ["content","flag.value",["loc",[null,[17,16],[17,30]]]]
        ],
        locals: ["flag"],
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
        "moduleName": "ember-riak-explorer/templates/components/object-contents-flags.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container set-contents-list");
        var el2 = dom.createTextNode("\n    ");
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
        morphs[0] = dom.createMorphAt(dom.childAt(element2, [1]),1,1);
        morphs[1] = dom.createMorphAt(element2,3,3);
        return morphs;
      },
      statements: [
        ["content","model.flagsList.length",["loc",[null,[2,15],[2,41]]]],
        ["block","each",[["get","model.flagsList",["loc",[null,[4,12],[4,27]]]]],[],0,null,["loc",[null,[4,4],[21,13]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-map', ['exports'], function (exports) {

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
        dom.setAttribute(el1,"class","riak-object");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","object-contents");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-header");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","object-contents-actions");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-body");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4,"class","form-inline");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4,"class","form-inline");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4,"class","form-inline");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4,"class","form-inline");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4,"class","form-inline");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-footer");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Page loaded:");
        dom.appendChild(el4, el5);
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
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 1]),1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [1]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element1, [3]),1,1);
        morphs[3] = dom.createMorphAt(dom.childAt(element1, [5]),1,1);
        morphs[4] = dom.createMorphAt(dom.childAt(element1, [7]),1,1);
        morphs[5] = dom.createMorphAt(dom.childAt(element1, [9]),1,1);
        morphs[6] = dom.createMorphAt(dom.childAt(element0, [5]),3,3);
        return morphs;
      },
      statements: [
        ["inline","object-actions",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[5,39],[5,44]]]]],[],[]],"deleteObject","deleteObject"],["loc",[null,[5,16],[5,74]]]],
        ["inline","object-contents-registers",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[10,50],[10,55]]]]],[],[]],"addField","addField","editField","editField","removeField","removeField"],["loc",[null,[10,16],[13,47]]]],
        ["inline","object-contents-flags",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[17,46],[17,51]]]]],[],[]],"removeField","removeField"],["loc",[null,[17,16],[18,47]]]],
        ["inline","object-contents-counters-embedded",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[22,58],[22,63]]]]],[],[]],"removeField","removeField"],["loc",[null,[22,16],[23,47]]]],
        ["inline","object-contents-sets-embedded",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[27,54],[27,59]]]]],[],[]],"addElement","addElement","removeElement","removeElement"],["loc",[null,[27,16],[29,51]]]],
        ["inline","object-contents-maps-embedded",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[33,54],[33,59]]]]],[],[]],"removeField","removeField","addField","addField","editField","editField","addElement","addElement","removeElement","removeElement"],["loc",[null,[33,16],[36,20]]]],
        ["content","model.metadata.dateLoaded",["loc",[null,[40,42],[40,71]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-maps-embedded', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 4
            },
            "end": {
              "line": 49,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-maps-embedded.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row map-composite-field-name");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-12");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3,"type","button");
          dom.setAttribute(el3,"class","btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n                  remove map\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n                  \n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-12");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3,"class","form-inline");
          var el4 = dom.createTextNode("\n                    ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3,"class","form-inline");
          var el4 = dom.createTextNode("\n                    ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3,"class","form-inline");
          var el4 = dom.createTextNode("\n                    ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3,"class","form-inline");
          var el4 = dom.createTextNode("\n                    ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("form");
          dom.setAttribute(el3,"class","form-inline");
          var el4 = dom.createTextNode("\n                    ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
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
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [1]);
          var element2 = dom.childAt(fragment, [3, 1]);
          var morphs = new Array(7);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          morphs[2] = dom.createMorphAt(dom.childAt(element2, [1]),1,1);
          morphs[3] = dom.createMorphAt(dom.childAt(element2, [3]),1,1);
          morphs[4] = dom.createMorphAt(dom.childAt(element2, [5]),1,1);
          morphs[5] = dom.createMorphAt(dom.childAt(element2, [7]),1,1);
          morphs[6] = dom.createMorphAt(dom.childAt(element2, [9]),1,1);
          return morphs;
        },
        statements: [
          ["element","action",["removeField",["get","model",["loc",[null,[8,45],[8,50]]]],["get","map",["loc",[null,[8,51],[8,54]]]]],[],["loc",[null,[8,22],[8,56]]]],
          ["content","map.name",["loc",[null,[12,24],[12,36]]]],
          ["inline","object-contents-registers",[],["model",["subexpr","@mut",[["get","map",["loc",[null,[18,54],[18,57]]]]],[],[]],"addField","addField","editField","editField","removeField","removeField"],["loc",[null,[18,20],[21,51]]]],
          ["inline","object-contents-flags",[],["model",["subexpr","@mut",[["get","map",["loc",[null,[25,50],[25,53]]]]],[],[]],"removeField","removeField"],["loc",[null,[25,20],[26,51]]]],
          ["inline","object-contents-counters-embedded",[],["model",["subexpr","@mut",[["get","map",["loc",[null,[30,62],[30,65]]]]],[],[]],"removeField","removeField"],["loc",[null,[30,20],[31,51]]]],
          ["inline","object-contents-sets-embedded",[],["model",["subexpr","@mut",[["get","map",["loc",[null,[35,58],[35,61]]]]],[],[]],"removeField","removeField","addElement","addElement","removeElement","removeElement"],["loc",[null,[35,20],[38,55]]]],
          ["inline","object-contents-maps-embedded",[],["model",["subexpr","@mut",[["get","map",["loc",[null,[42,58],[42,61]]]]],[],[]],"removeField","removeField","addField","addField","editField","editField","addElement","addElement","removeElement","removeElement"],["loc",[null,[42,20],[45,24]]]]
        ],
        locals: ["map"],
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
        dom.setAttribute(el1,"class","container set-contents-list");
        var el2 = dom.createTextNode("\n    ");
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
        morphs[0] = dom.createMorphAt(dom.childAt(element3, [1]),1,1);
        morphs[1] = dom.createMorphAt(element3,3,3);
        return morphs;
      },
      statements: [
        ["content","model.mapsList.length",["loc",[null,[2,14],[2,39]]]],
        ["block","each",[["get","model.mapsList",["loc",[null,[4,12],[4,26]]]]],[],0,null,["loc",[null,[4,4],[49,13]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-registers', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 4
            },
            "end": {
              "line": 25,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-registers.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row set-element-row");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-1");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3,"type","button");
          dom.setAttribute(el3,"class","btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n                  ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4,"class","glyphicon glyphicon-trash");
          dom.setAttribute(el4,"aria-hidden","true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-3 field-label");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-8");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3,"type","button");
          dom.setAttribute(el3,"class","btn btn-xs btn-primary");
          var el4 = dom.createTextNode("\n                  ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("span");
          dom.setAttribute(el4,"class","glyphicon glyphicon-pencil");
          dom.setAttribute(el4,"aria-hidden","true");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("code");
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
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
          var element1 = dom.childAt(element0, [1, 1]);
          var element2 = dom.childAt(element0, [5]);
          var element3 = dom.childAt(element2, [1]);
          var morphs = new Array(4);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]),0,0);
          morphs[2] = dom.createElementMorph(element3);
          morphs[3] = dom.createMorphAt(dom.childAt(element2, [3]),1,1);
          return morphs;
        },
        statements: [
          ["element","action",["removeField",["get","model",["loc",[null,[8,45],[8,50]]]],["get","register",["loc",[null,[8,51],[8,59]]]]],[],["loc",[null,[8,22],[8,61]]]],
          ["content","register.name",["loc",[null,[13,24],[13,41]]]],
          ["element","action",["editField",["get","model",["loc",[null,[17,43],[17,48]]]],["get","register",["loc",[null,[17,49],[17,57]]]]],[],["loc",[null,[17,22],[17,59]]]],
          ["content","register.value",["loc",[null,[21,16],[21,34]]]]
        ],
        locals: ["register"],
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
        "moduleName": "ember-riak-explorer/templates/components/object-contents-registers.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container set-contents-list");
        var el2 = dom.createTextNode("\n    ");
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
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row set-element-row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-4 text-right");
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
        dom.setAttribute(el3,"class","col-md-8");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("button");
        dom.setAttribute(el4,"type","button");
        dom.setAttribute(el4,"class","btn btn-sm btn-primary");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5,"class","glyphicon glyphicon-plus");
        dom.setAttribute(el5,"aria-hidden","true");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n                Add Register");
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
        var element4 = dom.childAt(fragment, [0]);
        var element5 = dom.childAt(element4, [5]);
        var element6 = dom.childAt(element5, [3]);
        var element7 = dom.childAt(element6, [3]);
        var morphs = new Array(5);
        morphs[0] = dom.createMorphAt(dom.childAt(element4, [1]),1,1);
        morphs[1] = dom.createMorphAt(element4,3,3);
        morphs[2] = dom.createMorphAt(dom.childAt(element5, [1]),1,1);
        morphs[3] = dom.createMorphAt(element6,1,1);
        morphs[4] = dom.createElementMorph(element7);
        return morphs;
      },
      statements: [
        ["content","model.registersList.length",["loc",[null,[2,19],[2,49]]]],
        ["block","each",[["get","model.registersList",["loc",[null,[4,12],[4,31]]]]],[],0,null,["loc",[null,[4,4],[25,13]]]],
        ["inline","input",[],["value",["subexpr","@mut",[["get","fieldToAddName",["loc",[null,[29,26],[29,40]]]]],[],[]],"class","form-control","placeholder","field name"],["loc",[null,[29,12],[29,88]]]],
        ["inline","input",[],["value",["subexpr","@mut",[["get","fieldToAddValue",["loc",[null,[32,26],[32,41]]]]],[],[]],"class","form-control","placeholder","value"],["loc",[null,[32,12],[32,84]]]],
        ["element","action",["addField",["get","model",["loc",[null,[34,40],[34,45]]]]],[],["loc",[null,[34,20],[34,47]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-set-elements', ['exports'], function (exports) {

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
              "column": 4
            },
            "end": {
              "line": 11,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-set-elements.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row set-element-row");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-12");
          var el3 = dom.createTextNode("\n              ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n                \n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("span");
          dom.setAttribute(el3,"class","set-element");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
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
          var element0 = dom.childAt(fragment, [1, 1]);
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(element0,1,1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          return morphs;
        },
        statements: [
          ["inline","button.set-element-remove",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[5,48],[5,53]]]]],[],[]],"item",["subexpr","@mut",[["get","element",["loc",[null,[5,59],[5,66]]]]],[],[]],"removeElement","removeElement"],["loc",[null,[5,14],[6,49]]]],
          ["content","element",["loc",[null,[8,42],[8,53]]]]
        ],
        locals: ["element"],
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
        "moduleName": "ember-riak-explorer/templates/components/object-contents-set-elements.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container set-contents-list");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row set-element-row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-12");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("button");
        dom.setAttribute(el4,"type","button");
        dom.setAttribute(el4,"class","btn btn-sm btn-primary");
        var el5 = dom.createTextNode("\n                ");
        dom.appendChild(el4, el5);
        var el5 = dom.createElement("span");
        dom.setAttribute(el5,"class","glyphicon glyphicon-plus");
        dom.setAttribute(el5,"aria-hidden","true");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n                Add element");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
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
        var element1 = dom.childAt(fragment, [0]);
        var element2 = dom.childAt(element1, [3, 1]);
        var element3 = dom.childAt(element2, [3]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element2,1,1);
        morphs[2] = dom.createElementMorph(element3);
        return morphs;
      },
      statements: [
        ["block","each",[["get","setElements",["loc",[null,[2,12],[2,23]]]]],[],0,null,["loc",[null,[2,4],[11,13]]]],
        ["inline","input",[],["value",["subexpr","@mut",[["get","elementToAdd",["loc",[null,[15,26],[15,38]]]]],[],[]],"class","form-control"],["loc",[null,[15,12],[15,61]]]],
        ["element","action",["addElement",["get","model",["loc",[null,[17,42],[17,47]]]]],[],["loc",[null,[17,20],[17,49]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-set', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 15,
              "column": 14
            },
            "end": {
              "line": 19,
              "column": 14
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-set.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                  ");
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
          ["inline","object-contents-set-elements",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[16,55],[16,60]]]]],[],[]],"setElements",["subexpr","@mut",[["get","model.contentsForDisplay",["loc",[null,[17,34],[17,58]]]]],[],[]],"removeElement","removeElement","addElement","addElement"],["loc",[null,[16,18],[18,77]]]]
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
        dom.setAttribute(el1,"class","riak-object");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","object-contents");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-header");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","object-contents-info");
        var el5 = dom.createTextNode("\n                Set\n                (");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode(")\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","object-contents-actions");
        var el5 = dom.createTextNode("\n              ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-body");
        var el4 = dom.createTextNode("\n          ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("form");
        dom.setAttribute(el4,"class","form-inline");
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("          ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-footer");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Page loaded:");
        dom.appendChild(el4, el5);
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
        morphs[0] = dom.createMorphAt(dom.childAt(element1, [1]),1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [3]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [3, 1]),1,1);
        morphs[3] = dom.createMorphAt(dom.childAt(element0, [5]),3,3);
        return morphs;
      },
      statements: [
        ["content","model.contentsForDisplay.length",["loc",[null,[6,17],[6,52]]]],
        ["inline","object-actions",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[9,37],[9,42]]]]],[],[]],"isEditing",["subexpr","@mut",[["get","isEditing",["loc",[null,[9,53],[9,62]]]]],[],[]],"deleteObject","deleteObject"],["loc",[null,[9,14],[10,47]]]],
        ["block","if",[["get","model.contentsForDisplay",["loc",[null,[15,20],[15,44]]]]],[],0,null,["loc",[null,[15,14],[19,21]]]],
        ["content","model.metadata.dateLoaded",["loc",[null,[23,42],[23,71]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents-sets-embedded', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 4,
              "column": 4
            },
            "end": {
              "line": 22,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents-sets-embedded.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row map-composite-field-name");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-12");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("button");
          dom.setAttribute(el3,"type","button");
          dom.setAttribute(el3,"class","btn btn-xs btn-danger");
          var el4 = dom.createTextNode("\n                  remove set\n                ");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n                  \n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("strong");
          var el4 = dom.createComment("");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n        ");
          dom.appendChild(el1, el2);
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n        ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("div");
          dom.setAttribute(el1,"class","row");
          var el2 = dom.createTextNode("\n            ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","col-md-12");
          var el3 = dom.createTextNode("\n                ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n            ");
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
          var element0 = dom.childAt(fragment, [1, 1]);
          var element1 = dom.childAt(element0, [1]);
          var morphs = new Array(3);
          morphs[0] = dom.createElementMorph(element1);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
          morphs[2] = dom.createMorphAt(dom.childAt(fragment, [3, 1]),1,1);
          return morphs;
        },
        statements: [
          ["element","action",["removeField",["get","model",["loc",[null,[8,45],[8,50]]]],["get","setField",["loc",[null,[8,51],[8,59]]]]],[],["loc",[null,[8,22],[8,61]]]],
          ["content","setField.fullName",["loc",[null,[12,24],[12,45]]]],
          ["inline","object-contents-set-elements",[],["model",["subexpr","@mut",[["get","setField",["loc",[null,[17,53],[17,61]]]]],[],[]],"setElements",["subexpr","@mut",[["get","setField.value",["loc",[null,[18,32],[18,46]]]]],[],[]],"removeElement","removeElement","addElement","addElement"],["loc",[null,[17,16],[19,75]]]]
        ],
        locals: ["setField"],
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
        "moduleName": "ember-riak-explorer/templates/components/object-contents-sets-embedded.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","container set-contents-list");
        var el2 = dom.createTextNode("\n    ");
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
        morphs[0] = dom.createMorphAt(dom.childAt(element2, [1]),1,1);
        morphs[1] = dom.createMorphAt(element2,3,3);
        return morphs;
      },
      statements: [
        ["content","model.setsList.length",["loc",[null,[2,14],[2,39]]]],
        ["block","each",[["get","model.setsList",["loc",[null,[4,12],[4,26]]]]],[],0,null,["loc",[null,[4,4],[22,13]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-contents', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 5,
              "column": 16
            },
            "end": {
              "line": 13,
              "column": 16
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("form");
          dom.setAttribute(el1,"class","form-inline");
          var el2 = dom.createTextNode("\n                        ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","form-group");
          var el3 = dom.createTextNode("\n                            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("label");
          dom.setAttribute(el3,"for","contentType");
          var el4 = dom.createTextNode("Content Type:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n                            ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n                        ");
          dom.appendChild(el2, el3);
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1]),3,3);
          return morphs;
        },
        statements: [
          ["inline","input",[],["value",["subexpr","@mut",[["get","model.metadata.contentType",["loc",[null,[9,42],[9,68]]]]],[],[]],"id","metadata.contentType","class","form-control"],["loc",[null,[9,28],[10,80]]]]
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
              "line": 13,
              "column": 16
            },
            "end": {
              "line": 15,
              "column": 16
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                    ");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
          return morphs;
        },
        statements: [
          ["content","model.metadata.contentType",["loc",[null,[14,23],[14,53]]]]
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
              "line": 23,
              "column": 12
            },
            "end": {
              "line": 33,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("                ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("form");
          var el2 = dom.createTextNode("\n                    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("div");
          dom.setAttribute(el2,"class","form-group");
          var el3 = dom.createTextNode("\n                    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createComment("");
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n                    ");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n                    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("button");
          dom.setAttribute(el2,"type","button");
          dom.setAttribute(el2,"class","btn btn-md btn-primary");
          var el3 = dom.createTextNode("\n                        Save");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n                ");
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
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),1,1);
          morphs[1] = dom.createElementMorph(element1);
          return morphs;
        },
        statements: [
          ["inline","textarea",[],["class","form-control","value",["subexpr","@mut",[["get","model.contents",["loc",[null,[26,58],[26,72]]]]],[],[]],"rows",20,"autofocus",true],["loc",[null,[26,20],[27,40]]]],
          ["element","action",["saveObject",["get","model",["loc",[null,[30,50],[30,55]]]]],[],["loc",[null,[30,28],[30,57]]]]
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
                "line": 36,
                "column": 16
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/object-contents.hbs"
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode("                ");
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
            morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),0,0);
            return morphs;
          },
          statements: [
            ["content","model.contentsForDisplay",["loc",[null,[35,22],[35,50]]]]
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
              "line": 33,
              "column": 12
            },
            "end": {
              "line": 37,
              "column": 12
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
          morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
          dom.insertBoundary(fragment, 0);
          dom.insertBoundary(fragment, null);
          return morphs;
        },
        statements: [
          ["block","if",[["get","model.contentsForDisplay",["loc",[null,[34,22],[34,46]]]]],[],0,null,["loc",[null,[34,16],[36,23]]]]
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
            "line": 45,
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
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","object-contents");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-header");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","object-contents-info");
        var el5 = dom.createTextNode("\n");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("div");
        dom.setAttribute(el4,"class","object-contents-actions");
        var el5 = dom.createTextNode("\n              ");
        dom.appendChild(el4, el5);
        var el5 = dom.createComment("");
        dom.appendChild(el4, el5);
        var el5 = dom.createTextNode("\n            ");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","object-contents-body");
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
        dom.setAttribute(el3,"class","object-contents-footer");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Page loaded:");
        dom.appendChild(el4, el5);
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
        morphs[0] = dom.createMorphAt(dom.childAt(element3, [1]),1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element3, [3]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element2, [3]),1,1);
        morphs[3] = dom.createMorphAt(dom.childAt(element2, [5]),3,3);
        return morphs;
      },
      statements: [
        ["block","if",[["get","isEditing",["loc",[null,[5,22],[5,31]]]]],[],0,1,["loc",[null,[5,16],[15,23]]]],
        ["inline","object-actions",[],["model",["subexpr","@mut",[["get","model",["loc",[null,[18,37],[18,42]]]]],[],[]],"isEditing",["subexpr","@mut",[["get","isEditing",["loc",[null,[18,53],[18,62]]]]],[],[]],"deleteObject","deleteObject"],["loc",[null,[18,14],[19,47]]]],
        ["block","if",[["get","isEditing",["loc",[null,[23,18],[23,27]]]]],[],2,3,["loc",[null,[23,12],[37,19]]]],
        ["content","model.metadata.dateLoaded",["loc",[null,[40,42],[40,71]]]]
      ],
      locals: [],
      templates: [child0, child1, child2, child3]
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
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        /\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"style","display: inline-block;");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        /\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"style","display: inline-block;");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createComment("");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        /\n    ");
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
        var el5 = dom.createTextNode("\n                key:\n                ");
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
        var el1 = dom.createTextNode("\n");
        dom.appendChild(el0, el1);
        return el0;
      },
      buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
        var element0 = dom.childAt(fragment, [0]);
        var element1 = dom.childAt(element0, [1]);
        var morphs = new Array(4);
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [3]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element1, [5]),1,1);
        morphs[3] = dom.createMorphAt(dom.childAt(element0, [3, 1, 1, 1]),0,0);
        return morphs;
      },
      statements: [
        ["inline","link.link-cluster",[],["cluster",["subexpr","@mut",[["get","object.cluster",["loc",[null,[3,36],[3,50]]]]],[],[]]],["loc",[null,[3,8],[3,52]]]],
        ["inline","link.bucket-type",[],["bucketType",["subexpr","@mut",[["get","object.bucketType",["loc",[null,[6,42],[6,59]]]]],[],[]]],["loc",[null,[6,12],[6,61]]]],
        ["inline","link.link-bucket",[],["bucket",["subexpr","@mut",[["get","object.bucket",["loc",[null,[10,38],[10,51]]]]],[],[]]],["loc",[null,[10,12],[10,53]]]],
        ["content","object.key",["loc",[null,[19,22],[19,36]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/object-metadata', ['exports'], function (exports) {

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
                "column": 16
              },
              "end": {
                "line": 6,
                "column": 47
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
              "column": 12
            },
            "end": {
              "line": 7,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
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
          ["block","object-headers-edit",[],["headers",["subexpr","@mut",[["get","metadata.headersIndexes",["loc",[null,[5,47],[5,70]]]]],[],[]],"title","Secondary Indexes"],0,null,["loc",[null,[5,16],[6,71]]]]
        ],
        locals: [],
        templates: [child0]
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
            "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
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
              "line": 7,
              "column": 12
            },
            "end": {
              "line": 10,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
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
          ["block","object-headers",[],["headers",["subexpr","@mut",[["get","metadata.headersIndexes",["loc",[null,[8,42],[8,65]]]]],[],[]],"title","Secondary Indexes"],0,null,["loc",[null,[8,16],[9,66]]]]
        ],
        locals: [],
        templates: [child0]
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
                "line": 16,
                "column": 16
              },
              "end": {
                "line": 17,
                "column": 44
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
              "line": 15,
              "column": 12
            },
            "end": {
              "line": 18,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
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
          ["block","object-headers-edit",[],["headers",["subexpr","@mut",[["get","metadata.headersCustom",["loc",[null,[16,47],[16,69]]]]],[],[]],"title","Custom Headers"],0,null,["loc",[null,[16,16],[17,68]]]]
        ],
        locals: [],
        templates: [child0]
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
                "line": 19,
                "column": 16
              },
              "end": {
                "line": 20,
                "column": 44
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
              "line": 18,
              "column": 12
            },
            "end": {
              "line": 21,
              "column": 12
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/object-metadata.hbs"
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
          ["block","object-headers",[],["headers",["subexpr","@mut",[["get","metadata.headersCustom",["loc",[null,[19,42],[19,64]]]]],[],[]],"title","Custom Headers"],0,null,["loc",[null,[19,16],[20,63]]]]
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
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-10");
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
        dom.setAttribute(el3,"class","col-md-10");
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
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 1]),1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 1]),1,1);
        return morphs;
      },
      statements: [
        ["block","if",[["get","isEditing",["loc",[null,[4,18],[4,27]]]]],[],0,1,["loc",[null,[4,12],[10,19]]]],
        ["block","if",[["get","isEditing",["loc",[null,[15,18],[15,27]]]]],[],2,3,["loc",[null,[15,12],[21,19]]]]
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
        dom.setAttribute(el1,"class","container");
        var el2 = dom.createTextNode("\n    ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-2");
        dom.setAttribute(el3,"style","text-align: right;");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Last Modified:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-10");
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
        dom.setAttribute(el3,"class","col-md-2");
        dom.setAttribute(el3,"style","text-align: right;");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Etag:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-10");
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
        dom.setAttribute(el3,"class","col-md-2");
        dom.setAttribute(el3,"style","text-align: right;");
        var el4 = dom.createTextNode("\n            ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("strong");
        var el5 = dom.createTextNode("Causal Context:");
        dom.appendChild(el4, el5);
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n        ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n        ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("div");
        dom.setAttribute(el3,"class","col-md-10");
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
        var element0 = dom.childAt(fragment, [0]);
        var morphs = new Array(3);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]),1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element0, [5, 3]),1,1);
        return morphs;
      },
      statements: [
        ["content","object.metadata.dateLastModified",["loc",[null,[7,12],[7,48]]]],
        ["content","object.metadata.etag",["loc",[null,[15,12],[15,36]]]],
        ["content","object.metadata.causalContext",["loc",[null,[23,12],[23,45]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/components/pagination-component', ['exports'], function (exports) {

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
                "column": 4
              },
              "end": {
                "line": 11,
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
            dom.setAttribute(el1,"class","pagination-link number-link");
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
            morphs[1] = dom.createMorphAt(element0,0,0);
            return morphs;
          },
          statements: [
            ["element","action",["numberLinkClick",["get","link",["loc",[null,[10,73],[10,77]]]]],[],["loc",[null,[10,46],[10,79]]]],
            ["content","link",["loc",[null,[10,80],[10,88]]]]
          ],
          locals: ["link"],
          templates: []
        };
      }());
      return {
        meta: {
          "revision": "Ember@1.13.5",
          "loc": {
            "source": null,
            "start": {
              "line": 5,
              "column": 0
            },
            "end": {
              "line": 15,
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
          dom.setAttribute(el1,"class","pagination-links");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("li");
          dom.setAttribute(el2,"class","pagination-link text-link");
          var el3 = dom.createTextNode("Prev");
          dom.appendChild(el2, el3);
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n\n");
          dom.appendChild(el1, el2);
          var el2 = dom.createComment("");
          dom.appendChild(el1, el2);
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("li");
          dom.setAttribute(el2,"class","pagination-link text-link");
          var el3 = dom.createTextNode("Next");
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
          morphs[2] = dom.createMorphAt(element1,3,3);
          morphs[3] = dom.createAttrMorph(element3, 'disabled');
          morphs[4] = dom.createElementMorph(element3);
          return morphs;
        },
        statements: [
          ["attribute","disabled",["subexpr","if",[["get","shouldPrevBeDisabled",["loc",[null,[7,56],[7,76]]]],"disabled"],[],["loc",[null,[7,51],[7,89]]]]],
          ["element","action",["prevLinkClick"],[],["loc",[null,[7,90],[7,116]]]],
          ["block","each",[["get","numberLinks",["loc",[null,[9,12],[9,23]]]]],[],0,null,["loc",[null,[9,4],[11,13]]]],
          ["attribute","disabled",["subexpr","if",[["get","shouldNextBeDisabled",["loc",[null,[13,56],[13,76]]]],"disabled"],[],["loc",[null,[13,51],[13,89]]]]],
          ["element","action",["nextLinkClick"],[],["loc",[null,[13,90],[13,116]]]]
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
        "moduleName": "ember-riak-explorer/templates/components/pagination-component.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","pagination-content");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        morphs[1] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["content","yield",["loc",[null,[2,2],[2,11]]]],
        ["block","if",[["get","shouldShowPaginationLinks",["loc",[null,[5,6],[5,31]]]]],[],0,null,["loc",[null,[5,0],[15,7]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/results-panel', ['exports'], function (exports) {

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
        "moduleName": "ember-riak-explorer/templates/components/results-panel.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","results-panel");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","results-panel-inner");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1]),1,1);
        return morphs;
      },
      statements: [
        ["content","outlet",["loc",[null,[3,4],[3,14]]]]
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
              "line": 15,
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
          dom.setAttribute(el5,"class","col-md-6");
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
          ["inline","link.link-bucket",[],["bucket",["subexpr","@mut",[["get","bucket",["loc",[null,[9,46],[9,52]]]]],[],[]]],["loc",[null,[9,20],[9,54]]]]
        ],
        locals: ["bucket"],
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
              "line": 15,
              "column": 0
            },
            "end": {
              "line": 17,
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
        ["block","each",[["get","bucketList.buckets",["loc",[null,[3,8],[3,26]]]]],[],0,1,["loc",[null,[3,0],[17,9]]]]
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
              "line": 15,
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
          ["inline","link.link-object",[],["obj",["subexpr","@mut",[["get","obj",["loc",[null,[9,43],[9,46]]]]],[],[]]],["loc",[null,[9,20],[9,48]]]]
        ],
        locals: ["obj"],
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
              "line": 15,
              "column": 0
            },
            "end": {
              "line": 17,
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
        ["block","each",[["get","keys",["loc",[null,[3,8],[3,12]]]]],[],0,1,["loc",[null,[3,0],[17,9]]]]
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
define('ember-riak-explorer/templates/components/sidebar-panel', ['exports'], function (exports) {

  'use strict';

  exports['default'] = Ember.HTMLBars.template((function() {
    var child0 = (function() {
      var child0 = (function() {
        var child0 = (function() {
          return {
            meta: {
              "revision": "Ember@1.13.5",
              "loc": {
                "source": null,
                "start": {
                  "line": 11,
                  "column": 10
                },
                "end": {
                  "line": 13,
                  "column": 10
                }
              },
              "moduleName": "ember-riak-explorer/templates/components/sidebar-panel.hbs"
            },
            arity: 0,
            cachedFragment: null,
            hasRendered: false,
            buildFragment: function buildFragment(dom) {
              var el0 = dom.createDocumentFragment();
              var el1 = dom.createTextNode("            ");
              dom.appendChild(el0, el1);
              var el1 = dom.createElement("span");
              dom.setAttribute(el1,"class","dev-mode");
              var el2 = dom.createTextNode("Dev mode");
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
        return {
          meta: {
            "revision": "Ember@1.13.5",
            "loc": {
              "source": null,
              "start": {
                "line": 9,
                "column": 8
              },
              "end": {
                "line": 14,
                "column": 8
              }
            },
            "moduleName": "ember-riak-explorer/templates/components/sidebar-panel.hbs"
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
            var el1 = dom.createComment("");
            dom.appendChild(el0, el1);
            return el0;
          },
          buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
            var morphs = new Array(2);
            morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
            morphs[1] = dom.createMorphAt(fragment,3,3,contextualElement);
            dom.insertBoundary(fragment, null);
            return morphs;
          },
          statements: [
            ["content","cluster.id",["loc",[null,[10,10],[10,24]]]],
            ["block","if",[["get","cluster.developmentMode",["loc",[null,[11,16],[11,39]]]]],[],0,null,["loc",[null,[11,10],[13,17]]]]
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
              "line": 7,
              "column": 4
            },
            "end": {
              "line": 16,
              "column": 4
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/sidebar-panel.hbs"
        },
        arity: 1,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("      ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("li");
          dom.setAttribute(el1,"class","sidebar-group-item");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1]),1,1);
          return morphs;
        },
        statements: [
          ["block","link-to",["cluster",["get","cluster",["loc",[null,[9,29],[9,36]]]]],[],0,null,["loc",[null,[9,8],[14,20]]]]
        ],
        locals: ["cluster"],
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
            "line": 28,
            "column": 0
          }
        },
        "moduleName": "ember-riak-explorer/templates/components/sidebar-panel.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","sidebar-panel");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("ul");
        dom.setAttribute(el2,"class","sidebar-group");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("li");
        dom.setAttribute(el3,"class","sidebar-group-heading");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("span");
        dom.setAttribute(el4,"class","glyphicon glyphicon-th");
        dom.setAttribute(el4,"aria-hidden","true");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      Clusters\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n");
        dom.appendChild(el2, el3);
        var el3 = dom.createComment("");
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("  ");
        dom.appendChild(el2, el3);
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("ul");
        dom.setAttribute(el2,"class","sidebar-group");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("li");
        dom.setAttribute(el3,"class","sidebar-group-heading");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("span");
        dom.setAttribute(el4,"class","glyphicon glyphicon-book");
        dom.setAttribute(el4,"aria-hidden","true");
        dom.appendChild(el3, el4);
        var el4 = dom.createTextNode("\n      Resources\n    ");
        dom.appendChild(el3, el4);
        dom.appendChild(el2, el3);
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("li");
        dom.setAttribute(el3,"class","sidebar-group-item");
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
        var morphs = new Array(2);
        morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),3,3);
        morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]),1,1);
        return morphs;
      },
      statements: [
        ["block","each",[["get","clusters",["loc",[null,[7,12],[7,20]]]]],[],0,null,["loc",[null,[7,4],[16,13]]]],
        ["inline","link-to",["API Docs","explorer_api"],[],["loc",[null,[24,6],[24,43]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/topbar-panel', ['exports'], function (exports) {

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
              "column": 2
            },
            "end": {
              "line": 4,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/topbar-panel.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("    ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("img");
          dom.setAttribute(el1,"src","assets/images/sample_logo.png");
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
              "line": 9,
              "column": 8
            },
            "end": {
              "line": 12,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/topbar-panel.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","glyphicon glyphicon-tasks");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n          Some Item\n");
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
              "line": 16,
              "column": 8
            },
            "end": {
              "line": 19,
              "column": 8
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/topbar-panel.hbs"
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode("          ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("span");
          dom.setAttribute(el1,"class","glyphicon glyphicon-user");
          dom.setAttribute(el1,"aria-hidden","true");
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode("\n          Authz User\n");
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
        "moduleName": "ember-riak-explorer/templates/components/topbar-panel.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("nav");
        dom.setAttribute(el1,"class","topbar");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("nav");
        dom.setAttribute(el2,"class","topbar-nav");
        var el3 = dom.createTextNode("\n    ");
        dom.appendChild(el2, el3);
        var el3 = dom.createElement("ul");
        dom.setAttribute(el3,"class","topbar-nav-group");
        var el4 = dom.createTextNode("\n      ");
        dom.appendChild(el3, el4);
        var el4 = dom.createElement("li");
        dom.setAttribute(el4,"class","topbar-nav-item");
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
        dom.setAttribute(el4,"class","topbar-nav-item");
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
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(dom.childAt(element1, [1]),1,1);
        morphs[2] = dom.createMorphAt(dom.childAt(element1, [3]),1,1);
        return morphs;
      },
      statements: [
        ["block","link-to",["application"],["class","logo"],0,null,["loc",[null,[2,2],[4,14]]]],
        ["block","link-to",["application"],[],1,null,["loc",[null,[9,8],[12,20]]]],
        ["block","link-to",["application"],[],2,null,["loc",[null,[16,8],[19,20]]]]
      ],
      locals: [],
      templates: [child0, child1, child2]
    };
  }()));

});
define('ember-riak-explorer/templates/components/view-label', ['exports'], function (exports) {

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
              "column": 2
            },
            "end": {
              "line": 4,
              "column": 2
            }
          },
          "moduleName": "ember-riak-explorer/templates/components/view-label.hbs"
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
          var el1 = dom.createTextNode(":\n");
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment,1,1,contextualElement);
          return morphs;
        },
        statements: [
          ["content","pre-label",["loc",[null,[3,4],[3,17]]]]
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
        "moduleName": "ember-riak-explorer/templates/components/view-label.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","view-label");
        var el2 = dom.createTextNode("\n");
        dom.appendChild(el1, el2);
        var el2 = dom.createComment("");
        dom.appendChild(el1, el2);
        var el2 = dom.createTextNode("  ");
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
        morphs[0] = dom.createMorphAt(element0,1,1);
        morphs[1] = dom.createMorphAt(element0,3,3);
        return morphs;
      },
      statements: [
        ["block","if",[["get","pre-label",["loc",[null,[2,8],[2,17]]]]],[],0,null,["loc",[null,[2,2],[4,9]]]],
        ["content","label",["loc",[null,[5,2],[5,11]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/components/wrapper-panel', ['exports'], function (exports) {

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
        "moduleName": "ember-riak-explorer/templates/components/wrapper-panel.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","wrapper-panel");
        var el2 = dom.createTextNode("\n  ");
        dom.appendChild(el1, el2);
        var el2 = dom.createElement("div");
        dom.setAttribute(el2,"class","row");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0, 1]),1,1);
        return morphs;
      },
      statements: [
        ["content","yield",["loc",[null,[3,4],[3,13]]]]
      ],
      locals: [],
      templates: []
    };
  }()));

});
define('ember-riak-explorer/templates/error/cluster-not-found', ['exports'], function (exports) {

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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","dashboard-module",[],["label","404 Cluster Not Found"],0,null,["loc",[null,[1,0],[3,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/error/object-not-found', ['exports'], function (exports) {

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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","dashboard-module",[],["label","404 Object Not Found"],0,null,["loc",[null,[1,0],[3,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/error/service-not-found', ['exports'], function (exports) {

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
          dom.setAttribute(el2,"href","https://github.com/basho-labs/riak_explorer");
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","dashboard-module",[],["label","503 Service Unavailable"],0,null,["loc",[null,[1,0],[7,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/error/unknown', ['exports'], function (exports) {

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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","dashboard-module",[],["label","An unknown error has occurred"],0,null,["loc",[null,[1,0],[3,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/explorer-api', ['exports'], function (exports) {

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
            dom.setAttribute(el1,"class","label label-success");
            var el2 = dom.createTextNode("service available");
            dom.appendChild(el1, el2);
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
            ["content","model.pingResult.ping.message",["loc",[null,[16,10],[16,43]]]]
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
            dom.setAttribute(el1,"class","label label-default");
            var el2 = dom.createTextNode("service unavailable");
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
          dom.setAttribute(el1,"class","key-value-table");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tr");
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3,"class","key");
          var el4 = dom.createTextNode("Name:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3,"class","value");
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
          dom.setAttribute(el3,"class","key");
          var el4 = dom.createTextNode("Ping:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3,"class","value");
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
          dom.setAttribute(el3,"class","key");
          var el4 = dom.createTextNode("Development Mode:");
          dom.appendChild(el3, el4);
          dom.appendChild(el2, el3);
          var el3 = dom.createTextNode("\n      ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("td");
          dom.setAttribute(el3,"class","value");
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
          morphs[0] = dom.createMorphAt(dom.childAt(element1, [1, 3]),0,0);
          morphs[1] = dom.createMorphAt(dom.childAt(element1, [3, 3]),1,1);
          morphs[2] = dom.createMorphAt(dom.childAt(element1, [5, 3]),1,1);
          return morphs;
        },
        statements: [
          ["content","model.service",["loc",[null,[10,24],[10,41]]]],
          ["block","if",[["get","model.pingResult.ping",["loc",[null,[15,14],[15,35]]]]],[],0,1,["loc",[null,[15,8],[20,15]]]],
          ["block","if",[["get","model.propsResult.props.development_mode",["loc",[null,[26,14],[26,54]]]]],[],2,3,["loc",[null,[26,8],[30,15]]]]
        ],
        locals: [],
        templates: [child0, child1, child2, child3]
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
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),1,1);
            return morphs;
          },
          statements: [
            ["content","route.links.self",["loc",[null,[47,12],[47,32]]]],
            ["content","route.resources",["loc",[null,[48,13],[48,32]]]]
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
          dom.setAttribute(el1,"class","table table-hover");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 3]),1,1);
          return morphs;
        },
        statements: [
          ["block","each",[["get","model.routes",["loc",[null,[45,12],[45,24]]]]],[],0,null,["loc",[null,[45,4],[50,13]]]]
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
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element2,1,1);
        morphs[1] = dom.createMorphAt(element2,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        morphs[3] = dom.createMorphAt(fragment,4,4,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["pageTitle","explorer api"],["loc",[null,[2,2],[2,51]]]],
        ["inline","view-label",[],["label","Riak Explorer API"],["loc",[null,[3,2],[3,42]]]],
        ["block","dashboard-module",[],["label","Service Information"],0,null,["loc",[null,[6,0],[34,21]]]],
        ["block","dashboard-module",[],["label","Routes"],1,null,["loc",[null,[36,0],[53,21]]]]
      ],
      locals: [],
      templates: [child0, child1]
    };
  }()));

});
define('ember-riak-explorer/templates/index', ['exports'], function (exports) {

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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [3]),1,1);
          return morphs;
        },
        statements: [
          ["inline","link-to",["Explorer API","explorer_api"],[],["loc",[null,[3,14],[3,55]]]]
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
        morphs[0] = dom.createMorphAt(fragment,0,0,contextualElement);
        dom.insertBoundary(fragment, 0);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["block","dashboard-module",[],["label","Welcome to Riak Explorer"],0,null,["loc",[null,[1,0],[4,21]]]]
      ],
      locals: [],
      templates: [child0]
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
        dom.setAttribute(el1,"class","loading-container");
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
        morphs[0] = dom.createMorphAt(dom.childAt(fragment, [0]),1,1);
        return morphs;
      },
      statements: [
        ["content","loading-spinner",["loc",[null,[2,2],[2,21]]]]
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
      var child0 = (function() {
        return {
          meta: {
            "revision": "Ember@1.13.5",
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
            "moduleName": "ember-riak-explorer/templates/node-stats.hbs"
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
            dom.setAttribute(el2,"class","key");
            var el3 = dom.createComment("");
            dom.appendChild(el2, el3);
            dom.appendChild(el1, el2);
            var el2 = dom.createTextNode("\n        ");
            dom.appendChild(el1, el2);
            var el2 = dom.createElement("td");
            dom.setAttribute(el2,"class","value");
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
            morphs[0] = dom.createMorphAt(dom.childAt(element0, [1]),0,0);
            morphs[1] = dom.createMorphAt(dom.childAt(element0, [3]),0,0);
            return morphs;
          },
          statements: [
            ["content","stat.key",["loc",[null,[15,24],[15,36]]]],
            ["content","stat.value",["loc",[null,[16,26],[16,40]]]]
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
              "line": 10,
              "column": 0
            },
            "end": {
              "line": 21,
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
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1,"class","key-value-table");
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
          morphs[0] = dom.createMorphAt(dom.childAt(fragment, [1, 1]),1,1);
          return morphs;
        },
        statements: [
          ["block","each",[["get","model.stats",["loc",[null,[13,12],[13,23]]]]],[],0,null,["loc",[null,[13,4],[18,13]]]]
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
            "line": 22,
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
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element1,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","clusterId",["loc",[null,[3,12],[3,21]]]]],[],[]],"pageTitle",["subexpr","@mut",[["get","model.node",["loc",[null,[4,12],[4,22]]]]],[],[]]],["loc",[null,[2,2],[4,24]]]],
        ["inline","view-label",[],["pre-label","Node Stats","label",["subexpr","@mut",[["get","model.node",["loc",[null,[7,8],[7,18]]]]],[],[]]],["loc",[null,[5,2],[7,20]]]],
        ["block","dashboard-module",[],[],0,null,["loc",[null,[10,0],[21,21]]]]
      ],
      locals: [],
      templates: [child0]
    };
  }()));

});
define('ember-riak-explorer/templates/riak-ping', ['exports'], function (exports) {

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
              "line": 23,
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
          var el1 = dom.createTextNode("  ");
          dom.appendChild(el0, el1);
          var el1 = dom.createElement("table");
          dom.setAttribute(el1,"class","key-value-table");
          var el2 = dom.createTextNode("\n    ");
          dom.appendChild(el1, el2);
          var el2 = dom.createElement("tbody");
          var el3 = dom.createTextNode("\n    ");
          dom.appendChild(el2, el3);
          var el3 = dom.createElement("tr");
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4,"class","key");
          var el5 = dom.createTextNode("Node id:");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4,"class","value");
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
          dom.setAttribute(el4,"class","key");
          var el5 = dom.createTextNode("Riak Ping:");
          dom.appendChild(el4, el5);
          dom.appendChild(el3, el4);
          var el4 = dom.createTextNode("\n      ");
          dom.appendChild(el3, el4);
          var el4 = dom.createElement("td");
          dom.setAttribute(el4,"class","value");
          var el5 = dom.createComment("");
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
          var morphs = new Array(2);
          morphs[0] = dom.createMorphAt(dom.childAt(element0, [1, 3]),0,0);
          morphs[1] = dom.createMorphAt(dom.childAt(element0, [3, 3]),0,0);
          return morphs;
        },
        statements: [
          ["content","nodeId",["loc",[null,[15,24],[15,34]]]],
          ["content","model.message",["loc",[null,[19,24],[19,41]]]]
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
        "moduleName": "ember-riak-explorer/templates/riak-ping.hbs"
      },
      arity: 0,
      cachedFragment: null,
      hasRendered: false,
      buildFragment: function buildFragment(dom) {
        var el0 = dom.createDocumentFragment();
        var el1 = dom.createElement("div");
        dom.setAttribute(el1,"class","view-header");
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
        morphs[0] = dom.createMorphAt(element1,1,1);
        morphs[1] = dom.createMorphAt(element1,3,3);
        morphs[2] = dom.createMorphAt(fragment,2,2,contextualElement);
        dom.insertBoundary(fragment, null);
        return morphs;
      },
      statements: [
        ["inline","breadcrumb-component",[],["clusterId",["subexpr","@mut",[["get","clusterId",["loc",[null,[3,12],[3,21]]]]],[],[]],"pageTitle",["subexpr","@mut",[["get","nodeId",["loc",[null,[4,12],[4,18]]]]],[],[]]],["loc",[null,[2,2],[4,20]]]],
        ["inline","view-label",[],["pre-label","Node Ping","label",["subexpr","@mut",[["get","nodeId",["loc",[null,[7,8],[7,14]]]]],[],[]]],["loc",[null,[5,2],[7,16]]]],
        ["block","dashboard-module",[],[],0,null,["loc",[null,[10,0],[23,21]]]]
      ],
      locals: [],
      templates: [child0]
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
define('ember-riak-explorer/tests/components/breadcrumb-component.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/breadcrumb-component.js should pass jshint', function() { 
    ok(true, 'components/breadcrumb-component.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/components/button/delete-object.jshint', function () {

  'use strict';

  module('JSHint - components/button');
  test('components/button/delete-object.js should pass jshint', function() { 
    ok(true, 'components/button/delete-object.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/button/edit-object.jshint', function () {

  'use strict';

  module('JSHint - components/button');
  test('components/button/edit-object.js should pass jshint', function() { 
    ok(true, 'components/button/edit-object.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/button/object-view-raw.jshint', function () {

  'use strict';

  module('JSHint - components/button');
  test('components/button/object-view-raw.js should pass jshint', function() { 
    ok(true, 'components/button/object-view-raw.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/button/refresh-buckets.jshint', function () {

  'use strict';

  module('JSHint - components/button');
  test('components/button/refresh-buckets.js should pass jshint', function() { 
    ok(true, 'components/button/refresh-buckets.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/button/refresh-keys.jshint', function () {

  'use strict';

  module('JSHint - components/button');
  test('components/button/refresh-keys.js should pass jshint', function() { 
    ok(true, 'components/button/refresh-keys.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/button/set-element-remove.jshint', function () {

  'use strict';

  module('JSHint - components/button');
  test('components/button/set-element-remove.js should pass jshint', function() { 
    ok(true, 'components/button/set-element-remove.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/dashboard-module.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/dashboard-module.js should pass jshint', function() { 
    ok(true, 'components/dashboard-module.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/link/bucket-type.jshint', function () {

  'use strict';

  module('JSHint - components/link');
  test('components/link/bucket-type.js should pass jshint', function() { 
    ok(true, 'components/link/bucket-type.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/link/link-bucket.jshint', function () {

  'use strict';

  module('JSHint - components/link');
  test('components/link/link-bucket.js should pass jshint', function() { 
    ok(true, 'components/link/link-bucket.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/link/link-cluster.jshint', function () {

  'use strict';

  module('JSHint - components/link');
  test('components/link/link-cluster.js should pass jshint', function() { 
    ok(true, 'components/link/link-cluster.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/link/link-object.jshint', function () {

  'use strict';

  module('JSHint - components/link');
  test('components/link/link-object.js should pass jshint', function() { 
    ok(true, 'components/link/link-object.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/components/object-actions.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-actions.js should pass jshint', function() { 
    ok(true, 'components/object-actions.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-counter.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-counter.js should pass jshint', function() { 
    ok(true, 'components/object-contents-counter.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-counters-embedded.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-counters-embedded.js should pass jshint', function() { 
    ok(true, 'components/object-contents-counters-embedded.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-flags.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-flags.js should pass jshint', function() { 
    ok(true, 'components/object-contents-flags.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-map.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-map.js should pass jshint', function() { 
    ok(true, 'components/object-contents-map.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-maps-embedded.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-maps-embedded.js should pass jshint', function() { 
    ok(true, 'components/object-contents-maps-embedded.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-registers.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-registers.js should pass jshint', function() { 
    ok(true, 'components/object-contents-registers.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-set-elements.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-set-elements.js should pass jshint', function() { 
    ok(true, 'components/object-contents-set-elements.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-set.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-set.js should pass jshint', function() { 
    ok(true, 'components/object-contents-set.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents-sets-embedded.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents-sets-embedded.js should pass jshint', function() { 
    ok(true, 'components/object-contents-sets-embedded.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-contents.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-contents.js should pass jshint', function() { 
    ok(true, 'components/object-contents.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/components/object-metadata.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-metadata.js should pass jshint', function() { 
    ok(true, 'components/object-metadata.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/object-version.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/object-version.js should pass jshint', function() { 
    ok(true, 'components/object-version.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/pagination-component.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/pagination-component.js should pass jshint', function() { 
    ok(true, 'components/pagination-component.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/results-panel.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/results-panel.js should pass jshint', function() { 
    ok(true, 'components/results-panel.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/components/sidebar-panel.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/sidebar-panel.js should pass jshint', function() { 
    ok(true, 'components/sidebar-panel.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/topbar-panel.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/topbar-panel.js should pass jshint', function() { 
    ok(true, 'components/topbar-panel.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/view-label.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/view-label.js should pass jshint', function() { 
    ok(true, 'components/view-label.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/components/wrapper-panel.jshint', function () {

  'use strict';

  module('JSHint - components');
  test('components/wrapper-panel.js should pass jshint', function() { 
    ok(true, 'components/wrapper-panel.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/integration/components/breadcrumb-component-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('breadcrumb-component', 'Integration | Component | breadcrumb component', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 24
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'breadcrumb-component', ['loc', [null, [1, 0], [1, 24]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'breadcrumb-component', [], [], 0, null, ['loc', [null, [2, 4], [4, 29]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/breadcrumb-component-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/breadcrumb-component-test.js should pass jshint', function() { 
    ok(true, 'integration/components/breadcrumb-component-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/dashboard-module-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('dashboard-module', 'Integration | Component | dashboard module', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 20
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'dashboard-module', ['loc', [null, [1, 0], [1, 20]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'dashboard-module', [], [], 0, null, ['loc', [null, [2, 4], [4, 25]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/dashboard-module-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/dashboard-module-test.js should pass jshint', function() { 
    ok(true, 'integration/components/dashboard-module-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/loading-spinner-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('loading-spinner', 'Integration | Component | loading spinner', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 19
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'loading-spinner', ['loc', [null, [1, 0], [1, 19]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'loading-spinner', [], [], 0, null, ['loc', [null, [2, 4], [4, 24]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/loading-spinner-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/loading-spinner-test.js should pass jshint', function() { 
    ok(true, 'integration/components/loading-spinner-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/object-contents-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('object-contents', 'Integration | Component | object contents', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 19
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'object-contents', ['loc', [null, [1, 0], [1, 19]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'object-contents', [], [], 0, null, ['loc', [null, [2, 4], [4, 24]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/object-contents-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/object-contents-test.js should pass jshint', function() { 
    ok(true, 'integration/components/object-contents-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/object-metadata-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('object-metadata', 'Integration | Component | object metadata', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 19
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'object-metadata', ['loc', [null, [1, 0], [1, 19]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'object-metadata', [], [], 0, null, ['loc', [null, [2, 4], [4, 24]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/object-metadata-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/object-metadata-test.js should pass jshint', function() { 
    ok(true, 'integration/components/object-metadata-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/pagination-component-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('pagination-component', 'Integration | Component | pagination component', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 24
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'pagination-component', ['loc', [null, [1, 0], [1, 24]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'pagination-component', [], [], 0, null, ['loc', [null, [2, 4], [4, 29]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/pagination-component-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/pagination-component-test.js should pass jshint', function() { 
    ok(true, 'integration/components/pagination-component-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/results-panel-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('results-panel', 'Integration | Component | results panel', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 17
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'results-panel', ['loc', [null, [1, 0], [1, 17]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'results-panel', [], [], 0, null, ['loc', [null, [2, 4], [4, 22]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/results-panel-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/results-panel-test.js should pass jshint', function() { 
    ok(true, 'integration/components/results-panel-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/sidebar-panel-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('sidebar-panel', 'Integration | Component | sidebar panel', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 17
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'sidebar-panel', ['loc', [null, [1, 0], [1, 17]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'sidebar-panel', [], [], 0, null, ['loc', [null, [2, 4], [4, 22]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/sidebar-panel-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/sidebar-panel-test.js should pass jshint', function() { 
    ok(true, 'integration/components/sidebar-panel-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/topbar-panel-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('topbar-panel', 'Integration | Component | topbar panel', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 16
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'topbar-panel', ['loc', [null, [1, 0], [1, 16]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'topbar-panel', [], [], 0, null, ['loc', [null, [2, 4], [4, 21]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/topbar-panel-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/topbar-panel-test.js should pass jshint', function() { 
    ok(true, 'integration/components/topbar-panel-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/view-label-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('view-label', 'Integration | Component | view label', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 14
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'view-label', ['loc', [null, [1, 0], [1, 14]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'view-label', [], [], 0, null, ['loc', [null, [2, 4], [4, 19]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/view-label-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/view-label-test.js should pass jshint', function() { 
    ok(true, 'integration/components/view-label-test.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/integration/components/wrapper-panel-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForComponent('wrapper-panel', 'Integration | Component | wrapper panel', {
    integration: true
  });

  ember_qunit.test('it renders', function (assert) {
    assert.expect(2);

    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 17
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createComment('');
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
        statements: [['content', 'wrapper-panel', ['loc', [null, [1, 0], [1, 17]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'revision': 'Ember@1.13.5',
            'loc': {
              'source': null,
              'start': {
                'line': 2,
                'column': 4
              },
              'end': {
                'line': 4,
                'column': 4
              }
            }
          },
          arity: 0,
          cachedFragment: null,
          hasRendered: false,
          buildFragment: function buildFragment(dom) {
            var el0 = dom.createDocumentFragment();
            var el1 = dom.createTextNode('      template block text\n');
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
          'revision': 'Ember@1.13.5',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 5,
              'column': 2
            }
          }
        },
        arity: 0,
        cachedFragment: null,
        hasRendered: false,
        buildFragment: function buildFragment(dom) {
          var el0 = dom.createDocumentFragment();
          var el1 = dom.createTextNode('\n');
          dom.appendChild(el0, el1);
          var el1 = dom.createComment('');
          dom.appendChild(el0, el1);
          var el1 = dom.createTextNode('  ');
          dom.appendChild(el0, el1);
          return el0;
        },
        buildRenderNodes: function buildRenderNodes(dom, fragment, contextualElement) {
          var morphs = new Array(1);
          morphs[0] = dom.createMorphAt(fragment, 1, 1, contextualElement);
          return morphs;
        },
        statements: [['block', 'wrapper-panel', [], [], 0, null, ['loc', [null, [2, 4], [4, 22]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });

});
define('ember-riak-explorer/tests/integration/components/wrapper-panel-test.jshint', function () {

  'use strict';

  module('JSHint - integration/components');
  test('integration/components/wrapper-panel-test.js should pass jshint', function() { 
    ok(true, 'integration/components/wrapper-panel-test.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/models/object-metadata.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/object-metadata.js should pass jshint', function() { 
    ok(true, 'models/object-metadata.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/models/route.jshint', function () {

  'use strict';

  module('JSHint - models');
  test('models/route.js should pass jshint', function() { 
    ok(true, 'models/route.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/pods/riak-object/counter/controller.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/counter');
  test('pods/riak-object/counter/controller.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/counter/controller.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/counter/model.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/counter');
  test('pods/riak-object/counter/model.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/counter/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/counter/route.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/counter');
  test('pods/riak-object/counter/route.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/counter/route.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/pods/riak-object/embedded-map/model.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/embedded-map');
  test('pods/riak-object/embedded-map/model.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/embedded-map/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/map/controller.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/map');
  test('pods/riak-object/map/controller.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/map/controller.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/map/model.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/map');
  test('pods/riak-object/map/model.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/map/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/map/route.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/map');
  test('pods/riak-object/map/route.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/map/route.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/map-field/model.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/map-field');
  test('pods/riak-object/map-field/model.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/map-field/model.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/pods/riak-object/set/controller.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/set');
  test('pods/riak-object/set/controller.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/set/controller.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/set/model.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/set');
  test('pods/riak-object/set/model.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/set/model.js should pass jshint.'); 
  });

});
define('ember-riak-explorer/tests/pods/riak-object/set/route.jshint', function () {

  'use strict';

  module('JSHint - pods/riak-object/set');
  test('pods/riak-object/set/route.js should pass jshint', function() { 
    ok(true, 'pods/riak-object/set/route.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/routes/error/service-not-found.jshint', function () {

  'use strict';

  module('JSHint - routes/error');
  test('routes/error/service-not-found.js should pass jshint', function() { 
    ok(true, 'routes/error/service-not-found.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/unit/models/object-metadata-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleForModel('object-metadata', 'Unit | Model | object metadata', {
    // Specify the other units that are required for this test.
    needs: []
  });

  ember_qunit.test('it exists', function (assert) {
    var model = this.subject();
    // var store = this.store();
    assert.ok(!!model);
  });

});
define('ember-riak-explorer/tests/unit/models/object-metadata-test.jshint', function () {

  'use strict';

  module('JSHint - unit/models');
  test('unit/models/object-metadata-test.js should pass jshint', function() { 
    ok(true, 'unit/models/object-metadata-test.js should pass jshint.'); 
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
define('ember-riak-explorer/tests/unit/routes/error/service-not-found-test', ['ember-qunit'], function (ember_qunit) {

  'use strict';

  ember_qunit.moduleFor('route:error/service-not-found', 'Unit | Route | error/service not found', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  ember_qunit.test('it exists', function (assert) {
    var route = this.subject();
    assert.ok(route);
  });

});
define('ember-riak-explorer/tests/unit/routes/error/service-not-found-test.jshint', function () {

  'use strict';

  module('JSHint - unit/routes/error');
  test('unit/routes/error/service-not-found-test.js should pass jshint', function() { 
    ok(true, 'unit/routes/error/service-not-found-test.js should pass jshint.'); 
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
  require("ember-riak-explorer/app")["default"].create({"name":"ember-riak-explorer","version":"0.0.0+053bcaaa"});
}

/* jshint ignore:end */
//# sourceMappingURL=ember-riak-explorer.map