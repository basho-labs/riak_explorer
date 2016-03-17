define('ember-riak-explorer/tests/adapters/application.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/application.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/application.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/bucket-list.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/bucket-list.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/bucket-list.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/bucket-type.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/bucket-type.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/bucket-type.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/bucket.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/bucket.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/bucket.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/cluster.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/cluster.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/cluster.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/config-file.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/config-file.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/config-file.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/log-file.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/log-file.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/log-file.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/node.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/node.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/node.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/object-list.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/object-list.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/object-list.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/riak-object.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/riak-object.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/riak-object.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/adapters/search-index.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - adapters/search-index.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'adapters/search-index.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/app.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - app.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'app.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/blanket-options', ['exports'], function (exports) {
  /* globals blanket, module */

  var options = {
    modulePrefix: 'ember-riak-explorer',
    filter: '//.*ember-riak-explorer/.*/',
    antifilter: '//.*(tests|template).*/',
    loaderExclusions: [],
    enableCoverage: true,
    cliOptions: {
      reporters: ['json'],
      autostart: true
    }
  };
  if (typeof exports === 'undefined') {
    blanket.options(options);
  } else {
    module.exports = options;
  }
});
define('ember-riak-explorer/tests/blanket-options.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - blanket-options.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'blanket-options.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/breadcrumb-component.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/breadcrumb-component.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/breadcrumb-component.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/button/delete-object.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/button/delete-object.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/button/delete-object.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/button/edit-object.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/button/edit-object.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/button/edit-object.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/button/object-view-raw.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/button/object-view-raw.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/button/object-view-raw.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/button/refresh-buckets.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/button/refresh-buckets.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/button/refresh-buckets.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/button/refresh-keys.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/button/refresh-keys.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/button/refresh-keys.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/button/set-element-remove.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/button/set-element-remove.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/button/set-element-remove.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/cluster-status-indicator.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/cluster-status-indicator.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/cluster-status-indicator.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/code-highlighter.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/code-highlighter.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/code-highlighter.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/dashboard-module/body.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/dashboard-module/body.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/dashboard-module/body.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/dashboard-module/container.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/dashboard-module/container.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/dashboard-module/container.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/dashboard-module/header.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/dashboard-module/header.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/dashboard-module/header.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/link/bucket-type.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/link/bucket-type.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/link/bucket-type.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/link/link-bucket.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/link/link-bucket.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/link/link-bucket.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/link/link-cluster.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/link/link-cluster.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/link/link-cluster.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/link/link-index.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/link/link-index.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/link/link-index.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/link/link-object.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/link/link-object.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/link/link-object.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/loading-spinner.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/loading-spinner.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/loading-spinner.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/map-object/contents.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/map-object/contents.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/map-object/contents.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/map-object/map.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/map-object/map.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/map-object/map.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/map-object/maps.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/map-object/maps.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/map-object/maps.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/map-object/section.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/map-object/section.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/map-object/section.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/map-object/sets.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/map-object/sets.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/map-object/sets.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/pagination-component.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/pagination-component.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/pagination-component.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/table/bucket-props-advanced.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/table/bucket-props-advanced.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/table/bucket-props-advanced.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/table/bucket-props-overview.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/table/bucket-props-overview.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/table/bucket-props-overview.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/table/bucket-types.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/table/bucket-types.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/table/bucket-types.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/table/object-headers.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/table/object-headers.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/table/object-headers.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/table/object-version.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/table/object-version.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/table/object-version.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/table/search-indexes.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/table/search-indexes.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/table/search-indexes.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/tooltip/bucket-props.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/tooltip/bucket-props.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/tooltip/bucket-props.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/tooltip/node-config.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/tooltip/node-config.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/tooltip/node-config.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/tooltip/node-stats.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/tooltip/node-stats.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/tooltip/node-stats.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/view-label.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/view-label.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/view-label.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/components/wrapper-panel.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - components/wrapper-panel.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'components/wrapper-panel.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/controllers/application.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - controllers/application.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'controllers/application.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/helpers/destroy-app', ['exports', 'ember'], function (exports, _ember) {
  exports['default'] = destroyApp;

  function destroyApp(application) {
    _ember['default'].run(application, 'destroy');
  }
});
define('ember-riak-explorer/tests/helpers/destroy-app.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - helpers/destroy-app.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'helpers/destroy-app.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/helpers/ember-power-select', ['exports', 'ember'], function (exports, _ember) {
  exports.triggerKeydown = triggerKeydown;
  exports.typeInSearch = typeInSearch;
  exports.clickTrigger = clickTrigger;

  // Helpers for integration tests

  function typeText(selector, text) {
    $(selector).val(text);
    $(selector).trigger('input');
  }

  function triggerKeydown(domElement, k) {
    var oEvent = document.createEvent("Events");
    oEvent.initEvent('keydown', true, true);
    $.extend(oEvent, {
      view: window,
      ctrlKey: false,
      altKey: false,
      shiftKey: false,
      metaKey: false,
      keyCode: k,
      charCode: k
    });
    _ember['default'].run(function () {
      domElement.dispatchEvent(oEvent);
    });
  }

  function typeInSearch(text) {
    _ember['default'].run(function () {
      typeText('.ember-power-select-search input, .ember-power-select-trigger-multiple-input', text);
    });
  }

  function clickTrigger(scope) {
    var selector = '.ember-power-select-trigger';
    if (scope) {
      selector = scope + ' ' + selector;
    }
    var event = new window.Event('mousedown', { bubbles: true, cancelable: true, view: window });
    _ember['default'].run(function () {
      return _ember['default'].$(selector)[0].dispatchEvent(event);
    });
  }

  // Helpers for acceptance tests

  exports['default'] = function () {
    var isEmberOne = _ember['default'].VERSION.match(/1\.13/);

    _ember['default'].Test.registerAsyncHelper('selectChoose', function (app, cssPath, value) {
      var uuid = find(cssPath).find('.ember-power-select-trigger').attr('class').match(/ember-power-select-trigger-(\d+)/)[1];
      // If the dropdown is closed, open it
      if (_ember['default'].$('.ember-power-select-dropdown-' + uuid).length === 0) {
        click(cssPath + ' .ember-power-select-trigger');
      }

      // Select the option with the given text
      click('.ember-power-select-dropdown-' + uuid + ' .ember-power-select-option:contains("' + value + '")');
    });

    _ember['default'].Test.registerAsyncHelper('selectSearch', function (app, cssPath, value) {
      var uuid = find(cssPath).find('.ember-power-select-trigger').attr('class').match(/ember-power-select-trigger-(\d+)/)[1];
      var isMultipleSelect = _ember['default'].$(cssPath + ' .ember-power-select-trigger-multiple-input').length > 0;

      var dropdownIsClosed = _ember['default'].$('.ember-power-select-dropdown-' + uuid).length === 0;
      if (dropdownIsClosed) {
        click(cssPath + ' .ember-power-select-trigger');
      }

      if (isMultipleSelect) {
        fillIn(cssPath + ' .ember-power-select-trigger-multiple-input', value);
        if (isEmberOne) {
          triggerEvent(cssPath + ' .ember-power-select-trigger-multiple-input', 'input');
        }
      } else {
        fillIn('.ember-power-select-search input', value);
        if (isEmberOne) {
          triggerEvent('.ember-power-select-dropdown-' + uuid + ' .ember-power-select-search input', 'input');
        }
      }
    });
  };
});
define('ember-riak-explorer/tests/helpers/module-for-acceptance', ['exports', 'qunit', 'ember-riak-explorer/tests/helpers/start-app', 'ember-riak-explorer/tests/helpers/destroy-app'], function (exports, _qunit, _emberRiakExplorerTestsHelpersStartApp, _emberRiakExplorerTestsHelpersDestroyApp) {
  exports['default'] = function (name) {
    var options = arguments.length <= 1 || arguments[1] === undefined ? {} : arguments[1];

    (0, _qunit.module)(name, {
      beforeEach: function beforeEach() {
        this.application = (0, _emberRiakExplorerTestsHelpersStartApp['default'])();

        if (options.beforeEach) {
          options.beforeEach.apply(this, arguments);
        }
      },

      afterEach: function afterEach() {
        (0, _emberRiakExplorerTestsHelpersDestroyApp['default'])(this.application);

        if (options.afterEach) {
          options.afterEach.apply(this, arguments);
        }
      }
    });
  };
});
define('ember-riak-explorer/tests/helpers/module-for-acceptance.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - helpers/module-for-acceptance.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'helpers/module-for-acceptance.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/helpers/object-length.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - helpers/object-length.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'helpers/object-length.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/helpers/resolver', ['exports', 'ember-riak-explorer/resolver', 'ember-riak-explorer/config/environment'], function (exports, _emberRiakExplorerResolver, _emberRiakExplorerConfigEnvironment) {

  var resolver = _emberRiakExplorerResolver['default'].create();

  resolver.namespace = {
    modulePrefix: _emberRiakExplorerConfigEnvironment['default'].modulePrefix,
    podModulePrefix: _emberRiakExplorerConfigEnvironment['default'].podModulePrefix
  };

  exports['default'] = resolver;
});
define('ember-riak-explorer/tests/helpers/resolver.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - helpers/resolver.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'helpers/resolver.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/helpers/start-app', ['exports', 'ember', 'ember-riak-explorer/app', 'ember-riak-explorer/config/environment'], function (exports, _ember, _emberRiakExplorerApp, _emberRiakExplorerConfigEnvironment) {
  exports['default'] = startApp;

  function startApp(attrs) {
    var application = undefined;

    var attributes = _ember['default'].merge({}, _emberRiakExplorerConfigEnvironment['default'].APP);
    attributes = _ember['default'].merge(attributes, attrs); // use defaults, but you can override;

    _ember['default'].run(function () {
      application = _emberRiakExplorerApp['default'].create(attributes);
      application.setupForTesting();
      application.injectTestHelpers();
    });

    return application;
  }
});
define('ember-riak-explorer/tests/helpers/start-app.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - helpers/start-app.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'helpers/start-app.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/initializers/explorer.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - initializers/explorer.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'initializers/explorer.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/integration/components/cluster-status-indicator-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleForComponent)('cluster-status-indicator', 'Integration | Component | cluster status indicator', {
    integration: true
  });

  (0, _emberQunit.test)('it renders', function (assert) {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });"

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'fragmentReason': {
            'name': 'missing-wrapper',
            'problems': ['wrong-type']
          },
          'revision': 'Ember@2.3.1',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 28
            }
          }
        },
        isEmpty: false,
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
        statements: [['content', 'cluster-status-indicator', ['loc', [null, [1, 0], [1, 28]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:"
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'fragmentReason': false,
            'revision': 'Ember@2.3.1',
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
          isEmpty: false,
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
          'fragmentReason': {
            'name': 'missing-wrapper',
            'problems': ['wrong-type']
          },
          'revision': 'Ember@2.3.1',
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
        isEmpty: false,
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
        statements: [['block', 'cluster-status-indicator', [], [], 0, null, ['loc', [null, [2, 4], [4, 33]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });
});
define('ember-riak-explorer/tests/integration/components/cluster-status-indicator-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - integration/components/cluster-status-indicator-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'integration/components/cluster-status-indicator-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/integration/components/tooltip/node-config-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleForComponent)('tooltip/node-config', 'Integration | Component | tooltip/node config', {
    integration: true
  });

  (0, _emberQunit.test)('it renders', function (assert) {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });"

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'fragmentReason': {
            'name': 'missing-wrapper',
            'problems': ['wrong-type']
          },
          'revision': 'Ember@2.3.1',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 23
            }
          }
        },
        isEmpty: false,
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
        statements: [['content', 'tooltip/node-config', ['loc', [null, [1, 0], [1, 23]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:"
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'fragmentReason': false,
            'revision': 'Ember@2.3.1',
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
          isEmpty: false,
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
          'fragmentReason': {
            'name': 'missing-wrapper',
            'problems': ['wrong-type']
          },
          'revision': 'Ember@2.3.1',
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
        isEmpty: false,
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
        statements: [['block', 'tooltip/node-config', [], [], 0, null, ['loc', [null, [2, 4], [4, 28]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });
});
define('ember-riak-explorer/tests/integration/components/tooltip/node-config-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - integration/components/tooltip/node-config-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'integration/components/tooltip/node-config-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/integration/components/tooltip/node-stats-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleForComponent)('tooltip/node-stats', 'Integration | Component | tooltip/node stats', {
    integration: true
  });

  (0, _emberQunit.test)('it renders', function (assert) {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });"

    this.render(Ember.HTMLBars.template((function () {
      return {
        meta: {
          'fragmentReason': {
            'name': 'missing-wrapper',
            'problems': ['wrong-type']
          },
          'revision': 'Ember@2.3.1',
          'loc': {
            'source': null,
            'start': {
              'line': 1,
              'column': 0
            },
            'end': {
              'line': 1,
              'column': 22
            }
          }
        },
        isEmpty: false,
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
        statements: [['content', 'tooltip/node-stats', ['loc', [null, [1, 0], [1, 22]]]]],
        locals: [],
        templates: []
      };
    })()));

    assert.equal(this.$().text().trim(), '');

    // Template block usage:"
    this.render(Ember.HTMLBars.template((function () {
      var child0 = (function () {
        return {
          meta: {
            'fragmentReason': false,
            'revision': 'Ember@2.3.1',
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
          isEmpty: false,
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
          'fragmentReason': {
            'name': 'missing-wrapper',
            'problems': ['wrong-type']
          },
          'revision': 'Ember@2.3.1',
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
        isEmpty: false,
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
        statements: [['block', 'tooltip/node-stats', [], [], 0, null, ['loc', [null, [2, 4], [4, 27]]]]],
        locals: [],
        templates: [child0]
      };
    })()));

    assert.equal(this.$().text().trim(), 'template block text');
  });
});
define('ember-riak-explorer/tests/integration/components/tooltip/node-stats-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - integration/components/tooltip/node-stats-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'integration/components/tooltip/node-stats-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/models/bucket-props.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/models/bucket-props.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/models/bucket-props.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/models/cached-list-watcher.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/models/cached-list-watcher.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/models/cached-list-watcher.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/models/cached-list.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/models/cached-list.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/models/cached-list.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/models/map-object.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/models/map-object.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/models/map-object.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/models/object-headers.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/models/object-headers.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/models/object-headers.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/routes/alerts.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/routes/alerts.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/routes/alerts.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/routes/loading-slider.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/routes/loading-slider.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/routes/loading-slider.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/routes/scroll-reset.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/routes/scroll-reset.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/routes/scroll-reset.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/mixins/routes/wrapper-state.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - mixins/routes/wrapper-state.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'mixins/routes/wrapper-state.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/models/bucket-list.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - models/bucket-list.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'models/bucket-list.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/models/object-list.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - models/object-list.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'models/object-list.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/bucket/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/bucket/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/bucket/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/bucket/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/bucket/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/bucket/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/bucket-type/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/bucket-type/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/bucket-type/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/bucket-type/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/bucket-type/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/bucket-type/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/data/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/data/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/data/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/data/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/data/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/data/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/ops/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/ops/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/ops/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/ops/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/ops/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/ops/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/query/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/query/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/query/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/query/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/query/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/query/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/cluster/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/cluster/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/cluster/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/config-file/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/config-file/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/config-file/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/config-file/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/config-file/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/config-file/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/error/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/error/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/error/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/error/service-not-found/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/error/service-not-found/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/error/service-not-found/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/help/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/help/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/help/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/index/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/index/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/index/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/log-file/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/log-file/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/log-file/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/log-file/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/log-file/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/log-file/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/node/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/node/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/node/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/node/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/node/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/node/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/riak-object/counter/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/riak-object/counter/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/riak-object/counter/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/riak-object/edit/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/riak-object/edit/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/riak-object/edit/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/riak-object/map/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/riak-object/map/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/riak-object/map/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/riak-object/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/riak-object/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/riak-object/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/riak-object/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/riak-object/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/riak-object/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/riak-object/set/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/riak-object/set/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/riak-object/set/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-index/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-index/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-index/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-index/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-index/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-index/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-schema/create/controller.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-schema/create/controller.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-schema/create/controller.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-schema/create/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-schema/create/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-schema/create/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-schema/edit/controller.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-schema/edit/controller.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-schema/edit/controller.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-schema/edit/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-schema/edit/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-schema/edit/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-schema/model.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-schema/model.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-schema/model.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/pods/search-schema/route.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - pods/search-schema/route.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'pods/search-schema/route.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/resolver.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - resolver.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'resolver.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/router.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - router.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'router.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/routes/application.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - routes/application.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'routes/application.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/serializers/application.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - serializers/application.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'serializers/application.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/serializers/bucket-type.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - serializers/bucket-type.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'serializers/bucket-type.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/serializers/cluster.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - serializers/cluster.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'serializers/cluster.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/serializers/config-file.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - serializers/config-file.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'serializers/config-file.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/serializers/log-file.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - serializers/log-file.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'serializers/log-file.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/serializers/node.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - serializers/node.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'serializers/node.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/serializers/search-index.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - serializers/search-index.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'serializers/search-index.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/services/explorer.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - services/explorer.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'services/explorer.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/test-helper', ['exports', 'ember-riak-explorer/tests/helpers/resolver', 'ember-qunit'], function (exports, _emberRiakExplorerTestsHelpersResolver, _emberQunit) {

  /**
  * Basic helper function to mark tests as pending.
  * NOTE: This still marks the test as passed. It does style the output in the browser.
  *       No phantomJS support as of right now.
  *
  * @method pending
  */
  _emberQunit['default'].pending = function () {
    _emberQunit['default'].test(arguments[0] + ' (PENDING TEST)', function (assert) {
      assert.ok(!0); //dont expect any tests

      $('.running').css('background', '#FFFF99');
    });
  };

  _emberQunit['default'].setResolver(_emberRiakExplorerTestsHelpersResolver['default']);
});
define('ember-riak-explorer/tests/test-helper.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - test-helper.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'test-helper.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/bucket-list-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:bucket-list', 'Unit | Adapter | bucket list', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/bucket-list-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/bucket-list-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/bucket-list-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/bucket-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:bucket', 'Unit | Adapter | bucket', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/bucket-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/bucket-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/bucket-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/bucket-type-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:bucket-type', 'Unit | Adapter | bucket type', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/bucket-type-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/bucket-type-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/bucket-type-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/cluster-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:cluster', 'Unit | Adapter | cluster', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/cluster-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/cluster-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/cluster-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/config-file-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:config-file', 'Unit | Adapter | config file', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/config-file-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/config-file-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/config-file-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/log-file-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:log-file', 'Unit | Adapter | log file', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/log-file-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/log-file-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/log-file-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/riak-node-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:node', 'Unit | Adapter | node', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/riak-node-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/riak-node-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/riak-node-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/riak-object-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:riak-object', 'Unit | Adapter | riak object', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/riak-object-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/riak-object-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/riak-object-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/adapters/search-index-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('adapter:search-index', 'Unit | Adapter | search index', {
    // Specify the other units that are required for this test.
    // needs: ['serializer:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var adapter = this.subject();
    assert.ok(adapter);
  });
});
define('ember-riak-explorer/tests/unit/adapters/search-index-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/adapters/search-index-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/adapters/search-index-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/controllers/application-test', ['exports', 'ember-qunit'], function (exports, _emberQunit) {

  (0, _emberQunit.moduleFor)('controller:application', {
    // Specify the other units that are required for this test.
    // needs: ['controller:foo']
  });

  // Replace this with your real tests.
  (0, _emberQunit.test)('it exists', function (assert) {
    var controller = this.subject();
    assert.ok(controller);
  });
});
define('ember-riak-explorer/tests/unit/controllers/application-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/controllers/application-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/controllers/application-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/helpers/object-length-test', ['exports', 'ember-riak-explorer/helpers/object-length', 'qunit'], function (exports, _emberRiakExplorerHelpersObjectLength, _qunit) {

  (0, _qunit.module)('Unit | Helper | object length');

  // Replace this with your real tests.
  (0, _qunit.test)('it works', function (assert) {
    var objectWithLength = {
      foo: "bar",
      haz: "cheezburger"
    };

    var objectWithOutLength = {};

    var result1 = (0, _emberRiakExplorerHelpersObjectLength.objectLength)([objectWithLength]);
    var result2 = (0, _emberRiakExplorerHelpersObjectLength.objectLength)([objectWithOutLength]);

    assert.equal(result1, 2);
    assert.equal(result2, 0);
  });
});
define('ember-riak-explorer/tests/unit/helpers/object-length-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/helpers/object-length-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/helpers/object-length-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/mixins/routes/loading-slider-test', ['exports', 'ember', 'ember-riak-explorer/mixins/routes/loading-slider', 'qunit'], function (exports, _ember, _emberRiakExplorerMixinsRoutesLoadingSlider, _qunit) {

  (0, _qunit.module)('Unit | Mixin | routes/loading slider');

  // Replace this with your real tests.
  (0, _qunit.test)('it works', function (assert) {
    var RoutesLoadingSliderObject = _ember['default'].Object.extend(_emberRiakExplorerMixinsRoutesLoadingSlider['default']);
    var subject = RoutesLoadingSliderObject.create();
    assert.ok(subject);
  });
});
define('ember-riak-explorer/tests/unit/mixins/routes/loading-slider-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/mixins/routes/loading-slider-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/mixins/routes/loading-slider-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/mixins/routes/scroll-reset-test', ['exports', 'ember', 'ember-riak-explorer/mixins/routes/scroll-reset', 'qunit'], function (exports, _ember, _emberRiakExplorerMixinsRoutesScrollReset, _qunit) {

  (0, _qunit.module)('Unit | Mixin | routes/scroll reset');

  // Replace this with your real tests.
  (0, _qunit.test)('it works', function (assert) {
    var RoutesScrollResetObject = _ember['default'].Object.extend(_emberRiakExplorerMixinsRoutesScrollReset['default']);
    var subject = RoutesScrollResetObject.create();
    assert.ok(subject);
  });
});
define('ember-riak-explorer/tests/unit/mixins/routes/scroll-reset-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/mixins/routes/scroll-reset-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/mixins/routes/scroll-reset-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/bucket-list-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('bucket-list', 'Unit | Model | bucket list', {
    needs: ['model:bucketType']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('bucket type relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('bucketType');

    assert.equal(relationship.key, 'bucketType');

    assert.equal(relationship.kind, 'belongsTo');
  });
});
define('ember-riak-explorer/tests/unit/models/bucket-list-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/bucket-list-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/bucket-list-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/bucket-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('bucket', 'Unit | Model | bucket', {
    needs: ['model:objectList', 'model:bucketType', 'model:riakObject']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('object list relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('objectList');

    assert.equal(relationship.key, 'objectList');

    assert.equal(relationship.kind, 'belongsTo');
  });

  (0, _emberQunit.test)('bucket type relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('bucketType');

    assert.equal(relationship.key, 'bucketType');

    assert.equal(relationship.kind, 'belongsTo');
  });

  (0, _emberQunit.test)('objects relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('objects');

    assert.equal(relationship.key, 'objects');

    assert.equal(relationship.kind, 'hasMany');
  });
});
define('ember-riak-explorer/tests/unit/models/bucket-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/bucket-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/bucket-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/bucket-type-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('bucket-type', 'Unit | Model | bucket type', {
    needs: ['model:cluster', 'model:bucketList', 'model:bucket']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('cluster relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('cluster');

    assert.equal(relationship.key, 'cluster');

    assert.equal(relationship.kind, 'belongsTo');
  });

  (0, _emberQunit.test)('bucket lists relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('bucketList');

    assert.equal(relationship.key, 'bucketList');

    assert.equal(relationship.kind, 'belongsTo');
  });

  (0, _emberQunit.test)('buckets relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('buckets');

    assert.equal(relationship.key, 'buckets');
    assert.equal(relationship.kind, 'hasMany');
  });
});
define('ember-riak-explorer/tests/unit/models/bucket-type-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/bucket-type-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/bucket-type-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/cluster-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('cluster', 'Unit | Model | cluster', {
    needs: ['model:bucketType', 'model:node', 'model:searchIndex', 'model:searchSchema', 'model:config-file', 'model:log-file']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('bucketTypes relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('bucketTypes');

    assert.equal(relationship.key, 'bucketTypes');
    assert.equal(relationship.kind, 'hasMany');
  });

  (0, _emberQunit.test)('nodes relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('nodes');

    assert.equal(relationship.key, 'nodes');
    assert.equal(relationship.kind, 'hasMany');
  });

  (0, _emberQunit.test)('searchIndexes relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('searchIndexes');

    assert.equal(relationship.key, 'searchIndexes');
    assert.equal(relationship.kind, 'hasMany');
  });

  (0, _emberQunit.test)('searchSchemas relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('searchSchemas');

    assert.equal(relationship.key, 'searchSchemas');
    assert.equal(relationship.kind, 'hasMany');
  });

  (0, _emberQunit.pending)('getting active bucket types', function () {});

  (0, _emberQunit.pending)('getting inactive bucket types', function () {});

  (0, _emberQunit.pending)('determining production mode', function () {});

  (0, _emberQunit.test)('status', function (assert) {
    var model = this.subject();
    var store = this.store();

    _ember['default'].run(function () {
      // No nodes should return down
      assert.equal(model.get('status'), 'down');

      // Create some mock nodes
      var node1 = store.createRecord('node', { name: 'node1', cluster: model });
      var node2 = store.createRecord('node', { name: 'node2', cluster: model });
      var node3 = store.createRecord('node', { name: 'node3', cluster: model });

      node1.set('available', true);
      node1.set('status', 'valid');
      node2.set('available', true);
      node2.set('status', 'valid');
      node3.set('available', true);
      node3.set('status', 'valid');
      assert.equal(model.get('status'), 'ok');

      node1.set('available', false);
      node1.set('status', 'valid');
      node2.set('available', false);
      node2.set('status', 'invalid');
      node3.set('available', false);
      node3.set('status', 'valid');
      assert.equal(model.get('status'), 'down');

      node1.set('available', true);
      node1.set('status', 'valid');
      node2.set('available', false);
      node2.set('status', 'invalid');
      node3.set('available', false);
      node3.set('status', 'valid');
      assert.equal(model.get('status'), 'warning');
    });
  });
});
define('ember-riak-explorer/tests/unit/models/cluster-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/cluster-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/cluster-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/config-file-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('config-file', 'Unit | Model | config file', {
    needs: ['model:node']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('node relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('node');

    assert.equal(relationship.key, 'node');
    assert.equal(relationship.kind, 'belongsTo');
  });
});
define('ember-riak-explorer/tests/unit/models/config-file-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/config-file-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/config-file-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/log-file-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('log-file', 'Unit | Model | log file', {
    needs: ['model:node']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('node relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('node');

    assert.equal(relationship.key, 'node');
    assert.equal(relationship.kind, 'belongsTo');
  });
});
define('ember-riak-explorer/tests/unit/models/log-file-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/log-file-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/log-file-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/node-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('node', 'Unit | Model | node', {
    needs: ['model:cluster', 'model:log-file', 'model:config-file']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('cluster relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('cluster');

    assert.equal(relationship.key, 'cluster');
    assert.equal(relationship.kind, 'belongsTo');
  });

  (0, _emberQunit.test)('log files relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('logFiles');

    assert.equal(relationship.key, 'logFiles');
    assert.equal(relationship.kind, 'hasMany');
  });

  (0, _emberQunit.test)('config files relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('configFiles');

    assert.equal(relationship.key, 'configFiles');
    assert.equal(relationship.kind, 'hasMany');
  });

  (0, _emberQunit.test)('isHealthy', function (assert) {
    var model = this.subject();

    _ember['default'].run(function () {
      model.set('available', true);
      model.set('status', 'valid');
      assert.equal(model.get('isHealthy'), true);

      model.set('available', false);
      model.set('status', 'valid');
      assert.equal(model.get('isHealthy'), false);

      model.set('available', true);
      model.set('status', 'invalid');
      assert.equal(model.get('isHealthy'), false);
    });
  });
});
define('ember-riak-explorer/tests/unit/models/node-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/node-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/node-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/riak-object-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('riak-object', 'Unit | Model | riak object', {
    needs: ['model:bucket']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('bucket relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('bucket');

    assert.equal(relationship.key, 'bucket');

    assert.equal(relationship.kind, 'belongsTo');
  });
});
define('ember-riak-explorer/tests/unit/models/riak-object-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/riak-object-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/riak-object-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/search-index-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('search-index', 'Unit | Model | search index', {
    needs: ['model:cluster', 'model:searchSchema']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('cluster relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('cluster');

    assert.equal(relationship.key, 'cluster');

    assert.equal(relationship.kind, 'belongsTo');
  });

  (0, _emberQunit.test)('schema relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('schema');

    assert.equal(relationship.key, 'schema');

    assert.equal(relationship.kind, 'belongsTo');
  });
});
define('ember-riak-explorer/tests/unit/models/search-index-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/search-index-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/search-index-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/unit/models/search-schema-test', ['exports', 'ember-qunit', 'ember'], function (exports, _emberQunit, _ember) {

  (0, _emberQunit.moduleForModel)('search-schema', 'Unit | Model | search schema', {
    // Specify the other units that are required for this test.
    needs: ['model:cluster']
  });

  (0, _emberQunit.test)('it exists', function (assert) {
    var model = this.subject();
    var store = this.store();

    assert.ok(!!model);
    assert.ok(!!store);
  });

  (0, _emberQunit.test)('cluster relationship', function (assert) {
    var klass = this.subject({}).constructor;
    var relationship = _ember['default'].get(klass, 'relationshipsByName').get('cluster');

    assert.equal(relationship.key, 'cluster');

    assert.equal(relationship.kind, 'belongsTo');
  });
});
define('ember-riak-explorer/tests/unit/models/search-schema-test.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - unit/models/search-schema-test.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'unit/models/search-schema-test.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/utils/parse-header.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - utils/parse-header.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'utils/parse-header.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/utils/riak-help/bucket_props.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - utils/riak-help/bucket_props.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'utils/riak-help/bucket_props.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/utils/riak-help/riak_config.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - utils/riak-help/riak_config.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'utils/riak-help/riak_config.js should pass jshint.');
  });
});
define('ember-riak-explorer/tests/utils/riak-help/riak_status.jshint', ['exports'], function (exports) {
  'use strict';

  QUnit.module('JSHint - utils/riak-help/riak_status.js');
  QUnit.test('should pass jshint', function (assert) {
    assert.expect(1);
    assert.ok(true, 'utils/riak-help/riak_status.js should pass jshint.');
  });
});
/* jshint ignore:start */

require('ember-riak-explorer/tests/test-helper');
EmberENV.TESTS_FILE_LOADED = true;

/* jshint ignore:end */
//# sourceMappingURL=tests.map