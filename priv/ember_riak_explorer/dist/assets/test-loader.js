/* globals requirejs, require */
(function() {
define("ember-cli/test-loader",
  [],
  function() {
    "use strict";

    var moduleIncludeMatchers = [];
    var moduleExcludeMatchers = [];

    function addModuleIncludeMatcher(fn) {
      moduleIncludeMatchers.push(fn);
    };

    function addModuleExcludeMatcher(fn) {
      moduleExcludeMatchers.push(fn);
    };

    function checkMatchers(matchers, moduleName) {
      var matcher;

      for (var i = 0, l = matchers.length; i < l; i++) {
        matcher = matchers[i];

        if (matcher(moduleName)) {
          return true;
        }
      }

      return false;
    }

    function TestLoader() {
      this._didLogMissingUnsee = false;
    };

    TestLoader.prototype = {
      shouldLoadModule: function(moduleName) {
        return (moduleName.match(/[-_]test$/));
      },

      listModules: function() {
        return Object.keys(requirejs.entries);
      },

      loadModules: function() {
        var moduleName, index, length;
        var moduleNames = this.listModules();

        for (index = 0, length = moduleNames.length; index < length; index++) {
          moduleName = moduleNames[index];

          if (checkMatchers(moduleExcludeMatchers, moduleName)) {
            continue;
          }

          if (checkMatchers(moduleIncludeMatchers, moduleName) || this.shouldLoadModule(moduleName)) {
            this.require(moduleName);
            this.unsee(moduleName);
          }
        }
      }
    };

    TestLoader.prototype.require = function(moduleName) {
      try {
        require(moduleName);
      } catch(e) {
        this.moduleLoadFailure(moduleName, e);
      }
    };

   TestLoader.prototype.unsee = function(moduleName) {
     if (typeof require.unsee === 'function') {
       require.unsee(moduleName);
     } else if (!this._didLogMissingUnsee) {
      this._didLogMissingUnsee = true;
      if (typeof console !== 'undefined') {
        console.warn('unable to require.unsee, please upgrade loader.js to >= v3.3.0');
      }
     }
    };

    TestLoader.prototype.moduleLoadFailure = function(moduleName, error) {
      console.error('Error loading: ' + moduleName, error.stack);
    };

    TestLoader.load = function() {
      new TestLoader().loadModules();
    };

    return {
      'default': TestLoader,
      addModuleIncludeMatcher: addModuleIncludeMatcher,
      addModuleExcludeMatcher: addModuleExcludeMatcher
    };
  }
);
})();

/*global QUnit, blanket, mocha,  $ */

function sendCoverage() {
	try {
		var data = JSON.stringify(window._$blanket_coverageData);
		$.ajax({
			type: 'POST',
			url:'/write-blanket-coverage',
			datatype: 'json',
			contentType:'application/json; charset=utf-8',
			data: data
		  });
	} catch(err) {
		console.error('JSON stringify error:', err);
		throw err;
	}
}

var origBlanketOnTestsDone = blanket.onTestsDone;

function cliFinish() {
	origBlanketOnTestsDone.apply(blanket);
	sendCoverage();
}

blanket.onTestsDone = cliFinish;

if (typeof(QUnit) === 'object') {
  QUnit.config.autostart = blanket.options('cliOptions').autostart !== false;
}
else if (typeof(mocha) === 'object') {

    /*
    * Mocha-BlanketJS adapter
    * Adds a BlanketJS coverage report at the bottom of the HTML Mocha report
    * Only needed for in-browser report; not required for the grunt/phantomjs task
    *
    * Distributed as part of the grunt-blanket-mocha plugin
    * https://github.com/ModelN/grunt-blanket-mocha
    * (C)2013 Model N, Inc.
    * Distributed under the MIT license
    *
    * Code originally taken from the BlanketJS project:
    * https://github.com/alex-seville/blanket/blob/master/src/adapters/mocha-blanket.js
    * Distributed under the MIT license
    */
    (function() {

        if(!mocha) {
            throw new Error("mocha library does not exist in global namespace!");
        }


        /*
         * Mocha Events:
         *
         *   - `start`  execution started
         *   - `end`  execution complete
         *   - `suite`  (suite) test suite execution started
         *   - `suite end`  (suite) all tests (and sub-suites) have finished
         *   - `test`  (test) test execution started
         *   - `test end`  (test) test completed
         *   - `hook`  (hook) hook execution started
         *   - `hook end`  (hook) hook complete
         *   - `pass`  (test) test passed
         *   - `fail`  (test, err) test failed
         *
         */

        var originalReporter = mocha._reporter;

        var blanketReporter = function(runner) {
                runner.on('start', function() {
                  blanket.setupCoverage();
                });

                runner.on('end', function() {
                  blanket.onTestsDone();
                });
                runner.on('suite', function() {
                    blanket.onModuleStart();
                });

                runner.on('test', function() {
                    blanket.onTestStart();
                });

                runner.on('test end', function(test) {
                    blanket.onTestDone(test.parent.tests.length, test.state === 'passed');
                });

                //I dont know why these became global leaks
                runner.globals(['stats', 'failures', 'runner', '_$blanket']);

                originalReporter.apply(this, [runner]);
            };

        blanketReporter.prototype = originalReporter.prototype;

        mocha.reporter(blanketReporter);
    })();
}
//# sourceMappingURL=test-loader.map