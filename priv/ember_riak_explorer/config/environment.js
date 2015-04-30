/* jshint node: true */

var explorerHost = 'http://localhost';
var explorerPort = '8098';
var explorerUrl = explorerHost + ':' + explorerPort;

module.exports = function(environment) {
  var ENV = {
    modulePrefix: 'ember-riak-explorer',
    environment: environment,
    baseURL: '/',
    locationType: 'auto',

    EmberENV: {
        // Riak Explorer vars
        explorerHost: explorerHost,
        explorerPort: explorerPort,

      FEATURES: {
        // Here you can enable experimental features on an ember canary build
        // e.g. 'with-controller': true

      }
    },

    APP: {
      // Here you can pass flags/options to your application instance
      // when it is created


    }
  };

  ENV.contentSecurityPolicy = {
      'default-src': "'self' " + explorerUrl,
      'connect-src': "'self' " + explorerUrl
  };

  if (environment === 'development') {
    // ENV.APP.LOG_RESOLVER = true;
    // ENV.APP.LOG_ACTIVE_GENERATION = true;
    // ENV.APP.LOG_TRANSITIONS = true;
    // ENV.APP.LOG_TRANSITIONS_INTERNAL = true;
    // ENV.APP.LOG_VIEW_LOOKUPS = true;
  }

  if (environment === 'test') {
    // Testem prefers this...
    ENV.baseURL = '/';
    ENV.locationType = 'none';

    // keep test console output quieter
    ENV.APP.LOG_ACTIVE_GENERATION = false;
    ENV.APP.LOG_VIEW_LOOKUPS = false;

    ENV.APP.rootElement = '#ember-testing';
  }

  if (environment === 'production') {

  }

  return ENV;
};
