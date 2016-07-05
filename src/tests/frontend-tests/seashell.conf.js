// Karma configuration
// Generated on Thu Jul 16 2015 11:55:11 GMT-0400 (EDT)

module.exports = function(config) {
  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',

    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['jasmine'],


    // list of files / patterns to load in the browser
    files: [
      //First ones must stay up to date with the 
      // scripts in ../frontend/includes/scripts.html
      '../../../lib/jslib/tests/jquery-2.1.3.min.js',
      '../../../lib/jslib/tests/underscore-min.js',
      '../../../lib/jslib/tests/sprintf.min.js',
      '../../../lib/jslib/tests/sjcl.min.js',
      '../../../lib/jslib/tests/jquery.timeago.min.js',
      '../../../lib/jslib/tests/codemirror.min.js',
      '../../../lib/jslib/tests/clike.min.js',
      '../../../lib/jslib/tests/scheme.min.js',
      '../../../lib/jslib/tests/dialog.min.js',
      '../../../lib/jslib/tests/searchcursor.min.js',
      '../../../lib/jslib/tests/search.min.js',
      '../../../lib/jslib/tests/matchbrackets.min.js',
      '../../../lib/jslib/tests/simplescrollbars.min.js',
      '../../../lib/jslib/tests/clike.min.js',
      '../../../lib/jslib/tests/vim.min.js',
      '../../../lib/jslib/tests/emacs.min.js',
      '../../../lib/jslib/tests/active-line.min.js',
      '../../../lib/jslib/tests/lint.min.js',
      '../../../lib/jslib/tests/fullscreen.min.js',
      '../../../lib/jslib/tests/rulers.min.js',
      '../../../lib/jslib/tests/show-hint.min.js',
      '../../../lib/jslib/tests/angular.min.js',
      '../../../lib/jslib/tests/angular-mocks.js',
      '../../../lib/jslib/tests/angular-cookies.min.js',
      '../../../lib/jslib/tests/angular-ui-router.js',
      '../../../lib/jslib/tests/ui-bootstrap.min.js',
      '../../../lib/jslib/tests/ui-bootstrap-tpls.min.js',
      '../../../lib/jslib/tests/angular-css.min.js',
      // Libraries stored in repo
      '../../../lib/jslib/ui-codemirror.js',
      '../../../lib/jslib/hotkeys.min.js',
      '../../../lib/jslib/autofill-event.js',
      // JS files we are testing
      '../../frontend/js/socket/crypto.js',
      '../../frontend/js/socket/websocket_client.js',
      '../../frontend/js/marmoset-service.js',
      '../../frontend/js/websocket-service.js',
      '../../frontend/js/project-service.js',
      // Frontend JS files we are testing
      '../../frontend/frontend/frontend.js',
      '../../frontend/frontend/filters.js',
      '../../frontend/frontend/errors.js',
      '../../frontend/frontend/modals.js',
      '../../frontend/frontend/directives.js',
      '../../frontend/frontend/settings.js',
      '../../frontend/frontend/console.js',
      '../../frontend/frontend/project-list.js',
      '../../frontend/frontend/project.js',
      '../../frontend/frontend/question.js',
      '../../frontend/frontend/file.js',
      '../../frontend/frontend/routes.js',
      'lib/creds.js',
      // Tests
      'spec/*.js'
    ],


    // list of files to exclude
    exclude: [
    ],


    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
    },


    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['progress'],


    // web server port
    port: 9876,


    // enable / disable colors in the output (reporters and logs)
    colors: true,


    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_DISABLE,


    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true, 

    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher 
    browsers: ['PhantomJS'],

    //Timeouts
    captureTimeout : 60000,

    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
    singleRun: true
    
  })
};
