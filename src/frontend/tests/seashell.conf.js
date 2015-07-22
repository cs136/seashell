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
      'lib/jquery-2.1.3.min.js',
      'lib/underscore-min.js',
      'lib/sprintf.min.js',
      'lib/sjcl.min.js',
      'lib/jquery.timeago.min.js',
      'lib/codemirror.min.js',
      'lib/clike.min.js',
      'lib/scheme.min.js',
      'lib/dialog.min.js',
      'lib/searchcursor.min.js',
      'lib/search.min.js',
      'lib/matchbrackets.min.js',
      'lib/simplescrollbars.min.js',
      'lib/clike.min.js',
      'lib/vim.min.js',
      'lib/emacs.min.js',
      'lib/active-line.min.js',
      'lib/lint.min.js',
      'lib/fullscreen.min.js',
      'lib/rulers.min.js',
      'lib/show-hint.min.js',
      'lib/angular.min.js',
      'lib/angular-mocks.js',
      'lib/angular-cookies.min.js',
      'lib/angular-ui-router.js',
      'lib/ui-bootstrap.min.js',
      'lib/ui-bootstrap-tpls.min.js',
      'lib/angular-css.min.js',
      // Libraries stored in repo
      '../js/lib/ui-codemirror.js',
      '../js/lib/hotkeys.min.js',
      '../js/lib/autofill-event.js',
      // JS files we are testing
      '../js/socket/crypto.js',
      '../js/socket/websocket_client.js',
      '../js/marmoset-service.js',
      '../js/websocket-service.js',
      '../js/project-service.js',
      // Frontend JS files we are testing
      '../frontend/frontend.js',
      '../frontend/filters.js',
      '../frontend/errors.js',
      '../frontend/modals.js',
      '../frontend/directives.js',
      '../frontend/settings.js',
      '../frontend/console.js',
      '../frontend/project-list.js',
      '../frontend/project.js',
      '../frontend/question.js',
      '../frontend/file.js',
      '../frontend/routes.js',
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
