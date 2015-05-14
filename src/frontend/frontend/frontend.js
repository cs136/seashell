/**
 * Seashell's frontend controller
 * Copyright (C) 2013-2015 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* jshint supernew: true */
angular.module('frontend-app', ['seashell-websocket', 'seashell-projects', 'jquery-cookie', 'ui.router',
    'ui.bootstrap', 'ui.codemirror', 'cfp.hotkeys'])
  // Main controller
  .controller('FrontendController', ['$scope', 'socket', '$q', 'error-service',
    '$modal', 'LoginModal', 'ConfirmationMessageModal', 'cookieStore', '$window',
    'settings-service', '$location',
      function ($scope, ws, $q, errors, $modal, LoginModal, confirm,
        cookieStore, $window, settings, $location) {
        "use strict";
        var self = this;
        self.timeout = false;
        self.disconnected = false;
        self.failed = false;
        self.errors = errors;
        var cookie = cookieStore.get(SEASHELL_CREDS_COOKIE);
        if(cookie) {
          self.host = cookie.host;
        }

        // Help function
        self.help = function () {
          $modal.open({
            templateUrl: "frontend/templates/help-template.html",
            controller: ['$scope', 'ConfirmationMessageModal', '$window',
              'cookieStore',
              function ($scope, confirm, $window, cookies) {
                $scope.login = function () {
                  self.login();
                  $scope.$dismiss();
                };
                $scope.archive = function() {
                  self.archive();
                  $scope.$dismiss();
                };
              }]});
        };
        // confirmation modal for archiving all projects
        self.archive = function() {
          confirm("Archive Projects",
            "Are you sure you want to archive all of your projects? If you do this, you will no longer be able to retrieve them through Seashell, but they will be accessible from your student.cs Linux account.")
            .then(function() {
              $q.when(ws.socket.archiveProjects())
                .then(function() {
                  // look at all these callbacks
                  $location.path("/");
                  $window.location.reload();
                 }).catch(function(err) {
                   self.errors.report(err, "Failed to archive projects.");
                 });
            });
        };
        // Logout
        self.logout = function () {
          confirm("Log out of Seashell",
            "Do you wish to logout?  Any unsaved data will be lost.")
            .then(function () {
              cookieStore.remove(SEASHELL_CREDS_COOKIE);
              $window.top.location = "https://cas.uwaterloo.ca/logout";
            });
        };
        // Settings
        self.settings = function () {
          settings.dialog();
        };
        // Reconnect
        self.reconnect = function () {
          ws.connect();
        };
        // Open login dialog window after disconnection
        self.login = function() {
          new LoginModal().then(function() {
            self.refresh();
          });
        };

        // This won't leak memory, as FrontendController stays in scope all the time.
        ws.register_callback('timein', function () {self.timeout = false;});
        ws.register_callback('timeout', function () {self.timeout = true;});
        ws.register_callback('connected',
            function () {self.disconnected = false; self.timeout = false; self.failed = false;}, true);
        ws.register_callback('disconnected', function () {self.disconnected = true;}, true);
        ws.register_callback('failed', function () {self.failed = true;}, true);
      }])
  .config(['hotkeysProvider', function(hotkeysProvider) {
    hotkeysProvider.includeCheatSheet = false;
  }])
  // Configuration for routes
  .config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {
    $urlRouterProvider.otherwise('/');
    $stateProvider
      .state("list-projects", {
        url: "/",
        templateUrl: "frontend/templates/project-list-template.html",
        controller: "ProjectListController as projects",
        resolve: {projectList: ['$q', 'projects', 'error-service', 'socket', function ($q, projects, errors, ws) {
            return new function () {
              var self = this;
              self.list = [];
              /** Run this every time the state associated with this controller is loaded.
               *  Returns a deferred that resolves when the state is properly loaded */
              self.refresh = function () {
                return projects.fetch().catch(function (projects) {
                  var type = projects.error.indexOf("503")===-1 ? "seashell" : "webserver";
                  errors.report(projects, 'Could not fetch projects.', type);
                }).then(function () {
                  return projects.list().then(function (projects_list) {
                    function compareTime(a, b) {
                      if(a[1] === b[1]){
                        return 0;
                      }
                      else {
                        return (a[1] > b[1]) ? -1 : 1;
                      }
                    }
                    projects_list.sort(compareTime);
                    self.list = projects_list;
                  }).catch(function (error) {
                    errors.report(error, "Could not generate list of projects.");
                  });
                });
              };
              /** Store the key into our callback [this is important, as a new object
               *  is created every time into this state, and we'll have to remove the CB
               *  as to not leave a dangling reference]. 
               *
               *  Great manual memory management in JavaScript.
               */
              var cb_key = ws.register_callback('connected', function () {self.refresh();}, true);
              self.destroy = function () {
                ws.unregister_callback(cb_key);
              };
            }();
          }]},
        onExit: ['projectList', function (projectList) {
          projectList.destroy();
        }]
        })
      .state("edit-project", {
        url: "/project/{project}",
        templateUrl: "frontend/templates/project-template.html",
        controller: "ProjectController as projectView",
        resolve: {openProject: ['projects', '$state', '$stateParams', 'error-service',
                                'ConfirmationMessageModal', function(projects, $state, $stateParams, errors, confirm) {
                                   return projects.open($stateParams.project).catch(function (error) {
                                     if (error === 'locked') {
                                       return confirm(sprintf('Unlock %s', $stateParams.project),
                                                      sprintf('Project %s is open in another browser.  Proceed?', $stateParams.project))
                                         .then(function () {
                                           return projects.open($stateParams.project, 'force-lock')
                                                          .catch(function (error) {
                                                             $state.go('list-projects');
                                                             errors.report(error, sprintf("Could not open project %s!", $stateParams.project));
                                                             return null;
                                                            });
                                           })
                                          .catch(function () {
                                            $state.go('list-projects');
                                          });
                                       } else {
                                         $state.go('list-projects');
                                         errors.report(error, sprintf("Could not open project %s!", $stateParams.project));
                                         return null;
                                       }
                                     });
                                   }]},
          onExit: ['openProject', function (project) {
            project.close();
          }]
        })
      .state("edit-project.editor", {
        url: "/edit/{question}",
        templateUrl: "frontend/templates/project-editor-template.html",
        controller: "EditorController as editView",
        resolve: {openQuestion: ['$stateParams', function($stateParams) {
          return $stateParams.question;
        }],
          scrollInfo: function() { return {}; },
          undoHistory: function() { return {}; }
        }
      })
      .state("edit-project.editor.file", {
        url: "/file/{part}/{file}",
        templateUrl: "frontend/templates/project-editor-editview-template.html",
        controller: "EditFileController as editFileView",
        resolve: {openFile: ['$stateParams', function($stateParams) {
            return $stateParams.file;
          }],
          openFolder: ['$stateParams', function($stateParams) {
            return $stateParams.part;
          }]}
      });
  }])
  .run(['cookie', 'socket', 'settings-service', 'error-service', 'projects', 
        '$window', '$document', '$rootScope',
        function(cookies, ws, settings, errors, projects, $window, $document, $rootScope) {
    ws.connect()
        .then(function () {
        });
    // Reload settings on (re)connect.
    ws.register_callback('connected', function () {
      return settings.load().catch(function (error) {
        errors.report(error, 'Could not load settings!');
      });
    });
    // Set up resize
    $($window).resize(function () {
      $rootScope.$broadcast('window-resized');
    });
  }]);
