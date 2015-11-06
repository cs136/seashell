/**
 * Seashell's frontend route configuration
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
angular.module('frontend-app')
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
                  var type = projects.error ? 
                    (projects.error.indexOf("503")===-1 ? "seashell" : "webserver")
                    : "seashell";
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
            return decodeURI($stateParams.file);
          }],
          openFolder: ['$stateParams', function($stateParams) {
            return $stateParams.part;
          }]}
      });
  }]);
