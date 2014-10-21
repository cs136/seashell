/**
 * Seashell's frontend.
 * Copyright (C) 2013-2014 The Seashell Maintainers.
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
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */
angular.module('frontend-app', ['seashell-websocket', 'seashell-projects', 'jquery-cookie', 'ui.router',
    'ui.bootstrap'])
  // Delete Project Modal Service
  .factory('DeleteProjectModal', ['$modal', 'projects',
      function ($modal, projects) {
        return function (project) {
          return $modal.open({
            templateUrl: "frontend/templates/delete-project-template.html",
            controller: ['$scope', function ($scope) {
              $scope.project = project;
            }]
          }).result.then(function () {
            projects.delete(project);
          });
        };
      }])
  // New Project Modal Service
  .factory('NewProjectModal', ['$modal', 'projects',
      function ($modal, projects) {
        return function () {
          return $modal.open({
            templateUrl: "frontend/templates/new-project-template.html",
            controller: ['$scope', function ($scope) {
              $scope.new_project_name = "";
              $scope.newProject = function () {
                if ($scope.new_project_name === "") {
                  return false;
                } else {
                  $scope.$close();
                }
              };
            }]
          }).result.then(function () {
            /** Move to new project. */
          });
        };
      }])
  // Main controller
  .controller('FrontendController', ['$scope', 'socket', '$q',
      function ($scope, ws, $q) {
        "use strict";
        var self = this;
        self.timeout = false;
        self.disconnected = false;
        self.failed = false;
        self.ready = false;

        ws.register_timein_callback(function () {self.timeout = false;});
        ws.register_timeout_callback(function () {self.timeout = true;});

        ws.register_connect_callback(function () {self.disconnected = false; self.timeout = false; self.failed = false;});
        ws.register_disconnect_callback(function () {self.disconnected = true;});
        
        ws.register_fail_callback(function () {self.failed = true;});
        ws.connect().finally(function () {self.ready = true;});
      }])
  // Controller for Project Lists
  .controller('ProjectListController', ['$rootScope', 'projects', '$q', 'DeleteProjectModal',
      function ($scope, projects, $q, deleteProjectModal) {
    var self = this;
    self.list = [];
    self.question_list = {};
    self.state = "list-projects";

    /** Run this every time the state associated with this controller is loaded.
     *  Returns a deferred that resolves when the state is properly loaded */
    self.refresh = function () {
      return projects.list().then(function (projects_list) {
        self.list = projects_list;

        return $q.when(_.map(projects_list, function (project) {
          return projects.open(project, true).questions().then(function (questions) {
            self.question_list[project] = questions;
          });
        }));
      });
    };

    /** Delete onClick handler. */
    self.delete = function(project) {
      deleteProjectModal(project).then(function () {
        self.refresh();
      });
    };

    /** Make refresh be called on any state transition to this state,
     *  and on the first time through this state. */
    $scope.$on('$stateChangeStart', function(_0, toState, _1, _2) {
      if (toState.name === self.state) {
        self.refresh();
      }
    });
    self.refresh();
  }])
  // Configuration for routes
  .config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {
    $urlRouterProvider.otherwise('/');
    $stateProvider
      .state("list-projects", {
        url: "/",
        templateUrl: "frontend/templates/project-list-template.html",
        controller: "ProjectListController as projects"
        })
      .state("list-projects.new-project", {
        url: "new",
        onEnter: ["$state", "NewProjectModal",
          function ($state, newProjectModal) {
            return newProjectModal().catch(function () {
              $state.transitionTo("list-projects");
            });
        }]});
  }]);
