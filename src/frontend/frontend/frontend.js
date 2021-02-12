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
angular.module('frontend-app', ['seashell-websocket', 'seashell-projects', 'ngCookies', 'ui.router',
    'ui.bootstrap', 'ui.codemirror', 'cfp.hotkeys', 'door3.css'])
  // Main controller
  .controller('FrontendController', ['$scope', 'socket', '$q', 'error-service',
    '$modal', 'LoginModal', 'ConfirmationMessageModal', '$cookies', '$window',
    'settings-service', '$location', '$css', 'projects', '$rootScope',
      function ($scope, ws, $q, errors, $modal, LoginModal, confirm,
        $cookies, $window, settings, $location, $css, projects, $rootScope) {
        "use strict";
        var self = this;
        self.timeout = false;
        self.disconnected = false;
        self.failed = false;
        self.offline_mode = false;
        self.has_offline_changes = false;
        self.errors = errors;
        self.oldsafari = !bowser.check({safari: '10'}, false, window.navigator.userAgent);
        $scope.navigator = navigator;
        self.updateHostPort = function () {
          var cookie = $cookies.getObject(SEASHELL_CREDS_COOKIE);
          if(cookie) {
            self.host = cookie.host;
            self.port = cookie.port;
            return {host: cookie.host, port: cookie.port}
          } else {
            return {host: "", port: 0}
          }
        }
        self.updateHostPort();
        // Refresh function
        self.refresh = function () {
          $rootScope.$broadcast('projects-refreshed');
        };
	// This modal is displayed when user clicks the connection
        // info link beside the Seashell logo
        self.webpingmodal = function () {
          var modal_footer = '<div class="modal-footer"><button type="button" class="btn btn-primary" ng-click="$close()">Close</button></div>';
          $modal.open({
            template: "<span class='page-text' ng-bind-html=\"vm.trustAsHtml(vm.message)\"></span>" + modal_footer,
            controllerAs: 'vm',
            controller: ['$scope', '$sce', '$cookies', '$http', function ($scope, $sce, $cookies, $http) {
              var self = this;
              self.trustAsHtml = $sce.trustAsHtml;
              self.message = "Loading...";
              var host = $cookies.getObject(SEASHELL_CREDS_COOKIE).host;
              var port = $cookies.getObject(SEASHELL_CREDS_COOKIE).port;
              var seashell_websocket_url = "https://"+host+":"+port;
              $http.get(seashell_websocket_url, { timeout: 5000, method: 'GET', params: {type: 'webping' } })
                .then(function (response) {
                  // success
                  console.log('success', response);
                  self.message =  response.data;//"Your Seashell session is set to connect to:<br/>"
                    //+ seashell_websocket_url;
                }, function (response) {
                  // error
                  console.log('error', response);
                  self.message = "Could not access Seashell connection (status: "
                    + (response.status == 0 ? "no response" : response.status) + "):<br/>"
		    + seashell_websocket_url;
                });
            }],
          });
        };
        // Help function
        self.help = function () {
          $modal.open({
            templateUrl: "frontend/templates/help-template.html",
            controller: ['$scope', 'ConfirmationMessageModal', '$window',
              '$cookies',
              function ($scope, confirm, $window, $cookies) {
                $scope.login = function (reset) {
                  self.login(reset);
                  $scope.$dismiss();
                };
                $scope.archive = function() {
                  self.archive();
                  $scope.$dismiss();
                };
              }]});
        };
        // offline mode info modal
        self.offline_info = function() {
          $modal.open({
            templateUrl: "frontend/templates/offline-info-template.html",
            controller: ['$scope', function($scope) {
              $scope.settings = function() {
                $scope.$dismiss();
                self.settings();
              };
            }]
          });
        };
        // confirmation modal for archiving all projects
        self.archive = function() {
          confirm("Archive Projects",
            "Are you sure you want to archive all of your projects? If you do this, you will no longer be able to retrieve them through Seashell, but they will be accessible from your student.cs Linux account.")
            .then(function() {
              $q.when(ws.archiveProjects())
                .then(function() {
                   self.refresh();
                 }).catch(function(err) {
                   self.errors.report(err, "Failed to archive projects.");
                 });
            });
        };
        // Sync all
        self.syncAll = function() {
          confirm("Sync all projects",
              "Confirming will download all files for use in offline mode. You should only have to do this once per browser.")
            .then(function() {
              $q.when(ws.syncAll()).then(function () {
                })
                .catch(function (err) {
                  self.errors.report(err, "Failed to sync all projects.");
                });
            });
        };
        self.hasOfflineChanges = function() {
          return ws.hasOfflineChanges();
        };
        self.isSyncing = function() {
          return ws.isSyncing;
        };
        // Logout
        self.logout = function () {
          confirm("Log out of Seashell",
            "Do you wish to logout?  Any unsaved data will be lost.")
            .then(function () {
              $cookies.remove(SEASHELL_CREDS_COOKIE);
              $window.top.location = SEASHELL_URL;
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
        self.login = function(reset) {
          new LoginModal(reset, self.updateHostPort).then(function() { });
        };

        // This won't leak memory, as FrontendController stays in scope all the time.
        ws.register_callback('timein', function () {self.timeout = false;});
        ws.register_callback('timeout', function () {self.timeout = true;});
        ws.register_callback('connected', function(offline_mode) {
          self.updateHostPort();
          self.disconnected = false; self.timeout = false;
          self.failed = false; self.offline_mode = offline_mode;
        }, true);
        ws.register_callback('disconnected',
          function() { self.disconnected = true; });
        ws.register_callback('failed', function() {
          // if on production, redirect to login screen; else, display error and
          // login prompt
          if(SEASHELL_BRANCH === 'stable'){
            window.location = SEASHELL_URL;
          }else {
            self.failed = true;
          }
        }, true);

        settings.addWatcher(function () {
          if (settings.settings.theme_style === "dark") {
            $css.removeAll();
            $css.add("frontend/css/dark.min.css");
          } else {
            $css.removeAll();
            $css.add("frontend/css/light.min.css");
          }
        }, true);
      }])
  .config(['hotkeysProvider', function(hotkeysProvider) {
    hotkeysProvider.includeCheatSheet = false;
  }])
  .run(['$cookies', 'socket', 'settings-service', 'error-service', 'projects',
        '$window', '$document', '$rootScope', 'localfiles',
        function($cookies, ws, settings, errors, projects, $window, $document, $rootScope,
                localfiles) {
    var ck = $cookies.getObject(SEASHELL_CREDS_COOKIE);
    if (ck) {
      Raven.setUserContext({email: ck.user + "@uwaterloo.ca"});
    }
    ws.connect();
    // Reload settings on (re)connect.
    ws.register_callback('connected', function () {
      return settings.load().catch(function (error) {
        errors.report(error, 'Could not load settings!');
      });
    });
  }]);
