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
    'settings-service', '$location', '$css',
      function ($scope, ws, $q, errors, $modal, LoginModal, confirm,
        $cookies, $window, settings, $location, $css) {
        "use strict";
        var self = this;
        self.timeout = false;
        self.disconnected = false;
        self.failed = false;
        self.errors = errors;
        var cookie = $cookies.getObject(SEASHELL_CREDS_COOKIE);
        if(cookie) {
          self.host = cookie.host;
        }

        // Help function
        self.help = function () {
          $modal.open({
            templateUrl: "frontend/templates/help-template.html",
            controller: ['$scope', 'ConfirmationMessageModal', '$window',
              '$cookies',
              function ($scope, confirm, $window, $cookies) {
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
              $q.when(ws.archiveProjects())
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
              $cookies.remove(SEASHELL_CREDS_COOKIE);
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
        ws.register_callback('failed', function () {
          // if on production, redirect to login screen; else, display error and
          // login prompt
          if(SEASHELL_BRANCH === 'stable'){
            window.location = 'https://www.student.cs.uwaterloo.ca/seashell';
          }else{
            self.failed = true;
          }
        }, true);
        settings.addWatcher(function () {
          if (settings.settings.theme_style === "dark") {
            $css.removeAll();
            $css.add("css/dark.css");
          } else {
            $css.removeAll();
            $css.add("css/light.css");
          } 
        }, true);
      }])
  .config(['hotkeysProvider', function(hotkeysProvider) {
    hotkeysProvider.includeCheatSheet = false;
  }])
  .run(['$cookies', 'socket', 'settings-service', 'error-service', 'projects', 
        '$window', '$document', '$rootScope',
        function($cookies, ws, settings, errors, projects, $window, $document, $rootScope) {
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
    $window.onresize = function () {
      $rootScope.$broadcast('window-resized');
    };
  }]);
