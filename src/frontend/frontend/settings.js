/**
 * Seashell's frontend settings service
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
  // Settings service.
  .service('settings-service', ['$rootScope', '$modal', 'socket', 'error-service', '$q', 'ConfirmationMessageModal',
      function ($rootScope, $modal, ws, errors, $q, confirm) {
        var self = this;
        self.settings =  {
          font : "Consolas",
          font_size  : 12,
          editor_mode  : "standard",
          tab_width  : "2",
          use_space : true,
          force_narrow : false,
          offline_mode : ws.offline_mode,
          theme_style : "light"
        };
        self.notify = {};
        self.needToLoad = true;
        var nKey = 0;

        function notifyChanges () {
          _.forEach(self.notify, function (fn) {fn();});
        }

        /** Adds and removes watchers on the settings service. */
        self.addWatcher = function(fn, invoke) {
          self.notify[nKey] = fn;
          var result = invoke && fn ();
          return nKey ++;
        };
        self.removeWatcher = function(key) {
          delete self.notify[key];
        };

        self.load = function () {
          // do nothing if we haven't written anything
          if (!self.needToLoad) {
            return $q.when();
          }
          return $q.when(ws.getSettings()).then(function (settings) {
            self.needToLoad = false;
            if (settings) {
              for (var k in settings)
                self.settings[k] = settings[k];
              self.settings.offline_mode = ws.offline_mode;
            }
            // Backwards compatibility.
            if (typeof self.settings.font_size === 'string') {
              self.settings.font_size = parseInt(self.settings.font_size);
            }
            notifyChanges();
          }).catch(function (message) {
            errors.report(message, "Could not load settings from server.");
          });
        };

        self.save = function() {
          self.needToLoad = true;
          ws.setOfflineModeSetting(self.settings.offline_mode);
          return $q.when(ws.saveSettings(self.settings)).then(notifyChanges);
        };

        self.dialog = function() {
          return $modal.open({
            templateUrl: "frontend/templates/settings-template.html",
            controller: ['$scope', function ($scope) {
              self.settings.offline_mode = ws.offline_mode;
              $scope.temp = _.clone(self.settings);
              $scope.saveSettings = function () {
                $scope.$close();
                self.settings = $scope.temp;
                if(ws.offline_mode === 0 && self.settings.offline_mode !== 0) {
                  confirm("Enable Offline Mode",
                    "Are you sure you want to enable Seashell's offline mode? This will store all of your projects locally so you can work on them and run them while temporarily disconnected form the internet. Only do this if you are on your own personal computer!")
                    .then(self.save);
                }
                else if(ws.offline_mode !== 0 && self.settings.offline_mode === 0) {
                  confirm("Disable Offline Mode",
                    "Are you sure you want to disable Seashell's offline mode? This means you will not be able to work offline if your computer loses connection. Any unsynced local changes you have will be lost. To use offline mode again in this browser, you will need to re-enable offline mode and press sync all.")
                    .then(self.save);
                }
                else {
                  self.save().catch(
                    function (error) {
                      errors.report(error, "Could not save settings!");
                    });
                }
                return true;
              };}]
            });
        };


      }]);
