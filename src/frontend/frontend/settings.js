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
  .service('settings-service', ['$rootScope', '$modal', 'socket', 'error-service', '$q',
      function ($rootScope, $modal, ws, errors, $q) {
        var self = this;
        self.settings =  {
          font : "Consolas",
          font_size  : 12,
          editor_mode  : "standard",
          tab_width  : 2,
          use_space : true,
          force_narrow : false,
          theme : "dark",
          offline_mode : "0"
        };
        self.notify = {};
        self.needToLoad = true;
        var nKey = 0;

        function notifyChanges () {
          _.forEach(self.notify, function (fn) {fn();});
          // Notify of offline/online changes 
          if (self.settings.offline_mode === "2")
          ws.forceOfflineMode(self.settings.offline_mode === "2"); 
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
            if (settings)
              for (var k in settings)
                self.settings[k] = settings[k];
            // Backwards compatibility.
            if (typeof self.settings.font_size === 'string') {
              self.settings.font_size = parseInt(self.settings.font_size);
            }
            notifyChanges();
          }).catch(function (message) {
            errors.report(message, "Could not load settings from server.");
          });
        };

        self.save = function () {
          self.needToLoad = true;
          return $q.when(ws.saveSettings(self.settings)).then(notifyChanges);
        };

        self.dialog = function () {
          return $modal.open({
            templateUrl: "frontend/templates/settings-template.html",
            controller: ['$scope', function ($scope) {
              $scope.temp = _.clone(self.settings);
              $scope.saveSettings = function () {
                $scope.$close();
                self.settings = $scope.temp;
                self.save().catch(
                  function (error) {
                    errors.report(error, "Could not save settings!");
                  });
                return true;
              };}]
            });
        };


      }]);
