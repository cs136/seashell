/**
 * Seashell's frontend error service
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

angular.module('frontend-app')
  // Error service.
  .service('error-service', ['$rootScope', '$timeout', '$sce',
    function ($rootScope, $timeout, $sce) {
    var self = this;
    self.errors = [];

    self.types = {
      "seashell" : "If this error persists, please email <a href='mailto:seashell@cs.uwaterloo.ca'>seashell@cs.uwaterloo.ca</a> the error message, and the following information for debugging purposes:",
      "marmoset" : "Make sure you can still access Marmoset's web interface, and try again in a few minutes.",
      "webserver" : "Make sure you can access other websites located on the student.cs.uwaterloo.ca subdomain and try again in a few minutes."
    };

    self.getMessage = function(type) {
      return $sce.trustAsHtml(type ? self.types[type] : self.types.seashell);
    };

    self.report = function (error, shorthand, type) {
      if (error) {
        console.log(error);
        self.errors.push({shorthand: shorthand, error: error, type: type});
        $timeout(function() {$rootScope.$broadcast('window-resized');}, 0);
      }
    };
    self.suppress = function (index) {
      self.errors.splice(index, 1);
      $timeout(function() {$rootScope.$broadcast('window-resized');}, 0);
    };
  }]);
