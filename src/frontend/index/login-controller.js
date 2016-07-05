/**
 * Seashell's login tools.
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
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */
angular.module('login-app', ['ngCookies'])
  .controller('LoginController', ['$scope', '$cookies', '$window',
      function($scope, $cookies, $window) {
        "use strict";
        var self = this;
        self.error = false;
        self.user = "";
        self.busy = false;
        self.login = function(user) {
          self.busy = true;
          self.error = false;
          var target = sprintf("https://%s%s/cgi-bin/login2.cgi",
              $window.location.host,
              $window.location.pathname.substring(0, $window.location.pathname.lastIndexOf('/')));
          $.ajax({url: target,
                  type: "POST",
                  data: {"u": self.user, "p": self.password},
                  dataType: "json"})
            .done(function(data) {
              $scope.$apply(function () {
                self.busy = false;
                if(data.error !== undefined) {
                  self.error = sprintf("An error was encountered while logging in: %s (code %d)", data.error.message,
                    data.error.code);
                  console.log(self.error);
                } else if (data.port !== undefined) {
                  $cookies.putObject(SEASHELL_CREDS_COOKIE, data, {secure: true});
                  console.assert($cookies.getObject(SEASHELL_CREDS_COOKIE), "You are probably not using https.");
                  console.log("All done login!");
                  $window.top.location = "frontend.html";
                } else {
                  self.error = "An internal error occurred: " + textStatus;
                  console.log(error);
                }});
            }).fail(function(error) {
              $scope.$apply(function () {
                self.busy = false;
                self.error = error;
                console.log(self.error);
              });
            });
        };
      }]);
