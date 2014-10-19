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
angular.module('frontend-app', ['seashell-websocket', 'seashell-projects', 'jquery-cookie'])
  // Main controller
  .controller('FrontendController', ['$scope', 'socket',
      function ($scope, ws) {
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
        ws.connect().then(function () {self.ready = true});
      }]);
