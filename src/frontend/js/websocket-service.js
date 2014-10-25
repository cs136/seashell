/**
 * Angular bindings for the Seashell WebSocket client.
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
angular.module('seashell-websocket', ['jquery-cookie'])
  /**
   * WebSocket service:
   *  provides:
   *    register_disconnect_callback |
   *    register_reconnect_callback  | Callbacks to be invoked on change of socket connectivity.
   *    register_fail_callback       | 
   *    register_timein/out_callback |
   *    connect                      - Connects the socket
   *    socket                       - Socket object.  Is invalid after disconnect/fail | before connect.
   */
  .service('socket', ['$rootScope', '$q', '$interval', 'cookie', 'cookieStore', '$timeout',
      function($scope, $q, $interval, rawCookie, cookie, $timeout) {
    "use strict";
    var self = this;
    self.socket = null;
    self.connected = false;
    self.failed = false;

    var timeout_count = 0;
    var timeout_callbacks = [];
    var timein_callbacks = [];
    var timeout_interval = null;
    var disconnect_callbacks = [];
    var connect_callbacks = [];
    var failure_callbacks = [];

    /** Registers callbacks to run when the socket has not seen activity
     *  in some while, and when messages are received after a timeout has passed.
     */
    self.register_timeout_callback = function(cb) {
      timeout_callbacks.push(cb);
    };
    self.register_timein_callback = function(cb) {
      timein_callbacks.push(cb);
    };
    /** Registers callbacks to run when the socket loses/gains connectivity. */
    self.register_disconnect_callback = function(cb, runNow) {
      disconnect_callbacks.push(cb);

      if (runNow && !self.connected) {
        $timeout(cb, 0);
      }
    };
    self.register_connect_callback = function(cb, runNow) {
      connect_callbacks.push(cb);

      if (runNow && self.connected) {
        $timeout(cb, 0);
      }
    };
    /** Registers callback to run when the socket has run into an error. */
    self.register_fail_callback = function(cb, runNow) {
      failure_callbacks.push(cb);
      
      if (runNow && self.failed) {
        $timeout(cb, 0);
      }
    };

    /** Connects the socket, sets up the disconnection monitor. */ 
    self.connect = function () {
      if (!rawCookie.get("seashell-session")) {
        self.failed = true;
        $timeout(function () {
          _.each(failure_callbacks, function (x) {x();});
        }, 0);
        return $q.reject("No credentials found!");
      }

      self.socket = new SeashellWebsocket(sprintf("wss://%s:%d",cookie.get("seashell-session").host, cookie.get("seashell-session").port),
                                          cookie.get("seashell-session").key,
                                          /** Failure - probably want to prompt the user to attempt to reconnect/
                                           *  log in again.
                                           */
                                          function () {
                                            self.failed = true;
                                            $scope.$apply(function () {
                                              $interval.cancel(timeout_interval);
                                              _.each(failure_callbacks, function (x) {x();});
                                            });
                                          },
                                          /** Socket closed - probably want to prompt the user to reconnect? */
                                          function () {
                                            self.connected = false;
                                            $scope.$apply(function () {
                                              $interval.cancel(timeout_interval);
                                              _.each(disconnect_callbacks, function (x) {x();});
                                            });
                                          });
      return $q.when(self.socket.ready)
        .then(function () {
          console.log("Seashell socket set up properly.");
          timeout_interval = $interval(function () {
            if (timeout_count++ === 3) {
              _.each(timeout_callbacks, function (x) {x();});
            }
            $q.when(self.socket.ping())
              .then(function () {
                if (timeout_count >= 3) {
                  _.each(timein_callbacks, function (x) {x();});
                }
                timeout_count = 0;
              });
          }, 4000);
          self.connected = true;
          self.failed = false;
          console.log("Websocket disconnection monitor set up properly.");
          /** Run the callbacks. */
          _.each(connect_callbacks, function (x) {x();});
        });
    };
  }]);
