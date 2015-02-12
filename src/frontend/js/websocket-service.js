/**
 * Angular bindings for the Seashell WebSocket client.
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
    var timeout_interval = null;
    var key = 0;
    var callbacks = {};

    /** Registers callbacks to run when the socket has not seen activity
     *  in some while, and when messages are received after a timeout has passed.
     */
    self.register_callback = function(type, cb, now) {
      callbacks[key] = {type: type, cb: cb, now: now};

      if (type === 'disconnected' && !self.connected && now) {
        $timeout(cb, 0);
      } else if (type === 'connected' && self.connected && now) {
        $timeout(cb, 0);
      } else if (type === 'failed' && self.failed && now) {
        $timeout(cb, 0);
      }
      return key++;
    };
    self.unregister_callback = function (key) {
      delete callbacks[key];
    };

    /** Helper function to invoke the I/O callback. */
    function io_cb(ignored, message) {
      _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'io';}),
                   function (x) {return x.cb;}),
             function (x) {x(message);});
    }
    function test_cb(ignored, result) {
      _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'test';}),
                   function (x) {return x.cb;}),
             function (x) {x(result);});
    }

    /** Connects the socket, sets up the disconnection monitor. */ 
    self.connect = function () {
      if (!rawCookie.get(SEASHELL_CREDS_COOKIE)) {
        self.failed = true;
        $timeout(function () {
          _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'failed';}),
                       function (x) {return x.cb;}),
            function (x) {x();});
        }, 0);
        return $q.reject("No credentials found!");
      }

      self.socket = new SeashellWebsocket(sprintf("wss://%s:%d",cookie.get(SEASHELL_CREDS_COOKIE).host, cookie.get(SEASHELL_CREDS_COOKIE).port),
                                          cookie.get(SEASHELL_CREDS_COOKIE).key,
                                          /** Failure - probably want to prompt the user to attempt to reconnect/
                                           *  log in again.
                                           */
                                          function () {
                                            self.failed = true;
                                            $scope.$apply(function () {
                                              $interval.cancel(timeout_interval);
                                              _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'failed';}),
                                                           function (x) {return x.cb;}),
                                                function (x) {x();});
                                            });
                                          },
                                          /** Socket closed - probably want to prompt the user to reconnect? */
                                          function () {
                                            self.connected = false;
                                            $scope.$apply(function () {
                                              $interval.cancel(timeout_interval);
                                              _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'disconnected';}),
                                                           function (x) {return x.cb;}),
                                                function (x) {x();});
                                            });
                                          });
      return $q.when(self.socket.ready)
        .then(function () {
          console.log("Seashell socket set up properly.");
          timeout_interval = $interval(function () {
            if (timeout_count++ === 3) {
              _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'timeout';}),
                           function (x) {return x.cb;}),
                function (x) {x();});
            }
            $q.when(self.socket.ping())
              .then(function () {
                if (timeout_count >= 3) {
                  _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'timein';}),
                               function (x) {return x.cb;}),
                    function (x) {x();});
                }
                timeout_count = 0;
              });
          }, 4000);
          self.connected = true;
          self.failed = false;
          self.socket.requests[-3].callback = io_cb;
          self.socket.requests[-4].callback = test_cb;
          console.log("Websocket disconnection monitor set up properly.");
          /** Run the callbacks. */
          _.each(_.map(_.filter(callbacks, function (x) {return x.type === 'connected';}),
                       function (x) {return x.cb;}),
            function (x) {x();});
        });
    };
  }]);
