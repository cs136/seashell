angular.module('frontend-app')
  // controller for the list of projects
  .controller('ProjectList', ['$scope',
    function($scope) {
      "use strict";
      var self = this;
      self.projects = [["A1", "Proj"], ["A2"]];
    }])

  /**
   * WebSocket service:
   *  provides:
   *    register_disconnect_callback
   *    register_reconnect_callback
   *    register_fail_callback
   *    connect
   *    socket
   */
  .service('socket', ['$scope', '$q', '$window', '$interval', 'cookieStore', function($scope, $q, $window, $interval, cookie) {
    "use strict";
    var self = this;
    self.socket = null;

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
    self.register_disconnect_callback = function(cb) {
      disconnect_callbacks.push(cb);
    };
    self.register_connect_callback = function(cb) {
      connect_callbacks.push(cb);
    };
    /** Registers callback to run when the socket has run into an error. */
    self.register_fail_callback = function(cb) {
      failure_callbacks.push(cb);
    };

    /** Connects the socket, sets up the disconnection monitor. */ 
    self.connect = function () {
      self.socket = new SeashellWebsocket(sprintf("wss://%s:%d",cookie.get("creds").host, cookie.get("creds").port),
                                          cookie.get("creds").key,
                                          /** Failure - probably want to prompt the user to attempt to reconnect/
                                           *  log in again.
                                           */
                                          function () {
                                            $scope.$apply(function () {
                                              $interval.stop(timeout_interval);
                                              _.each(failure_callbacks, call);
                                            });
                                          },
                                          /** Socket closed - probably want to prompt the user to reconnect? */
                                          function () {
                                            $scope.apply(function () {
                                              $interval.stop(timeout_interval);
                                              _.each(disconnect_callbacks, call);
                                            });
                                          });
      return $q.when(self.socket.ready)
        .then(function () {
          console.log("Seashell socket set up properly.");
          timeout_interval = $interval(function () {
            if (timeout_count++ === 3) {
              _.each(timeout_callbacks, call);
            }
            $q.when(self.socket.ping())
              .then(function () {
                if (timeout_count >= 3) {
                  _.each(timein_callbacks, call);
                }
                timeout_count = 0;
              });
          }, 4000);
          console.log("Websocket disconnection monitor set up properly.");
          /** Run the callbacks. */
          _.each(connect_callbacks, call);
        });
    };
  }]);
