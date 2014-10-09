
angular.module('frontend-app')
  // controller for the list of projects
  .controller('ProjectList', ['$scope',
    function($scope) {
      "use strict";
      var self = this;
      self.projects = [["A1", "Proj"], ["A2"]];
    }])

  // websocket service
  .service('socket', ['$q', '$window', '$interval', 'cookieStore', function($q, $window, $interval, cookie) {
    var self = this;
    var dccount = 0;
    var disconnect_callbacks = [];
    var reconnect_callbacks = [];

    self.register_disconnect_callback = function(cb) {
      disconnect_callbacks.push(cb);
    }
    self.register_reconnect_callback = function(cb) {
      reconnect_callbacks.push(cb);
    }

    function setupDisconnectMonitor() {
      var max_dcs = 3;

      function onReconnect() {
        _.each(reconnect_callbacks, call);
        dccount = 0;
      }
      
      if(max_dcs == dccount++) {
        _.each(disconnect_callbacks, call);
      }
      if(self.s.websocket.readyState == 3) { // if socket is closed
        self.s = new SeashellWebsocket("wss://" + creds.host + ":" + creds.port, creds.key);
        $q.when(self.s.ready).done(onReconnect);
      }
      else {
        $q.when(self.s.ping).done(onReconnect);
      }
    }

    SeashellCoder.addEntropy();
    var creds = cookie.get("creds");
    if(creds) {
      self.s = new SeashellWebsocket("wss://" + creds.host + ":" + creds.port, creds.key);
      var qprom = $q.when(self.s.ready);
      qprom.done(function() {
        console.log("Seashell socket set up properly.");
        $interval(setupDisconnectMonitor, 4000);
        console.log("Websocket disconnection monitor set up properly.");
      });
      qprom.fail(function() {
        displayErrorMessage("Seashell socket could not be set up.");
      });
    }
    else {
      $window.location.replace("/seashell/");
    }
  });
