
angular.module('frontend-app')
  // controller for the list of projects
  .controller('ProjectList', ['$scope',
    function($scope) {
      "use strict";
      var self = this;
      self.projects = [["A1", "Proj"], ["A2"]];
    }])

  // websocket service
  .service('socket', ['$window', function($window) {
    var self = this;
    SeashellCoder.addEntropy();
    creds = read_login_credentials();
    if(creds) {
      self.s = new SeashellWebsocket("wss://" + creds.host + ":" + creds.port, creds.key);
      self.s.ready.done(function() {
        console.log("Seashell socket set up properly.");
        setupUI();
        console.log("User interface set up properly.");
        setInterval(setupDisconnectMonitor, 4000);
        console.log("Websocket disconnection monitor set up properly.");
        fetchNewAssignments().done(updateListOfProjects);
        updateMarmosetProjects();
        /** Install refresh handler */
        window.onbeforeunload = function() {
          if(SeashellProject.currentProject && SeashellProject.currentProject.isUnsaved())
            return "Are you sure you want to leave Seashell? Unsaved data may be lost.";
        };
        /** Run the rest of the code. */
        if(rest)
          rest();
      });
      self.s.ready.fail(function() {
        displayErrorMessage("Seashell socket could not be set up.");
      });
    }
    else {
      $window.location.replace("/seashell/");
    }
  });
