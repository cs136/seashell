angular.module('frontend-app', ['websocket-service'])
  // controller for the list of projects
  .controller('ProjectList', ['$scope',
    function($scope) {
      "use strict";
      var self = this;
      self.projects = [["A1", "Proj"], ["A2"]];
    }]);
