/**
 * Angular bindings for Seashell projects.
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
angular.module('project-service', ['websocket-service', 'marmoset-service'])
  /**
   * Project [factory] service.
   * Provides functions to list/load/open/create new SeashellProject
   *  instances.
   */ 
  .service('projects', ['$scope', '$q', 'socket', 'marmoset',
    function($scope, $q, ws, marmoset) {
      "use strict";
      var self = this;
      var list_url = "https://www.student.cs.uwaterloo.ca/~cs136/cgi-bin/skeleton_list.cgi";
      var skel_template = "file:///u/cs136/public_html/assignment_skeletons/%s";
     
      var SeashellProject = (function () { 
        /**
         * SeashellProject.
         *
         * @constructor Constructor for a new SeashellProject.
         *  Project is not ready until .init() is called.
         */
        function SeashellProject (name) {
          var self = this;

          self.name = name;
        }
      
        /**
         * SeashellProject.init()
         *
         * Initializes a project.
         * @param force-lock - Forcibly lock this project?
         * @returns {Angular.$q} Deferred that resolves when this is done,
         *  or an error:
         *    "locked" - Project is locked.
         *    other    - Error message.
         */
        SeashellProject.prototype.init = function(force_lock) {
          var self = this;
          if (!force_lock) {
            return $q.when(ws.socket.lockProject(self.name)).then(function () {return this;});
          } else {
            return $q.when(ws.socket.forceLockProject(self.name)).then(function () {return this;});
          }
        };

        /**
         * SeashellProject.close()
         *
         * Closes a project.
         */
        SeashellProject.prototype.close = function() {
          var self = this;
          return $q.when(ws.socket.unlockProject(self.name)); 
        };

        /** SeashellProject.questions()
         *
         * Lists all questions.
         * @returns {Angular.$q -> [string]} Promise that resolves to the list of questions.
         */
        SeashellProject.prototype.questions = function() {
          var self = this;
          return $q.when(ws.socket.listProject(self.name))
            .then(function (files) {
              return _.map(_.filter(_.map(files, function (file)
                      {return [file[0].split(), file[1]];}),
                      function (file) {
                        return file[0].length == 1 && file[0] !== "common" && !file[1];
                      }),
                      function (file) {
                        return file[0];
                      });
            });
        };

        /**
         * SeashellProject.filesFor(question)
         *
         * Lists all files present in a question.
         */
        SeashellProject.prototype.filesFor = function(question) {
          var self = this;
          return $q.when(ws.socket.listProject(self.name))
            .then(function (files) {
              files = _.map(files, function (file) {return [file[0].split("/"), file[1], file[2]];});
              var common = _.filter(files, function (file) {return file[0][0] === "common" && !file[1];});
              var question_files = _.filter(files, function (file) {
                    return file[0][0] === question && !file[1] &&
                      (file[0].length == 1 ||
                       (file[0].length >= 2 && file[0][1] !== "tests"));
                  });
              var tests = _.filter(files, function (file) {
                return (file[0].length >= 2 && file[0][1] === "tests" && !file[1]);
              });

              return {common: common, question: question_files, tests: tests};
            });
        };
      return SeashellProject;})();




      /**
       * Lists projects available.
       *
       * @returns {Angular.$q -> [String]/?} Deferred object that will resolve
       *  to the list of projects available to be opened.
       */
      self.list = function() {
        return $q.when(ws.socket.getProjects());
      };

      /**
       * Fetches new assignments.
       *
       * @returns {Angular.$q -> [projects]/[failed projects]/String} Deferred object
       *  that will resolve
       *  to the list of new assignments cloned, or a list of assignments that failed
       *  to clone, or a error message.
      */
      self.fetch = function() {
        return self.list()
            .then(function (projects) {
              return $q.when($.ajax({url: list_url, dataType: "json"}))
                .then(function (skels) {
                  var new_projects = _.filter(skels,
                      function (skel) {
                        return projects.indexOf(skel) == -1;
                      });
                  var failed_projects = [];
                  return _.foldl(new_projects,
                      function(in_continuation, template) {
                        function clone(failed) {
                          return $q.when(ws.socket.newProjectFrom(template,
                             sprintf(skel_template,
                              skel)))
                           .done(function () {
                             if (failed) {
                               $q.defer().reject("Propagating failure...");
                             }
                           })
                           .catch(function (info) {
                             failed_projects.push(template);
                           });
                        }
                        in_continuation.then(
                           function () {return clone(false);},
                           function () {return clone(true);}); 
                      },
                      $q.defer().resolve())
                    .then(function() {return (new_projects);})
                    .catch(function() {return (failed_projects);});
                })
                .catch(function () {
                  return ("Could not fetch list of skeletons!");
                });
            });
      };

      /**
       * Creates a new project.
       *
       * @param {String} name - Name of project.
       * @returns {Angular.$q -> SeashellProject/String}
       *  Angular deferred that resolves to the new, _ready_ SeashellProject instance.
       *  (or a error message on error)
       */
      self.create = function (name) {
        return $q.when(socket.newProject(name)).
          then(function () {
            return (new SeashellProject(name)).init();
          });
      };

      /** 
       * Opens an existing project.
       * @param {String} name
       * @param {boolean/optional} force-lock? - Forcibly lock project.
       * @returns {Angular.$q -> SeashellProject/String}
       *  Angular deferred that resolves to the new, _ready_ SeashellProject instance.
       *  (or a error message on error)
       */
      self.open = function(name, force_lock) {
        return (new SeashellProject(name)).init(force_lock);
      };
    }]);  
