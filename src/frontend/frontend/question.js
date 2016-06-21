/**
 * Seashell's frontend question view controller
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

/* jshint supernew: true */
angular.module('frontend-app')
  // Editor Controller
  .controller("EditorController", ['$state', 'openQuestion', '$scope', 'error-service',
      'openProject', 'NewFileModal', 'NewTestModal', 'SubmitMarmosetModal', '$interval', 'marmoset',
      'NewQuestionModal', 'MarmosetResultsModal', 'console-service', 'socket',
      function ($state, openQuestion, $scope, errors,
        openProject, newFileModal, newTestModal,  submitMarmosetModal,
        $interval, marmoset, newQuestionModal,
        marmosetResultsModal, Console, ws) {
        var self = this;
        self.question = openQuestion;
        self.project = openProject;
        self.common_files = [];
        self.question_files = [];
        self.runnerFile = "";
        self.test_files = [];
        self.console = Console;
        self.marmoset_short_results = null;
        self.marmoset_long_results = null;
        self.marmoset_refresh_interval = undefined;
        self.marmoset_timeout = 5000; // Anything less than 2500ms will cause issues.

        // Destroy interval when scope goes away.
        function cancelMarmosetRefresh() {
          var result = self.marmoset_refresh_interval &&
            $interval.cancel(self.marmoset_refresh_interval);
          self.marmoset_refresh_interval = null;
        }
        $scope.$on('$destroy', function() {
          cancelMarmosetRefresh();
          self.console.clear();
          ws.unregister_callback(self.refreshKey);
        });

        /*
         * handleMarmosetResults(result, target)
         *  result - from marmoset.results() call
         *  target - the Marmoset project we are getting results fora
        */
        self.handleMarmosetResults = function(result, target) {
          var data = result.result;
          if(result.error) {
            errors.report(result.result, sprintf("Failed to fetch Marmoset results for %s.", target), "marmoset");
            self.marmoset_short_results = null;
          }
          else if(data.length > 0 && data[0].status=="complete") {
            cancelMarmosetRefresh();
            var sub_pk = data[0].submission;
            var failed = false;
            var related = _.filter(data, function (entry) {
              return entry.submission === sub_pk;
            });
            self.marmoset_long_results = related;
            var total = 0, total_passed = 0;
            for(var i = 0; i < related.length; i++) {
              total += related[i].points;
              total_passed += related[i].outcome === "passed" ? related[i].points : 0;
              failed = failed || related[i].outcome !== "passed";
            }
            self.marmoset_short_results =
              sprintf("%s (%d/%d)", !failed ? "passed" : "failed",
                    total_passed, total);
          }
        };

        /** Refreshes the controller [list of files, ...] */
        self.refresh = function () {
          function groupfiles(lof) {
            lof.sort();
            var groups = [];
            for(var i=0; i<lof.length; i++) {
              groups.push([lof[i]]);
              var current_file = lof[i];
              while(i<lof.length-1 && lof[i+1] == lof[i]) {
                groups[groups.length-1].push(lof[i+1]);
                lof.splice(i+1, 1);
              }
              // wrap the list in an object with a boolean
              // isFileToRun is true if the file is the file to run
              groups[groups.length-1] = {
                files: groups[groups.length-1],
                isFileToRun: _.contains(groups[groups.length-1], self.runnerFile)
              };
            }
            return groups;
          }
          self.project.getFileToRun(self.question)
              .then(function (fileToRun) {
                self.runnerFile = fileToRun;
                var result = self.project.filesFor(self.question);
                self.common_files = groupfiles(result.common);
                self.question_files = groupfiles(result.question);
                self.test_files = groupfiles(result.tests);
              })
              .catch(function () {
                self.runnerFile = "";
                var result = self.project.filesFor(self.question);
                self.common_files = groupfiles(result.common);
                self.question_files = groupfiles(result.question);
                self.test_files = groupfiles(result.tests);
              });


          self.project.currentMarmosetProject(self.question).then(function(target) {
            if(target) {
              marmoset.results(target).then(function(result) {
                self.handleMarmosetResults(result, target);
              });
            }
          });
        };

        /** Handle the setFileToRun broadcast
         *  by refreshing the file list
         */
        $scope.$on('setFileToRun', function () {
            self.refresh();
        });

        /** Adds file to the project. */
        self.add_file = function () {
          newFileModal(self.project,
                       self.question,
                       self.common_files,
                       function () {
                           self.refresh();
                       });
        };

        /** Adds a pair of .in and .expect files to the project
         */
        self.add_test = function () {
          newTestModal(self.project, self.question, function () {
             self.refresh();
          });
        };

        /** Dispatches a function to run when the
         *  current file is saved.
         *
         *  If there is no such current file,
         *  run function immediately.
         */
        function runWhenSaved(fn) {
          if ($state.is('edit-project.editor.file')) {
            $scope.$broadcast('run-when-saved', fn);
          } else {
            fn();
          }
        }

        self.view_results = function() {
          marmosetResultsModal(self.marmoset_long_results);
        };

        /** Submits the current question. */
        self.submit_question = function (){runWhenSaved(function(){
          submitMarmosetModal(self.project, self.question, function (success, target) {
            if (!success) {
              self.marmoset_short_results = "failed to submit!";
            } else {
              var submitTime = new Date();
              cancelMarmosetRefresh();
              self.marmoset_refresh_interval = $interval(function () {
                marmoset.results(target).then(function (result) {
                  self.handleMarmosetResults(result, target);
                  var data = result.result;
                  if(data.length > 0 && data[0].status != "complete") {
                    self.marmoset_short_results =
                      sprintf("received %s (waiting on tests)",
                              $.timeago(data[0].timestamp));
                  } else if(data.length === 0) {
                    self.marmoset_short_results =
                      sprintf("submitted %s (waiting on receipt)",
                              $.timeago(submitTime));
                  }
                }).catch(function (error) {
                  errors.report(error, sprintf("Could not fetch results for %s!", target), "marmoset");
                  self.marmoset_short_results = "could not fetch results...";
                  cancelMarmosetRefresh();
                });
              }, self.marmoset_timeout);
            }
          }).then(function () {
            self.marmoset_short_results = "submitting...";
          });
        });};

        /** Try to load the question, and go back to the project if we can't. */
        var key = ws.register_callback('connected', function() {
          try {
            self.refresh();
            self.project.updateMostRecentlyUsed(self.question);
            self.project.mostRecentlyUsed(self.question)
              .then(function (recent) {
                if (recent && $state.is('edit-project.editor')) {
                  $state.go("edit-project.editor.file",
                            {part: recent.part, file: recent.file},
                            {location: "replace"});
                }
              });
          } catch (e) {
            errors.report({}, sprintf("Could not open question %s!", self.question));
            $state.go("edit-project");
          }
        });
        $scope.$on('$destroy', function() {
          ws.unregister_callback(key);
        });
      }]);
