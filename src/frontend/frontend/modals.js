/**
 * Seashell's frontend modal factories
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

angular.module('frontend-app')
  // Confirmation message modal service
  .factory('ConfirmationMessageModal', ['$modal',
      function ($modal) {
        return function (title, message) {
          return $modal.open({
            templateUrl: "frontend/templates/confirmation-template.html",
            controller: ['$scope', function ($scope) {
              $scope.title = title;
              $scope.message = message;
            }]}).result;
        };
      }])
  .factory('LoginModal', ['$modal',
      function($modal) {
        return function() { return $modal.open({
          templateUrl: "frontend/templates/login-template.html",
          controller: ['$scope', '$window', '$cookies', 'socket',
            function($scope, $window, $cookies, ws) {
              $scope.username = "";
              $scope.password = "";
              $scope.reset = false;
              $scope.busy = false;
              $scope.error = false;

              var current = $cookies.getObject(SEASHELL_CREDS_COOKIE);
              if (current) {
                $scope.username = current.user;
              }

              $scope.login = function() {
                $scope.busy = true;
                $scope.error = false;
                var target = sprintf("https://%s%s/cgi-bin/login2.cgi",
                  $window.location.host,
                  $window.location.pathname.substring(0, $window.location.pathname.lastIndexOf('/')));
                $.ajax({url: target,
                        type: "POST",
                        data: {"u": $scope.username, "p": $scope.password, "reset": $scope.reset},
                        dataType: "json"})
                  .done(function(data) {
                    $scope.$apply(function() {
                      $scope.busy = false;
                      if(data.error !== undefined) {
                        $scope.error = sprintf("An error was encountered while logging in: %s (code %d)",
                          data.error.message, data.error.code);
                        console.log(self.error);
                      } else if(data.port !== undefined) {
                        $cookies.putObject(SEASHELL_CREDS_COOKIE, data, {secure:true});
                        console.log("All done login!");
                        ws.connect().then(function() {
                          $scope.$dismiss();
                        })
                        .catch(function() {
                          $scope.error = "Could not connect to the websocket!";
                        });
                      } else {
                        $scope.error = "An internal error occurred: " + textStatus;
                        console.log(error);
                      }
                    });
                  }).fail(function(error) {
                    $scope.$apply(function() {
                      $scope.busy = false;
                      $scope.error = error;
                      console.log(error);
                    });
                  });
              };
            }
          ]}).result; };
      }])
  .factory('DeleteProjectModal', ['$modal', 'projects', 'error-service',
      function ($modal, projects, errors) {
        return function (project) {
          return $modal.open({
            templateUrl: "frontend/templates/delete-project-template.html",
            controller: ['$scope', function ($scope) {
              $scope.project = project;
            }]
          }).result.then(function () {
            return projects.delete(project).catch(
                function (error) {
                  errors.report(error, sprintf("Could not delete project %s!", project));
                });
          });
        };
      }])
  .factory('NewProjectModal', ['$modal', 'projects', 'error-service',
      function ($modal, projects, errors) {
        return function () {
          return $modal.open({
            templateUrl: "frontend/templates/new-project-template.html",
            controller: ['$scope', '$state', 'projects', 'error-service',
            function ($scope, $state, projects, errors) {
              $scope.new_project_name = "";
              $scope.newProject = function () {
                if ($scope.new_project_name === "") {
                  return false;
                } else {
                  $scope.$close();
                  projects.create($scope.new_project_name).then(function () {
                    $state.go('edit-project', {project: $scope.new_project_name});
                  }).catch(function (error) {
                    errors.report(error, sprintf("Could not create project %s!", $scope.new_project_name));
                  });
                }
              };
            }]
          }).result;
        };
      }])
  .factory('RenameFileModal', ['$modal', 'error-service',
    function($modal, errors) {
      return function(project, question, folder, file, notify) {
        notify = notify || function() {};
        return $modal.open({
          templateUrl: "frontend/templates/rename-file-template.html",
          controller: ["$scope", "$state", "error-service",
            function($scope, $state, errors) {
              $scope.rename_name = (folder=='common' ? "" : question + "/") +
                (folder=='question' ? "" : folder + "/") + file;
              $scope.renameFile = function() {
                project.renameFile(question, folder, file, folder, $scope.rename_name)
                  .then(function() {
                    $scope.$close();
                    notify($scope.rename_name);
                  })
                  .catch(function(err) {
                    $scope.$dismiss();
                    errors.report(err, "An error occurred while renaming the file.");
                  });
              };
            }]
          });
      };
  }])
  // Directive for 
  // New File Modal Service
  .factory('NewFileModal', ['$modal', 'error-service',
      function ($modal, errors) {
        return function (project, question, notify) {
          notify = notify || function () {};
          return $modal.open({
            templateUrl: "frontend/templates/new-file-template.html",
            controller: ['$scope', '$state', 'error-service', '$q', '$timeout',
            function ($scope, $state, errors, $q, $timeout) {
              $scope.new_file_name = "";
              $scope.new_file_folder = question;
              $scope.new_file_upload = [];
              $scope.question = question;
              $scope.inputError = false;
              $scope.normalize = false;
              $scope.newFile = function () {
                // 4 cases: file name specified AND file to upload
                //          no file name AND file to upload
                //          file name AND no file to upload
                //          no file name AND no file to upload
                if ($scope.new_file_upload.length > 0 && $scope.new_file_name.length > 0) {
                  $scope.inputError = "Can't specify file name when uploading files!";
                  return false;
                } else if ($scope.new_file_upload.length > 0 && $scope.new_file_name.length === 0) {
                  // For each file, upload.
                  _.forEach($scope.new_file_upload, function (file) {
                    var filename = file.name; // NOTE: does not contain path information!
                    var reader = new FileReader();
                    var extension = filename.split('.').pop();
                    var normalize = $scope.normalize && ['c', 'h', 'txt', 'rkt', 'in', 'expect'].indexOf(extension) >= 0;
                    reader.onload = function () {
                      project.createFile($scope.new_file_folder, question,
                        filename, reader.result, "url", normalize)
                             .then(function () {
                               notify(true, true, project, question, $scope.new_file_folder, filename);
                             })
                             .catch(function (error) {
                               notify(true, false, project, question, $scope.new_file_folder, filename);
                               errors.report(error, sprintf("Could not upload file %s!", filename));
                             });
                    };
                    reader.onerror = function () {
                      $timeout(function () {
                        errors.report({}, sprintf("Could not read file %s!", filename));
                        notify(true, false, project, question, $scope.new_file_folder, filename);
                      });
                    };
                    reader.readAsDataURL(file);

                  });
                  $scope.$close();
                } else if ($scope.new_file_upload.length === 0 && $scope.new_file_name.length > 0) {
                  var filename = $scope.new_file_name;
                  var extension = filename.split('.').pop();
                  var result = null;
                  // Write default contents.
                  if (extension === 'c' || extension === 'h') {
                      result = project.createFile($scope.new_file_folder, question, filename,
                                        sprintf("/**\n File: %s\nEnter a description for this file.\n*/\n", filename));
                  } else if (extension === 'rkt') {
                      result = project.createFile($scope.new_file_folder, question, filename,
                                        sprintf("#lang racket\n;; File %s\n;;Enter a description for this file.\n", filename));
                  } else {
                      result = project.createFile($scope.new_file_folder, question, filename);
                  }
                  result.then(function () {
                    notify(false, true, project, question, $scope.new_file_folder, filename);
                  })
                  .catch(function (error) {
                    notify(false, false, project, question, $scope.new_file_folder, filename);
                    errors.report(error, sprintf("Could not create file %s!", filename));
                  });
                  $scope.$close();
                } else {
                  $scope.inputError = "Need to specify file!";
                  return false;
                }
              };
            }]
          }).result;
        };
      }])
  // Directive for New Question Modal Service
  .factory('NewQuestionModal', ['$modal', 'error-service',
    function ($modal, errors){
        return function(project){
            return $modal.open({
                templateUrl: "frontend/templates/new-question.template.html",
                controller:  ['$scope', '$state', 'error-service', '$q',
                function ($scope, $state, errors, $q){
                    $scope.new_question_name = "";
                    $scope.inputError = false;
                    $scope.newQuestion = function () {
                        var promise = project.createQuestion($scope.new_question_name);
                        if(promise) promise.then(function () {
                            $state.go("edit-project.editor",
                                      {question:$scope.new_question_name});
                            $scope.$close();
                        });
                    };
                }]
            }).result;
        };
    }])
  // Submit to Marmoset Modal
  .factory('SubmitMarmosetModal', ['$modal', 'error-service',
      function ($modal, errors) {
        return function (project, question, notify) {
          notify = notify || function () {};
          return $modal.open({
            templateUrl: "frontend/templates/marmoset-submit-template.html",
            controller: ['$scope', '$state', 'error-service', '$q', 'marmoset',
            function ($scope, $state, errors, $q, marmoset) {
              $q.all([marmoset.projects(), project.currentMarmosetProject(question) || undefined])
                .then(function(res) {
                  $scope.marmoset_projects = res[0];
                  $scope.selected_project = res[1];
                  $scope.submit = function() {
                    $scope.$close();
                    project.submit(question, $scope.selected_project)
                       .catch(function (error) {
                         var type = error.error ? (error.error.indexOf("marmoset_submit") === -1 ? "seashell" : "marmoset") : "seashell";
                         errors.report(error, sprintf("Could not submit project %s!", $scope.selected_project), type);
                         notify(false, $scope.selected_project);
                         return $q.reject(error);
                       }).then(function () {
                         notify(true, $scope.selected_project);
                       });
                  };
                });
            }]}).result;
        };
      }])
  // Marmoset Results Modal
  .factory('MarmosetResultsModal', ['$modal', 'error-service',
      function ($modal, errors) {
        return function (results) {
          return $modal.open({
            templateUrl: "frontend/templates/marmoset-results-template.html",
            size: "lg",
            controller: ['$scope', '$state', 'error-service',
              function ($scope, $state, errors) {
                $scope.results = results;
              }]});
        };
      }]);
