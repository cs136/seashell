/**
 * Seashell's frontend.
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

/* jshint supernew: true */
angular.module('frontend-app', ['seashell-websocket', 'seashell-projects', 'jquery-cookie', 'ui.router',
    'ui.bootstrap', 'ui.codemirror', 'cfp.hotkeys'])
  .filter('projectFilter', function() {
    return function(input, type){
      var pattAssn = new RegExp('^A[0-9]+$');
      var pattTut = new RegExp('^Tut[0-9]+$');
      var pattLec = new RegExp('^Lec[0-9]+$');
      var out = [];
      for(var i = 0; i < input.length; i++){
        if(type === 'A'){
          if(pattAssn.test(input[i])){
            out.push(input[i]);
          }
        }
        else if(type === 'TUT'){
          if(pattTut.test(input[i])){
            out.push(input[i]);
          }
        }
        else if(type === 'LEC'){
          if(pattLec.test(input[i])){
            out.push(input[i]);
          }
        }
        else {
          if(!pattAssn.test(input[i]) && !pattTut.test(input[i]) && !pattLec.test(input[i])){
            out.push(input[i]);
          }
        }
      }
      return out;
    };
  })
  // Error service.
  .service('error-service', ['$rootScope', '$timeout', '$sce',
    function ($rootScope, $timeout, $sce) {
    var self = this;
    self.errors = [];

    self.types = {
      "seashell" : "If this error persists, please email <a href='mailto:seashell@cs.uwaterloo.ca'>seashell@cs.uwaterloo.ca</a> the error message, and the following information for debugging purposes:",
      "marmoset" : "Make sure you can still access Marmoset's web interface, and try again in a few minutes.",
      "webserver" : "Make sure you can access other websites located on the student.cs.uwaterloo.ca subdomain and try again in a few minutes."
    };

    self.getMessage = function(type) {
      return $sce.trustAsHtml(type ? self.types[type] : self.types.seashell);
    };

    self.report = function (error, shorthand, type) {
      if (error) {
        self.errors.push({shorthand: shorthand, error: error, type: type});
        $timeout(function() {$rootScope.$broadcast('window-resized');}, 0);
      }
    };
    self.suppress = function (index) {
      self.errors.splice(index, 1);
      $timeout(function() {$rootScope.$broadcast('window-resized');}, 0);
    };
  }])
  // Confirmation message modal service.
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
          controller: ['$scope', '$window', 'cookieStore', 'socket',
            function($scope, $window, cookieStore, ws) {
              $scope.username = "";
              $scope.password = "";
              $scope.reset = false;
              $scope.busy = false;
              $scope.error = false;
            
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
                        cookieStore.add(SEASHELL_CREDS_COOKIE, data, {secure:true});
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
  // Delete Project Modal Service
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
  // New Project Modal Service
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
  // Commit Project Modal Service
  .factory('CommitProjectModal', ['$modal', 'error-service',
      function ($modal, errors) {
        return function (project) {
          var modal = $modal.open({
            templateUrl: "frontend/templates/commit-project-template.html",
            controller: ['$scope', 'socket', 'error-service',
            function ($scope,  ws, errors) {
              $scope.commit_descr = "Saved "+(new Date()).toUTCString()+".";
              $scope.editor = null;
              $scope.codemirror_opts = {
                lineWrapping: true,
                mode: "text/plain",
                onLoad: function (cm) {
                  $scope.editor = cm;
                }
              };
              $scope.commit_project = function () {
                $scope.$close();
                project.save($scope.commit_descr)
                       .catch(function (error) {
                         errors.report(error, sprintf("Could not commit %s to storage!", project.name));
                       });
              };
            }]
          });
          return modal.result;
        };
      }])
  // Directive for binding a mutator watcher (HTML5)
  .directive('whenVisible', ['$parse', function ($parse) {
    return {
      link: function (scope, elem, attrs) {
        var triggered = false;
        scope.$watch(function () {
          if (elem.is(':visible') && !triggered) {
            $parse(attrs.whenVisible)(scope);
            triggered = true;
          }
        });
      }
    };
  }])
  // Directive for binding file uploads.
  .directive('filelistBind', ['$parse', function ($parse) {
    return {
      link: function (scope, elem, attrs) {
        elem.bind('change', function (event) {
          scope.$apply(function () {
            $parse(attrs.filelistBind).assign(scope, event.target.files);
          });
        });
      }
    };
  }])
  .directive('focusOn', ['$timeout', function($timeout) {
     return function(scope, elem, attr) {
        scope.$on(attr.focusOn, function(e) {
            $timeout(function () {elem[0].focus();});
        });
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
              $scope.rename_name = file;
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
                    reader.onload = function () {
                      project.createFile($scope.new_file_folder, question,
                        filename, reader.result, "url", $scope.normalize)
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
                         var type = error.error.indexOf("marmoset_submit")===-1 ? "seashell" : "marmoset";
                         errors.report(error, sprintf("Could not submit project %s!", $scope.selected_project), type);
                         notify(false, $scope.selected_project);
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
      }])
  // Settings service.
  .service('settings-service', ['$rootScope', '$modal', 'socket', 'error-service', '$q',
      function ($rootScope, $modal, ws, errors, $q) {
        var self = this;
        self.settings =  {
          font_size  : 10,
          editor_mode  : "standard",
          tab_width  : 2,
          text_style : "neat",
          use_space : true,
          force_narrow : false
        };
        self.notify = {};
        var nKey = 0;

        function notifyChanges () {
          _.forEach(self.notify, function (fn) {fn();});
        }

        /** Adds and removes watchers on the settings service. */
        self.addWatcher = function(fn, invoke) {
          self.notify[nKey] = fn;
          var result = invoke && fn ();
          return nKey ++;
        };
        self.removeWatcher = function(key) {
          delete self.notify[key];
        };

        self.load = function () {
          return $q.when(ws.socket.getSettings()).then(function (settings) {
            if (settings)
              for (var k in settings)
                self.settings[k] = settings[k];
            // Backwards compatibility.
            if (typeof self.settings.font_size === 'string') {
              self.settings.font_size = parseInt(self.settings.font_size);
            }
            notifyChanges();
          }).catch(function (message) {
            errors.report(message, "Could not load settings from server.");
          });
        };

        self.save = function () {
          return $q.when(ws.socket.saveSettings(self.settings));
        };

        self.dialog = function () {
          return $modal.open({
            templateUrl: "frontend/templates/settings-template.html",
            controller: ['$scope', function ($scope) {
              $scope.temp = _.clone(self.settings);
              $scope.saveSettings = function () {
                $scope.$close();
                self.settings = $scope.temp;
                self.save().then(notifyChanges).catch(
                  function (error) {
                    errors.report(error, "Could not save settings!");
                  });
                return true;
              };}]
            });
        };
      }])
  .service('console-service', ['$rootScope', 'socket', function($scope, socket) {
    var self = this;
    self.PIDs = null;
    // running is true iff we are running with "run", allows input
    self.running = false;
    self.inst = null;
    self.contents = "";
    self.errors = [];
    // Buffers
    self.stdout = "";
    self.stderr = "";
    self.asan_parse = false;
    self._contents = "";
    var ind = "";
    var spl ="";
    var asan_contents = [];

    // contents is an array of lines of address sanitizer output
    function parse_asan_output(contents) {
      var filepatt = /\/([^\/]+:[0-9]+)$/;
      var addrpatt = /0x[0-9a-f]{12}/;
      if(/ SEGV /.test(contents[1])) { // segfault
        self._write(sprintf("%s: Attempt to access invalid address %s.\n",
          filepatt.exec(contents[2])[1],
          addrpatt.exec(contents[1])));
      }
      else if(/stack-buffer-(over|under)flow /.test(contents[1])) { // stack buffer overflow
        self._write(sprintf("%s: Stack buffer overflow on address %s. Check array indices.\n",
          filepatt.exec(contents[3])[1],
          addrpatt.exec(contents[1])));
      }
      else if(/heap-buffer-(over|under)flow /.test(contents[1])) { // heap buffer overflow
        self._write(sprintf("%s: Heap buffer overflow on address %s. Check indices used for dynamically allocated arrays.\n",
          filepatt.exec(contents[3])[1],
          addrpatt.exec(contents[1])));
      }
      else if(/LeakSanitizer:/.test(contents[1])) { // memory leak
        self._write("Memory leaks occurred:\n");
        for(var idx=3; idx < contents.length; idx++) {
          if(/^(Direct|Indirect)/.test(contents[idx])) {
            var last = idx;
            for(; !/^$/.test(contents[last]); last++);
            last--;
            self._write(sprintf("  %s byte(s) allocated at %s never freed.\n",
              /[0-9]+/.exec(contents[idx]),
              filepatt.exec(asan_contents[last])[1]));
            idx = last+1;
          }
        }
      }
      else if(/heap-use-after-free /.test(contents[1])) { // use after free
        self._write(sprintf("%s: Using address %s after it has been freed.\n",
          filepatt.exec(contents[3])[1],
          addrpatt.exec(contents[1])));
      }
      else if(/double-free /.test(contents[1])) { // double free
        self._write(sprintf("%s: Attempting to free address %s, which has already been freed.\n",
          filepatt.exec(contents[3])[1],
          addrpatt.exec(contents[1])));
      }
      else { // else print usual message
        _.each(contents, function(line) {
          self._write(line + "\n");
        });
      }
    }

    socket.register_callback("io", function(io) {
      if(io.type == "stdout") {
        ind = io.message.indexOf("\n");
        if(ind > -1) {
          spl = io.message.split("\n");
          self._write(self.stdout);
          while(spl.length>1) { self._write(spl.shift() + "\n"); }
          self.stdout = spl[0];
        }
        else
          self.stdout += io.message;
      }
      else if(io.type == "stderr") {
        ind = io.message.indexOf("\n");
        if(ind > -1) {
          if(!self.asan_parse)
            self._write(self.stderr);
          else
            io.message = self.stderr + io.message;
          spl = io.message.split("\n");
          while(spl.length>1) {
            if(!self.asan_parse && /^=+$/.test(spl[0])) {
              self.asan_parse = true;
            }
            else if(!self.asan_parse)
              self._write(spl.shift() + "\n");
            else
              asan_contents.push(spl.shift());
          }
          self.stderr = spl[0];
        }
        else
          self.stderr += io.message;
      }
      else if(io.type == "done") {
        self._write(self.stdout);
        self._write(self.stderr);
        self.stdout = self.stderr = "";
        if(self.asan_parse) {
          parse_asan_output(asan_contents);
          self.asan_parse = false;
          asan_contents = [];
        }
        self.write("Program finished with exit code "+io.status+".\n");
        self.PIDs = null;
        self.running = false;
      }
      self.flush();
    });
    socket.register_callback("test", function(res) {
      self.PIDs = _.without(self.PIDs, res.pid);
      self.PIDs = self.PIDs.length === 0 ? null : self.PIDs;
      if(res.result==="passed") {
        self.write(sprintf("Test %s passed.\n", res.test_name));
      }
      else if(res.result==="failed") {
        self.write(sprintf("Test %s failed.\n", res.test_name));
        self.write('Produced output (stdout):\n');
        self.write(res.stdout);
        self.write('Produced output (stderr):\n');
        self.write(res.stderr);
        self.write('\n');
      } else if(res.result==="error") {
        self.write(sprintf("Test %s caused an error (with return code %d)!\n", res.test_name, res.exit_code));
        self.write('Produced output (stderr):\n');
        self.write(res.stderr);
        self.write('\n');
      } else if(res.result==="no-expect") {
        self.write(sprintf("Test %s produced output (stdout):\n", res.test_name));
        self.write(res.stdout);
        self.write('Produced output (stderr):\n');
        self.write(res.stderr);
        self.write('\n');
      } else if(res.result==="timeout") {
        self.write(sprintf("Test %s timed out.\n", res.test_name));
      }
      else if(res.result==="killed") {
        self.write(sprintf("Test %s was killed.\n", res.test_name));
      }
    });

    self.setRunning = function(project, PIDs, testing) {
      self.running = !testing;
      self.PIDs = PIDs;
      _.each(self.PIDs, function (pid) {
        socket.socket.startIO(project.name, pid);
      });
    };
    self.clear = function() {
      self.contents = self._contents = "";
      self.stdin = self.stdout = "";
    };
    self._write = function(msg) {
      self._contents += msg;
    };
    self.write = function(msg) {
      self.flush();
      self._write(msg);
      self.flush();
    };
    self.flush = function () {
      self.contents = self._contents + self.stdout + self.stderr;
    };
    self.flushForInput = function () {
      self._contents += self.stdout + self.stderr;
      self.stdout = self.stderr = "";
    };
  }])
  // Main controller
  .controller('FrontendController', ['$scope', 'socket', '$q', 'error-service',
    '$modal', 'LoginModal', 'ConfirmationMessageModal', 'cookieStore', '$window',
    'settings-service', '$location',
      function ($scope, ws, $q, errors, $modal, LoginModal, confirm,
        cookieStore, $window, settings, $location) {
        "use strict";
        var self = this;
        self.timeout = false;
        self.disconnected = false;
        self.failed = false;
        self.errors = errors;
        var cookie = cookieStore.get(SEASHELL_CREDS_COOKIE);
        if(cookie) {
          self.host = cookie.host;
        }

        // Help function
        self.help = function () {
          $modal.open({
            templateUrl: "frontend/templates/help-template.html",
            controller: ['$scope', 'ConfirmationMessageModal', '$window',
              'cookieStore',
              function ($scope, confirm, $window, cookies) {
                $scope.login = function () {
                  self.login();
                  $scope.$dismiss();
                };
                $scope.archive = function() {
                  self.archive();
                  $scope.$dismiss();
                };
              }]});
        };
        // confirmation modal for archiving all projects
        self.archive = function() {
          confirm("Archive Projects",
            "Are you sure you want to archive all of your projects? If you do this, you will no longer be able to retrieve them through Seashell, but they will be accessible from your student.cs Linux account.")
            .then(function() {
              $q.when(ws.socket.archiveProjects())
                .then(function() {
                  // look at all these callbacks
                  $location.path("/");
                  $window.location.reload();
                 }).catch(function(err) {
                   self.errors.report(err, "Failed to archive projects.");
                 });
            });
        };
        // Logout
        self.logout = function () {
          confirm("Log out of Seashell",
            "Do you wish to logout?  Any unsaved data will be lost.")
            .then(function () {
              cookieStore.remove(SEASHELL_CREDS_COOKIE);
              $window.top.location = "https://cas.uwaterloo.ca/logout";
            });
        };
        // Settings
        self.settings = function () {
          settings.dialog();
        };
        // Reconnect
        self.reconnect = function () {
          ws.connect();
        };
        // Open login dialog window after disconnection
        self.login = function() {
          new LoginModal().then(function() {
            self.refresh();
          });
        };

        // This won't leak memory, as FrontendController stays in scope all the time.
        ws.register_callback('timein', function () {self.timeout = false;});
        ws.register_callback('timeout', function () {self.timeout = true;});
        ws.register_callback('connected',
            function () {self.disconnected = false; self.timeout = false; self.failed = false;}, true);
        ws.register_callback('disconnected', function () {self.disconnected = true;}, true);
        ws.register_callback('failed', function () {self.failed = true;}, true);
      }])
  // Controller for Project Lists
  .controller('ProjectListController', ['projectList', 'projects',
      'NewProjectModal', 'DeleteProjectModal', 'error-service',
      function (projectList, projects, newProjectModal, deleteProjectModal, errors) {
    var self = this;
    self.projectList = projectList;
    self.state = "list-projects";

    /** Delete onClick handler. */
    self.delete = function(project) {
      deleteProjectModal(project).then(function () {
        self.projectList.refresh();
      });
    };

    /** New Project Handler */
    self.new = function() {
      newProjectModal().then(function () {
        self.projectList.refresh();
      });
    };

    /** Refresh onClick handler. */
    self.refresh = function () {
      self.projectList.refresh();
    };

    // Tests if project is deleteable
    self.isDeletable = function(project) {
      return ! /^[aA][0-9]+/.test(project);
    };
  }])
  // Project controller.
  .controller("ProjectController", ['$state', '$stateParams', '$scope', 'error-service',
      'openProject', 'cookieStore', 'NewQuestionModal', 'DeleteProjectModal',
    function($state, $stateParams, $scope,  errors, openProject, cookies, newQuestionModal, deleteProjectModal) {
      var self = this;
      self.state = 'edit-project';
      self.project = openProject;
      self.userid = cookies.get(SEASHELL_CREDS_COOKIE).user;
      self.is_deleteable = ! /^[aA][0-9]+/.test(self.project.name);
      self.project.prevCol = 0; self.project.prevLine = 0;
      self.project.setNewCol = 0; self.project.setNewLine = 0;
      self.download = function(){
        openProject.getDownloadToken().then(function (token){
            var raw = JSON.stringify(token);
            var url = sprintf("https://%s:%s/export/%s.zip?token=%s",
                              cookies.get(SEASHELL_CREDS_COOKIE).host,
                              cookies.get(SEASHELL_CREDS_COOKIE).port,
                              encodeURIComponent(openProject.name),
                              encodeURIComponent(raw));

            var ifrm = document.createElement("IFRAME");
            ifrm.setAttribute("src", url);
            ifrm.setAttribute("style", "display:none");
            document.body.appendChild(ifrm);
        });};
        self.newQuestion = function () {
          newQuestionModal(openProject);
        };
        self.close = function () {
          $state.go('list-projects');
        };
        self.delete = function () {
          deleteProjectModal(openProject.name).then(
              function () {$state.go('list-projects');});
        };

      self.project.mostRecentlyUsed()
        .then(function (recent) {
          if (recent && $state.is('edit-project')) {
            $state.go('edit-project.editor',
                      {question: recent},
                      {location: "replace"});
          }
          return recent;
        });
    }])
  // Editor Controller
  .controller("EditorController", ['$state', 'openQuestion', '$scope', 'error-service',
      'openProject', 'NewFileModal', 'SubmitMarmosetModal', '$interval', 'marmoset',
      'CommitProjectModal', 'NewQuestionModal', 'MarmosetResultsModal', 'console-service',
      function ($state, openQuestion, $scope, errors,
        openProject, newFileModal, submitMarmosetModal,
        $interval, marmoset, commitProjectModal, newQuestionModal,
        marmosetResultsModal, Console) {
        var self = this;
        self.question = openQuestion;
        self.project = openProject;
        self.common_files = [];
        self.question_files = [];
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
          var result = self.project.filesFor(self.question);
          self.common_files = result.common;
          self.question_files = result.question;
          self.test_files = result.tests;

          self.project.currentMarmosetProject(self.question).then(function(target) {
            if(target) {
              marmoset.results(target).then(function(result) {
                self.handleMarmosetResults(result, target);
              });
            }
          });
        };
        $scope.refresh = self.refresh;

        /** Adds file to the project. */
        self.add_file = function () {
          newFileModal(self.project, self.question, function () {
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

        /** Commits the project. */
        self.commit_project = function () {runWhenSaved(function (){
          commitProjectModal(self.project);
        });};

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

        /** Displays Marmoset Results. */
        self.marmoset_results = function () {
        };
      
        /** Try to load the question, and go back to the project if we can't. */ 
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
      }])
  .controller('EditFileController', ['$state', '$scope', '$timeout', '$q', 'openProject', 'openQuestion',
      'openFolder', 'openFile', 'error-service', 'settings-service', 'console-service', 'RenameFileModal',
      'ConfirmationMessageModal', '$window', '$document', 'hotkeys', 'scrollInfo',
      function($state, $scope, $timeout, $q, openProject, openQuestion, openFolder, openFile, errors,
          settings, Console, renameModal, confirmModal, $window, $document, hotkeys, scrollInfo) {
        var self = this;
        // Scope variable declarations follow.
        self.project = openProject;
        self.question = openQuestion;
        self.folder = openFolder;
        self.file = openFile;
        self.console = Console;
        self.scrollInfo = scrollInfo;
        self.isBinaryFile = false;
        self.ready = false;
        self.ext = self.file.split(".")[1];
        self.editor = null;
        self.timeout = null;
        self.loaded = false;
        self.editorOptions = {}; // Wait until we grab settings to load this.
        self.consoleEditor = null;
        self.settings = settings;
        /* runnerFile is the file to be run when RUN or TEST is clicked. false
         * if the current file is not runnable (and Seashell can't infer which
         * file to run). */
        self.runnerFile = false;
        self.consoleLoad = function(console_cm) {
          self.consoleEditor = console_cm;
          self.consoleEditor.on("change", function() {
            var scr = self.consoleEditor.getScrollInfo();
            self.consoleEditor.scrollTo(scr.left, scr.height);
          });
          onResize();
        };
        self.consoleOptions = {
          lineWrapping: true,
          readOnly: true,
          mode: "text/plain",
          onLoad: self.consoleLoad
        };
        $scope.$on('$destroy', function(){
          var scr = self.editor.getScrollInfo();
          if(undefined===self.scrollInfo[self.folder])
            self.scrollInfo[self.folder] = {};
          self.scrollInfo[self.folder][self.file] =
            {top:scr.top, left:scr.left};
        });
        self.editorFocus = false;
        self.contents = "";
        var mime = {"c" : "text/x-c", "h" : "text/x-c", "rkt" : "text/x-scheme"}[self.ext] || "text/plain";
        // Saving event.
        function runWhenSaved(fn) {
          if (self.timeout) {
            $timeout.cancel(self.timeout);
            self.timeout = null;
            self.project.saveFile(self.question, self.folder, self.file, self.contents).then(function (){
                fn();
              })
              .catch(function (error) {
                errors.report(error, "Could not save file!");
              });
          } else {
            fn();
          }
        }
        $scope.$on('run-when-saved', function (evt, fn) {
          runWhenSaved(fn);
        });
        self.activateResize = function(){
          settings.settings.force_narrow = !(settings.settings.force_narrow);
          settings.save();
          onResize();
        };
        //Resize on window size change
        function onResize() {
          var narrow = (settings.settings.force_narrow || $($document).width() < 992);
          var min_height = 500, margin_bottom = 30;
          var min_y_element = $('#editor > .CodeMirror');
          var h = Math.max($($window).height() - (min_y_element.offset().top - $($window).scrollTop()) - margin_bottom,
                           min_height);
                   $('#editor > .CodeMirror')
            .height(Math.floor(narrow ? h * 0.7 : h) - $('#current-file-controls').outerHeight()); 
          $('#console > .CodeMirror')
            .height((narrow ? (h * 0.3 - $('#console-title').outerHeight()) : 1 + h) - $('#console-input').outerHeight());
          if(self.editor)
            self.editor.refresh();
        }
        $scope.$on('window-resized', onResize);
        // Scope helper function follow.
        self.editorLoad = function(editor) {
          self.editor = editor;
          if (self.ext === "c" || self.ext==="h") {
            CodeMirror.registerHelper("lint","clike",function() {
              var found = [];
              _.forEach(self.console.errors,function(err) {
                var error = err[0], file = err[1].split("/");
                file = file[file.length-1];
                var line = _.max([err[2] - 1, 0]), column = err[3];
                var message = err[4];
                console.log(err);
                if (_.contains([self.file,
                                'final-link-result'],
                               file))
                  found.push({ from: CodeMirror.Pos(line, column),
                               to: CodeMirror.Pos(line),
                               message: message,
                               severity: 'error' });
              });
              return found;
            });
            self.editor.setOption("gutters", ["CodeMirror-lint-markers"]);
            self.editor.setOption("lint", true);
          }
          
          self.editor.on("change", function() {
            if(self.ready && self.timeout) {
              $timeout.cancel(self.timeout);
              self.timeout = null;
            }
            if (self.loaded) {
              self.timeout = $timeout(function() {
                self.project.saveFile(self.question, self.folder, self.file, self.contents)
                  .catch(function (error) {
                    errors.report(error, "Could not save file!");
                  })
                  .then(function () {
                    self.timeout = null;
                  });
              }, 2000);
              self.console.errors = [];
            } else {
              self.editor.clearHistory();
              self.editor.setCursor(self.project.setNewLine - 1, self.project.setNewCol - 1);
              var viewH = self.editor.getScrollInfo().clientHeight;
              var curH = self.editor.cursorCoords().top;
              self.editor.scrollTo(0, curH  - viewH);
            }
            self.loaded = true;
          });
          function updateColNums() {
            $timeout(function() {
              self.col = self.editor.getCursor().ch + 1;
              self.line = self.editor.getCursor().line + 1;
            }, 0);
          }
          self.editor.on("cursorActivity", updateColNums);
          self.editor.on("focus", updateColNums);
          self.editor.on("blur", updateColNums);
          onResize();
        };
        self.refreshSettings = function () {
          self.editorOptions = {
            autofocus: true,
            lineWrapping: true,
            lineNumbers: !self.isBinaryFile,
            readOnly: !self.ready || self.isBinaryFile,
            mode: mime,
            theme: settings.settings.text_style,
            tabSize: parseInt(settings.settings.tab_width),
            indentUnit: parseInt(settings.settings.tab_width),
            onLoad: self.editorLoad,
            matchBrackets: true,
            rulers: [80],
            extraKeys: {
              "Ctrl-Enter": function() {
                self.editor.setOption('fullScreen', !self.editor.getOption('fullScreen'));
              },
              "Ctrl-I": self.indentAll,
              "Esc": function() {
                if(self.editor.getOption('fullScreen')) self.editor.setOption('fullScreen', false);
              },
              // capture save shortcuts and ignore in the editor
              "Ctrl-S": function() { },
              "Cmd-S": function() { },
            }
          };
          var main_hotkeys = [{
            combo: 'ctrl+d',
            description: 'Sends EOF',
            callback: function(evt) {
              evt.preventDefault();
              self.sendEOF();
            }
          }, {
            combo: 'ctrl+k',
            description: "Kills the currently running program.",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: function (evt) {
              evt.preventDefault();
              self.killProgram();
            }
          }];
          var vim_disabled_hotkeys = [{
            combo: 'ctrl+r',
            description: "Runs the program",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: function (evt) {
              evt.preventDefault();
              self.runFile();
            }
          }, {
            combo: 'ctrl+u',
            description: "Starts Tests",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: function (evt) {
              evt.preventDefault();
              self.testFile();
            }
          }];

          if(settings.settings.editor_mode !== 'vim') {
            _.each(vim_disabled_hotkeys, function(hk) {
              hotkeys.bindTo($scope.$parent).add(hk);
            });
          }
          else {
            _.each(vim_disabled_hotkeys, function(hk) {
              hotkeys.del(hk.combo);
            });
          }
          _.each(main_hotkeys, function(hk) {
            hotkeys.bindTo($scope.$parent).add(hk);
          });

          if (settings.settings.editor_mode === 'vim') {
            self.editorOptions.vimMode = true;
          } else if(settings.settings.editor_mode === 'emacs') {
            self.editorOptions.keyMap = 'emacs';
            self.editorOptions.vimMode = false;
          } else {
            self.editorOptions.keyMap = 'default';
            self.editorOptions.vimMode = false;
          }
          
          if (self.editorOptions.vimMode) {
            delete self.editorOptions.extraKeys.Esc;
          }
          
          // Force the font size at any rate.
          $('.CodeMirror').css('font-size', sprintf("%dpt", parseInt(settings.settings.font_size)));
          // If the CodeMirror has been loaded, add it to the editor.
          if (self.editor) {
            for (var key in self.editorOptions) {
              self.editor.setOption(key, self.editorOptions[key]);
            }
            self.editor.addKeyMap({'Tab': 'insertSoftTab'});
            self.editor.refresh();
          }
        };
        self.renameFile = function() {
          renameModal(self.project, self.question, self.folder, self.file, function(newName) {
            $scope.$parent.refresh();
            $state.go("edit-project.editor.file", {part:self.folder, file:newName});
          });
        };

        self.deleteFile = function() {
          confirmModal("Delete File", "Are you sure you want to delete '"+self.file+"'?")
            .then(function() {
              self.project.deleteFile(self.question, self.folder, self.file)
                .then(function() {
                  $scope.$parent.refresh();
                  $state.go("edit-project.editor");
                });
            });
        };

        function handleCompileErr(msgs, warn_only) {
          if(msgs.length === 0) return;
          else if(!warn_only)
            self.console.write("Compilation failed with errors:\n");
          else
            self.console.write("Compilation generated warnings:\n");
          self.console.errors = msgs;
          if(self.ext=="h"||self.ext=="c") {
            self.editor.setOption("lint", false);
            self.editor.setOption("lint", true);
          }
          _.each(msgs, function(res) {
            self.console.write(sprintf("%s:%d:%d: %s\n", res[1], res[2], res[3], res[4]));
          });
        }

        self.runFile = function() {runWhenSaved(function () {
          self.killProgram().then(function() {
            self.console.clear();
            self.project.run(self.question, "question", self.runnerFile, self.contents, false)
              .then(function(res) {
                $scope.$broadcast('program-running');
                self.console.setRunning(self.project, [res.pid], false);
                handleCompileErr(res.messages, true);
                self.console.write("Running '"+self.project.name+"/"+self.question+"':\n");
              })
              .catch(function(res) {
                if(res.status === "compile-failed") {
                  handleCompileErr(res.messages);
                } else {
                  errors.report(res, "An error occurred when running the project.");
                }
              });
          }).catch(function (error) {
            errors.report(error, "Could not kill program!");
          });
        });};

        self.testFile = function() {runWhenSaved(function () {
          self.killProgram().then(function() {
            self.console.clear();
            self.project.run(self.question, "question", self.runnerFile, self.contents, true)
              .then(function(res) {
                self.console.setRunning(self.project, res.pids, true);
                handleCompileErr(res.messages, true);
                self.console.write("Running tests for '"+self.project.name+"/"+self.question+"':\n");
              })
              .catch(function(res) {
                if(res.status === "compile-failed") {
                  handleCompileErr(res.messages);
                } else {
                  errors.report(res, "An error occurred when running the project.");
                }
              });
          }).catch(function (error) {
            errors.report(error, "Could not kill program!");
          });
        });};

        self.killProgram = function() {
          if(!self.console.PIDs) {
            return $q.when();
          }
          return $q.all(_.map(self.console.PIDs, function(id) {
            return self.project.kill(id);
          }))
          .catch(function (error) {
            errors.report(error, "Could not stop program!");
            self.console.PIDs = null;
            self.console.running = false;
          });
        };

        self.indentAll = function() {
          var lineCount = self.editor.lineCount();
          for (var i = 0; i < lineCount; i++)
            self.editor.indentLine(i);
        };

        self.userInput = "";
        self.sendInput = function($event) {
          if($event.keyCode == 13) {
            if(self.console.running) {
              self.project.sendInput(self.console.PIDs[0], self.userInput + "\n");
              self.console.flushForInput();
              self.console.write(self.userInput + "\n");
              self.userInput = "";
            }
          }
        };

        self.clearConsole = function () {
          self.console.clear();
        };

        self.sendEOF = function() {
          if(self.console.running) {
            self.project.sendEOF(self.console.PIDs[0]).then(function () {
              self.console.running = false;
              self.userInput = "";
            });
          }
        };

        // Initialization code goes here.
        var key = settings.addWatcher(function () {self.refreshSettings();}, true);
        $scope.$on("$destroy", function() {
          if (self.timeout && self.ready) {
            $timeout.cancel(self.timeout);
            self.project.saveFile(self.question, self.folder, self.file, self.contents);
          }
          settings.removeWatcher(key);
        });
        self.project.openFile(self.question, self.folder, self.file)
          .then(function(conts) {
            self.contents = conts;
            self.ready = true;
            if (conts.length === 0) self.loaded = true;
            self.refreshSettings();
            $timeout(function() {
              if(self.scrollInfo[self.folder]!==undefined &&
                self.scrollInfo[self.folder][self.file]!==undefined) {
                var scr = self.scrollInfo[self.folder][self.file];
                self.editor.scrollTo(scr.left, scr.top);
              }
            }, 0);
            self.project.updateMostRecentlyUsed(self.question, self.folder, self.file);
          }).catch(function (error) {
            if (error.indexOf("bytes->string/utf-8: string is not a well-formed UTF-8 encoding") != -1)
              self.isBinaryFile = true;
            else {
              errors.report(error, sprintf("Unexpected error while reading file %s!", self.file));
              $state.go('edit-project.editor');
            }
            self.refreshSettings();
          });

        // true iff the given file has the given extension
        function has_ext(ext, fname){
          return fname.split(".").pop() === ext;
        }

        /* the following code updates which file (if any) will be run with RUN/TEST is clicked */
        var qfiles = self.project.filesFor(self.question).question;
        var rktFiles = _.filter(qfiles, _.partial(has_ext, "rkt"));

        // the below variables represent the precedence of rules for which file gets run
        var openFileIsRkt = has_ext("rkt", openFile) ? openFile : false;
        var anyCFile = _.find(qfiles, _.partial(has_ext, "c"));
        var uniqueRktFile = rktFiles.length === 1 ? rktFiles[0] : false;
        self.runnerFile = openFileIsRkt || anyCFile || uniqueRktFile;
      }])
  .config(['hotkeysProvider', function(hotkeysProvider) {
    hotkeysProvider.includeCheatSheet = false;
  }])
  // Configuration for routes
  .config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {
    $urlRouterProvider.otherwise('/');
    $stateProvider
      .state("list-projects", {
        url: "/",
        templateUrl: "frontend/templates/project-list-template.html",
        controller: "ProjectListController as projects",
        resolve: {projectList: ['$q', 'projects', 'error-service', 'socket', function ($q, projects, errors, ws) {
            return new function () {
              var self = this;
              self.list = [];
              self.question_list = {};
              /** Run this every time the state associated with this controller is loaded.
               *  Returns a deferred that resolves when the state is properly loaded */
              self.refresh = function () {
                return projects.fetch().catch(function (projects) {
                  var type = projects.error.indexOf("503")===-1 ? "seashell" : "webserver";
                  errors.report(projects, 'Could not fetch projects.', type);
                }).then(function () {
                  return projects.list().then(function (projects_list) {
                    var new_question_list = {};

                    return $q.when(_.map(projects_list, function (project) {
                      return projects.open(project, 'none').then(function (project_object) {
                        var questions = project_object.questions();
                        new_question_list[project] = questions;
                      });
                    })).then(function () {
                      self.list = projects_list;
                      self.question_list = new_question_list;
                    });
                  }).catch(function (error) {
                    errors.report(error, "Could not generate list of projects.");
                  });
                });
              };
              /** Store the key into our callback [this is important, as a new object
               *  is created every time into this state, and we'll have to remove the CB
               *  as to not leave a dangling reference]. 
               *
               *  Great manual memory management in JavaScript.
               */
              var cb_key = ws.register_callback('connected', function () {self.refresh();}, true);
              self.destroy = function () {
                ws.unregister_callback(cb_key);
              };
            }();
          }]},
        onExit: ['projectList', function (projectList) {
          projectList.destroy();
        }]
        })
      .state("edit-project", {
        url: "/project/{project}",
        templateUrl: "frontend/templates/project-template.html",
        controller: "ProjectController as projectView",
        resolve: {openProject: ['projects', '$state', '$stateParams', 'error-service',
                                'ConfirmationMessageModal', function(projects, $state, $stateParams, errors, confirm) {
                                   return projects.open($stateParams.project).catch(function (error) {
                                     if (error === 'locked') {
                                       return confirm(sprintf('Unlock %s', $stateParams.project),
                                                      sprintf('Project %s is open in another browser.  Proceed?', $stateParams.project))
                                         .then(function () {
                                           return projects.open($stateParams.project, 'force-lock')
                                                          .catch(function (error) {
                                                             $state.go('list-projects');
                                                             errors.report(error, sprintf("Could not open project %s!", $stateParams.project));
                                                             return null;
                                                            });
                                           })
                                          .catch(function () {
                                            $state.go('list-projects');
                                          });
                                       } else {
                                         $state.go('list-projects');
                                         errors.report(error, sprintf("Could not open project %s!", $stateParams.project));
                                         return null;
                                       }
                                     });
                                   }]},
          onExit: ['openProject', function (project) {
            project.close();
          }]
        })
      .state("edit-project.editor", {
        url: "/edit/{question}",
        templateUrl: "frontend/templates/project-editor-template.html",
        controller: "EditorController as editView",
        resolve: {openQuestion: ['$stateParams', function($stateParams) {
          return $stateParams.question;
        }],
          scrollInfo: function() { return {}; }
        }
      })
      .state("edit-project.editor.file", {
        url: "/file/{part}/{file}",
        templateUrl: "frontend/templates/project-editor-editview-template.html",
        controller: "EditFileController as editFileView",
        resolve: {openFile: ['$stateParams', function($stateParams) {
            return $stateParams.file;
          }],
          openFolder: ['$stateParams', function($stateParams) {
            return $stateParams.part;
          }]}
      });
  }])
  .run(['cookie', 'socket', 'settings-service', 'error-service', 'projects', 
        '$window', '$document', '$rootScope',
        function(cookies, ws, settings, errors, projects, $window, $document, $rootScope) {
    ws.connect()
        .then(function () {
        });
    // Reload settings on (re)connect.
    ws.register_callback('connected', function () {
      return settings.load().catch(function (error) {
        errors.report(error, 'Could not load settings!');
      });
    });
    // Set up resize
    $($window).resize(function () {
      $rootScope.$broadcast('window-resized');
    });
  }]);
