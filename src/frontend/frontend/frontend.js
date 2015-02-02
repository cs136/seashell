/**
 * Seashell's frontend.
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
angular.module('frontend-app', ['seashell-websocket', 'seashell-projects', 'jquery-cookie', 'ui.router',
    'ui.bootstrap', 'ui.codemirror'])
  // Error service.
  .service('error-service', function () {
    var self = this;
    self.errors = [];

    self.report = function (error, shorthand) {
      if (error) {
        self.errors.push({shorthand: shorthand, error: error});
      }
    };
    self.suppress = function (index) {
      self.errors.splice(index, 1);
    };
  })
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
            controller: ['$scope', '$state', 'error-service', '$q',
            function ($scope, $state, errors, $q) {
              $scope.new_file_name = "";
              $scope.new_file_folder = question;
              $scope.new_file_upload = [];
              $scope.question = question;
              $scope.inputError = false;
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
                      project.createFile($scope.new_file_folder, question, filename, reader.result, "url")
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
  // Submit to Marmoset Modal
  .factory('SubmitMarmosetModal', ['$modal', 'error-service',
      function ($modal, errors) {
        return function (project, question, notify) {
          notify = notify || function () {};
          return $modal.open({
            templateUrl: "frontend/templates/marmoset-submit-template.html",
            controller: ['$scope', '$state', 'error-service', '$q', 'marmoset',
            function ($scope, $state, errors, $q, marmoset) {
              $scope.marmoset_projects = marmoset.projects();
              $scope.selected_project = project.currentMarmosetProject(question) || undefined;
              $scope.submit = function() {
                $scope.$close();
                project.submit(question, $scope.selected_project)
                       .catch(function (error) {
                         errors.report(error, sprintf("Could not submit project %s!", $scope.selected_project));
                         notify(false, $scope.selected_project);
                       }).then(function () {
                         notify(true, $scope.selected_project);
                       });
              };
            }]}).result;
        };
      }])
  // Marmoset Results Modal
  .factory('MarmosetResultModal', ['$modal', 'error-service',
      function ($modal, errors) {
        return function (target) {
          return $modal.open({
            templateUrl: "frontend/templates/marmoset-results-template.html",
            controller: ['$scope', '$state', 'error-service', 'marmoset',
              function ($scope, $state, errors, marmoset) {
              }]});
        };
      }])
  // Settings service.
  .service('settings-service', ['$rootScope', '$modal', 'socket', 'error-service', '$q',
      function ($rootScope, $modal, ws, errors, $q) {
        var self = this;
        self.settings =  {
          font_size  : 10,
          edit_mode  : "standard",
          tab_width  : 2,
          text_style : "neat",
          use_space : true
        };
        self.notify = {};
        var nKey = 0;

        function notifyChanges () {
          _.forEach(self.notify, function (fn) {fn();});
        }

        /** Adds and removes watchers on the settings service. */
        self.addWatcher = function(fn, invoke) {
          self.notify[nKey] = fn;
          invoke && fn ();
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
    self.inst = null;
    self.contents = "";
    // buffers
    self.stdout = "";
    self.stderr = "";

    socket.register_callback("io", function(io) {
      if(io.type == "stdout") {
        var ind = io.message.indexOf("\n");
        if(ind > -1) {
          var spl = io.message.split("\n");
          self.write(self.stdout);
          while(spl.length>1) { self.write(spl.shift() + "\n"); }
          self.stdout = spl[0];
        }
        else
          self.stdout += io.message;
      }
      else if(io.type == "stderr") {
        var ind = io.message.indexOf("\n");
        if(ind > -1) {
          var spl = io.message.split("\n");
          self.write(self.stderr);
          while(spl.length>1) { self.write(spl.shift() + "\n"); }
          self.stderr = spl[0];
        }
        else
          self.stderr += io.message;
      }
      else if(io.type == "done") {
        self.write("Program finished with exit code "+io.status+".\n");
        self.PIDs = null;
      }
    });
    socket.register_callback("test", function(res) {
      self.PIDs = _.without(self.PIDs, res.pid);
      if(res.result=="passed") {
        self.write("Test '"+res.test_name+"' passed.\n");
      }
      else if(res.result=="failed") {
        self.write("Test '"+res.test_name+"' failed!\n");
      }
    });

    self.setRunning = function(PIDs) {
      self.PIDs = PIDs;
    };
    self.clear = function() {
      self.contents = "";
    };
    self.write = function(msg) {
      self.contents += msg;
    };
  }])
  // Main controller
  .controller('FrontendController', ['$scope', 'socket', '$q', 'error-service', '$modal', 'ConfirmationMessageModal', 'cookieStore', '$window', 'settings-service',
      function ($scope, ws, $q, errors, $modal, confirm, cookieStore, $window, settings) {
        "use strict";
        var self = this;
        self.timeout = false;
        self.disconnected = false;
        self.failed = false;
        self.errors = errors;

        // Help function
        self.help = function () {
          $modal.open({templateUrl: "frontend/templates/help-template.html"});
        };
        // Logout
        self.logout = function () {
          confirm("Log out of Seashell",
            "Do you wish to logout?  Any unsaved data will be lost.")
            .then(function () {
              cookieStore.remove("seashell-session");
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

    /** Fetch onClick handler. */
    self.fetch = function () {
      return projects.fetch().catch(function (projects) {
        errors.report(projects, 'Could not fetch projects.');
      }).then(function () {
        self.projectList.refresh();
      });
    };

    // Tests if project is deleteable
    self.isDeletable = function(project) {
      return ! /^[aA][0-9]+/.test(project);
    };
  }])
  // Project controller.
  .controller("ProjectController", ['$state', '$stateParams', '$scope', 'error-service',
      'openProject', 'cookieStore',
    function($state, $stateParams, $scope,  errors, openProject, cookies) {
      var self = this;
      self.state = 'edit-project';
      self.project = openProject;
      self.userid = cookies.get('seashell-session').user;
      self.is_deleteable = ! /^[aA][0-9]+/.test(self.project.name);
    }])
  // Editor Controller
  .controller("EditorController", ['$state', 'openQuestion', '$scope', 'error-service',
      'openProject', 'NewFileModal', 'SubmitMarmosetModal', '$interval', 'marmoset',
      'CommitProjectModal',
      function ($state, openQuestion, $scope, errors,
        openProject, newFileModal, submitMarmosetModal,
        $interval, marmoset, commitProjectModal) {
        var self = this;
        self.question = openQuestion;
        self.project = openProject;
        self.common_files = [];
        self.question_files = [];
        self.test_files = [];
        self.marmoset_short_results = null;
        self.marmoset_refresh_interval = undefined;
        self.marmoset_timeout = 1000;

        // Destroy interval when scope goes away.
        function cancelMarmosetRefresh() {
          return self.marmoset_refresh_interval &&
            $interval.cancel(self.marmoset_refresh_interval);
        }
        $scope.$on('$destroy', cancelMarmosetRefresh);
       
        /** Refreshes the controller [list of files, ...] */ 
        self.refresh = function () {
          var result = self.project.filesFor(self.question);
          self.common_files = result.common;
          self.question_files = result.question;
          self.test_files = result.tests;
        };
        $scope.refresh = self.refresh;

        /** Adds file to the project. */
        self.add_file = function () {
          newFileModal(self.project, self.question, function () {
            self.refresh();
          });
        };

        /** Register an event that will run when
         *  the current file has been saved.
         */
        $scope.$on('file-saved', function (event, fn) {
          fn ();
        }); 

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

        /** Submits the current question. */
        self.submit_question = function (){runWhenSaved(function(){
          submitMarmosetModal(self.project, self.question, function (success, target) {
            if (!success) {
              self.marmoset_short_results = "failed to submit!";
            } else {
              var submitTime = new Date();
              cancelMarmosetRefresh();
              self.marmoset_refresh_interval = $interval(function () {
                marmoset.results(self.project.name).then(function (result) {
                  if (result.error) {
                    self.marmoset_short_results = "errored!";
                    errors.report(result.result, sprintf("Unknown Marmoset Error submitting for %s", target));
                  } else {
                    cancelMarmosetRefresh();
                    var data = result.result;

                    if (data.length > 0 && data[0].status == "complete") {
                      var sub_pk = data[0].submission;
                      var related = _.filter(data, function (entry) {
                        return entry.submission === sub_pk;
                      });
                      var total = 0, passed = 0;
                      for (var i = 0; i < related.length; i++) {
                        total += related[i].points;
                        total_passed += data[i].outcome === "passed" ? data[i].points : 0;
                      }
                      
                      self.marmoset_short_results = 
                        sprintf("%s (%d/%d)", total_passed === total ? "passed" : "failed",
                                total_passed, total);
                    } else if (data.length > 0) {
                        sprintf("received %s (waiting on tests)",
                                $.timeago(data[0].timestamp));
                          
                    } else {
                      self.marmoset_short_results = 
                        sprintf("submitted %s (waiting on receipt)",
                                $.timeago(submitTime));
                    }
                  }
                }).catch(function (error) {
                  errors.report(error, sprintf("Could not fetch results for %s!", target));
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
        } catch (e) {
          errors.report({}, sprintf("Could not open question %s!", self.question));
          $state.go("edit-project");
        }
      }])
  .controller('EditFileController', ['$state', '$scope', '$timeout', '$q', 'openProject', 'openQuestion',
      'openFolder', 'openFile', 'error-service', 'settings-service', 'console-service', 'RenameFileModal',
      'ConfirmationMessageModal',
      function($state, $scope, $timeout, $q, openProject, openQuestion, openFolder, openFile, errors,
          settings, Console, renameModal, confirmModal) {
        var self = this;
        // Scope variable declarations follow.
        self.project = openProject;
        self.question = openQuestion;
        self.folder = openFolder;
        self.file = openFile;
        self.console = Console;
        self.isBinaryFile = false;
        self.ready = false;
        self.ext = self.file.split(".")[1];
        self.editor = null;
        self.timeout = null;
        self.editorOptions = {}; // Wait until we grab settings to load this.
        self.consoleLoad = function(console_cm) {
          self.console.inst = console_cm;
          self.console.inst.on("update", function() {
            var scr = self.console.inst.getScrollInfo();
            self.console.inst.scrollTo(scr.left, scr.height);
          });
        };
        self.consoleOptions = {
          lineWrapping: true,
          readOnly: true,
          mode: "text/plain",
          onLoad: self.consoleLoad
        };
        self.contents = "";
        var mime = {"c" : "text/x-c", "h" : "text/x-c", "rkt" : "text/x-scheme"}[self.ext] || "text/plain";
        // Saving event.
        $scope.$on('run-when-saved', function (evt, fn) {
          if (self.timeout) {
            $timeout.cancel(self.timeout);
            self.timeout = null;
            self.project.saveFile(self.question, self.folder, self.file, self.contents).then(function (){
                $scope.$emit('file-saved', fn);
              })
              .catch(function (error) {
                errors.report(error, "Could not save file!");
              });
          } else {
            $scope.$emit('file-saved', fn);
          }
        });

        // Scope helper function follow.
        self.editorLoad = function(editor) {
          self.editor = editor;
          self.editor.on("change", function() {
            if(self.ready && self.timeout) {
              $timeout.cancel(self.timeout);
              self.timeout = null;
            }
            self.timeout = $timeout(function() {
              self.project.saveFile(self.question, self.folder, self.file, self.contents)
                .catch(function (error) {
                  errors.report(error, "Could not save file!");
                })
                .then(function () {
                  self.timeout = null;
                });
            }, 2000);
          self.refreshSettings();
          });
        };
        self.refreshSettings = function () {
          self.editorOptions = {
            lineWrapping: true,
            lineNumbers: !self.isBinaryFile,
            readOnly: !self.ready || self.isBinaryFile,
            mode: mime,
            theme: settings.settings['text_style'],
            tabSize: parseInt(settings.settings['tab_width']),
            indentUnit: parseInt(settings.settings['tab_width']),
            onLoad: self.editorLoad
          };
          if (settings.settings['edit_mode'] === 'vim') {
            self.editorOptions['vim_mode'] = true;
          } else if(settings.settings['edit_mode'] === 'emacs') {
            self.editorOptions['keyMap'] = 'emacs';
            self.editorOptions['vim_mode'] = false;
          } else {
            self.editorOptions['keyMap'] = 'default';
            self.editorOptions['vim_mode'] = false;
          }

          // If the CodeMirror has been loaded, add it to the editor.
          if (self.editor) {
            for (var key in self.editorOptions) {
              self.editor.setOption(key, self.editorOptions[key]);
            }
            self.editor.addKeyMap({'Tab': 'insertSoftTab'});
          }
          // Force the font size at any rate.
          $('.CodeMirror').css('font-size', sprintf("%dpt", parseInt(settings.settings.font_size)));
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
          self.console.clear();
          if(msgs.length==0) return;
          if(!warn_only)
            self.console.write("Compilation failed with errors:\n");
          else
            self.console.write("Compilation generated warnings:\n");
          _.each(msgs, function(res) {
            self.console.write(sprintf("%s:%d:%d: %s\n", res[1], res[2], res[3], res[4]));
          });
        }

        self.runFile = function() {
          self.killProgram().then(function() {
            self.project.run(self.question, self.folder, self.file, self.contents, false)
              .then(function(res) {
                self.console.setRunning([res.pid]);
                self.console.clear();
                handleCompileErr(res.messages, true);
                self.console.write("Running '"+self.project.name+"/"+self.question+"':\n");
              })
              .catch(function(res) {
                if(res.error)
                  errors.report(res.error, "An error occurred when running the project.");
                handleCompileErr(res.messages);
              });
          });
        };

        self.testFile = function() {
          self.killProgram().then(function() {
            self.project.run(self.question, self.folder, self.file, self.contents, true)
              .then(function(res) {
                self.console.setRunning(res.pids);
                self.console.clear();
                handleCompileErr(res.messages, true);
                self.console.write("Running tests for '"+self.project.name+"/"+self.question+"':\n");
              })
              .catch(function(res) {
                if(res.error)
                  errors.report(res.error, "An error occurred when running the project.");
                handleCompileErr(res.messages);
              });
          });
        };

        self.killProgram = function() {
          if(!self.console.PIDs) {
            return $q.when();
          }
          return $q.all(_.map(self.console.PIDs, function(id) {
            return self.project.kill(id);
          })).then(function() {
            self.console.PIDs = null;
          });
        };

        self.userInput = "";
        self.sendInput = function($event) {
          if($event.keyCode == 13) {
            if(self.console.PID && !isNaN(self.console.PID)) {
              self.project.sendInput(self.console.PID, self.userInput);
              self.userInput = "";
            }
          }
        };

        self.sendEOF = function() {
          if(self.console.PID && !isNaN(self.console.PID)) {
            self.project.sendEOF(self.console.PID);
          }
        };

        // Initialization code goes here.
        var key = settings.addWatcher(function () {self.refreshSettings();}, true);
        $scope.$on("$destroy", function() {
          if (self.timeout)
            $timeout.cancel(self.timeout);
          if (self.ready)
            self.project.saveFile(self.question, self.folder, self.file, self.contents);
          settings.removeWatcher(key);
        });
        self.project.openFile(self.question, self.folder, self.file)
          .then(function(conts) {
            self.contents = conts;
            self.ready = true;
            self.refreshSettings();
          }).catch(function (error) {
            if (error.indexOf("bytes->string/utf-8: string is not a well-formed UTF-8 encoding") != -1)
              self.isBinaryFile = true;
            else {
              errors.report(error, sprintf("Unexpected error while reading file %s!", self.file));
              $state.go('edit-project.editor');
            }
            self.refreshSettings();
          });
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
              self.question_list = [];
              /** Run this every time the state associated with this controller is loaded.
               *  Returns a deferred that resolves when the state is properly loaded */
              self.refresh = function () {
                return projects.list().then(function (projects_list) {
                  self.list = projects_list;

                  return $q.when(_.map(projects_list, function (project) {
                    return projects.open(project, 'none').then(function (project_object) {
                      var questions = project_object.questions();
                      self.question_list[project] = questions;
                    });
                  }));
                }).catch(function (error) {
                  errors.report(error, "Could not generate list of projects.");
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
        resolve: {openProject: ['projects', '$state', '$stateParams', 'error-service', function(projects, $state, $stateParams, errors) {
          return projects.open($stateParams.project).catch(function (error) {
            errors.report(error, sprintf("Could not open project %s!", $stateParams.project));
            $state.go('list-projects');
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
        }]}
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
  .run(['cookie', 'socket', 'settings-service', 'error-service', 'projects', function(cookies, ws, settings, errors, projects) {
    ws.connect()
        .then(function () {
          return projects.fetch().catch(function (projects) {
            errors.report(projects, 'Could not fetch projects.');
          });
        });
    // Reload settings on (re)connect.
    ws.register_callback('connected', function () {
      return settings.load().catch(function (error) {
        errors.report(error, 'Could not load settings!');
      });
    });
  }]);
