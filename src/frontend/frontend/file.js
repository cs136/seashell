/**
 * Seashell's frontend edit file controller
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

var SEASHELL_READONLY_STRING = "SEASHELL_READONLY";

/* jshint supernew: true */
angular.module('frontend-app')
  .controller('EditFileController', ['$state', '$scope', '$timeout', '$q', 'openProject', 'openQuestion',
      'openFolder', 'openFile', 'error-service', 'settings-service', 'console-service', 'RenameFileModal',
      'ConfirmationMessageModal', '$window', '$document', 'hotkeys', 'scrollInfo', 'undoHistory', 'socket', 'CopyFileModal',
      function($state, $scope, $timeout, $q, openProject, openQuestion, openFolder, openFile, errors,
          settings, Console, renameModal, confirmModal, $window, $document, hotkeys, scrollInfo, undoHistory,
          ws, copyModal) {
        var self = this;
        // Scope variable declarations follow.

        // These are all arguments passed into the controller,
        // and persist after the controller is destroyed.
        self.project = openProject;
        self.question = openQuestion;
        self.folder = openFolder;
        self.file = openFile;
        self.console = Console;
        self.settings = settings;

        // Instance fields.
        self.scrollInfo = scrollInfo;
        self.isBinaryFile = false;
        self.unavailable = false;
        self.ready = false;
        self.ext = self.file.split(".")[1];
        self.runnerFile = false; // true if a runner file is present in the project
        self.isFileToRun = false; // true if the current file is the runner file
        self.editor = null;
        self.timeout = null;
        self.loaded = false;
        self.editorOptions = {}; // Wait until we grab settings to load this.
        self.consoleEditor = null;
        self.consoleOptions = {};
        self.editorReadOnly = true; // We start out read only until contents are loaded.
        self.fileReadOnly = false;

        /** Callback key when connected.
         *  NOTE: This is slightly sketchy -- however, as
         *  the editor will only be loaded if and only if
         *  the socket exists in the first place, this is
         *  fine for now. */
        var cbC_key = ws.register_callback('connected', function () {
          if (self.editor)
            self.editor.setOption("readOnly", self.editorReadOnly);
        }, true);
        var cbF_key = ws.register_callback('failed', function () {
          if (self.editor)
            self.editor.setOption("readOnly", true);
        }, true);
        var cbD_key = ws.register_callback('disconnected', function () {
          if (self.editor)
            self.editor.setOption("readOnly", true);
        }, true);

        $scope.$on('$destroy', function(){
          var scr = self.editor.getScrollInfo();
          if(undefined===self.scrollInfo[self.folder])
            self.scrollInfo[self.folder] = {};
          self.scrollInfo[self.folder][self.file] =
            {top:scr.top, left:scr.left, line:self.line, col:self.col};
        });
        self.editorFocus = false;
        self.contents = "";
        var mime = {"c" : "text/x-c", "h" : "text/x-c", "rkt" : "text/x-scheme"}[self.ext] || "text/plain";
        // Saving event.
        function runWhenSaved(fn) {
          if (self.timeout) {
            $timeout.cancel(self.timeout);
            self.timeout = null;
            self.undoHistory = self.editor.getHistory();
            self.project.saveFile(self.question, self.folder, self.file, self.contents, JSON.stringify(self.undoHistory)).then(function (){
                fn();
              })
              .catch(function (error) {
                errors.report(error, "Could not save file!");
              });
          } else {
            fn();
          }
        }

        (function initResizableUI() {
          var consoleDOM = $("#console");
          var editorDOM = $("#editor");
          var containerDOM = $("#editor-console-container");
          var w = containerDOM.width();
          var h = editorDOM.height();
          var minEditorW = 470;
          var minConsoleW = 220;
          editorDOM.resizable({
            handles: "e",
            resize: function(event, ui) {
              editorDOM.css({
                width: ui.size.width
              });
              consoleDOM.css({
                width: w - ui.size.width
              });
              self.editor.refresh();
              self.consoleEditor.refresh();
              event.stopPropagation();
            }
          });
          function resizeToFit() {
            w = containerDOM.width();
            h = editorDOM.height();
            if (minEditorW + minConsoleW < w) {
              editorDOM.resizable("option", "minWidth", minEditorW);
              editorDOM.resizable("option", "maxWidth", w - minConsoleW);
            } else {
              editorDOM.resizable("option", "minWidth", 50);
              editorDOM.resizable("option", "maxWidth", w - 50);
            }
            editorDOM.resizable("option", "minHeight", h);
            editorDOM.resizable("option", "maxHeight", h);
            editorDOM.css({
              width: w / 2
            });
            consoleDOM.css({
              width: w / 2
            });
            if (self.editor) {self.editor.refresh();}
            if (self.consoleEditor) {self.consoleEditor.refresh();}
          }
          resizeToFit();
          window.onresize = resizeToFit;
        })();
        
        $scope.$on('run-when-saved', function (evt, fn) {
          runWhenSaved(fn);
        });

        self.onResize = function() {
          settings.settings.force_narrow = !(settings.settings.force_narrow);
          settings.save();
        };

        // Scope helper function follow.
        self.consoleLoad = function(console_cm) {
          self.consoleEditor = console_cm;
          self.consoleEditor.on("change", function() {
            var scr = self.consoleEditor.getScrollInfo();
            self.consoleEditor.scrollTo(scr.left, scr.height);
          });
        };

        self.editorLoad = function(editor) {
          self.editor = editor;
          if (self.ext === "c" || self.ext === "h") {
            CodeMirror.registerHelper("lint","clike",function() {
              var found = [];
              _.forEach(self.console.errors,function(err) {
                var error = err[0], file = err[1].split("/");
                file = file[file.length-1];
                var line = _.max([err[2] - 1, 0]), column = err[3] - 1;
                var message = err[4];
                console.error(err);
                if (_.contains([self.file,
                                'final-link-result'],
                               file))
                  found.push({ from: CodeMirror.Pos(line, column),
                               to: CodeMirror.Pos(line),
                               message: message,
                               severity: error ? 'error' : 'warning' });
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
            if (self.loaded && !self.isBinaryFile) {
              self.timeout = $timeout(function() {
                self.undoHistory = self.editor.getHistory();
                self.project.saveFile(self.question, self.folder, self.file, self.contents, JSON.stringify(self.undoHistory))
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
              if (self.undoHistory !== undefined){
                self.editor.setHistory(self.undoHistory);
              }
             // }
              if(self.scrollInfo[self.folder] &&
                self.scrollInfo[self.folder][self.file]) {
                var scr = self.scrollInfo[self.folder][self.file];
                self.editor.scrollTo(scr.left, scr.top);
                self.editor.setCursor(scr.line - 1, scr.col - 1);
              }
            }
            self.loaded = true;
          });
          function updateColNums() {
            // update column numbers
            $timeout(function() {
              self.col = self.editor.getCursor().ch + 1;
              self.line = self.editor.getCursor().line + 1;
            }, 0);
          }
          // check and set a css class to the line if over 80 chars
          function warnLineLength(lineNum) {
               var lineStr = self.editor.getLine(lineNum);
               var len = lineStr.length;
               self.editor.findMarksAt(
                  {line: lineNum, ch: 0}
               ).forEach(function (m) {m.clear();});
               if (len > 80) {
                  self.editor.markText(
                     {line: lineNum, ch: 0},
                     {line: lineNum, ch: len},
                     {className: "cm-line-too-long"}
                  );
               }
          }
          // check and set a css class to each line over 80 chars
          function warnAllLength() {
            self.editor.eachLine(function(line) {
               var lineNum = self.editor.getLineNumber(line);
               warnLineLength(lineNum);
            });
          }
          // check and set a css class to cursor line if over 80 chars
          function warnCurrentLength() {
            var lineNum = self.editor.getCursor().line;
            warnLineLength(lineNum);
          }
          // check all lines only when initially loaded
          self.editor.on("change", function(editor, changeObj) {
            if (changeObj.origin === "setValue" || changeObj.origin === "paste") {
               warnAllLength();
            }
          });
          self.editor.on("change", warnCurrentLength);
          self.editor.on("cursorActivity", updateColNums);
          self.editor.on("focus", updateColNums);
          self.editor.on("blur", updateColNums);
        };

        function betterTab(){
          if(self.editor.somethingSelected()){
            self.editor.indentSelection("add");
          } else {
            self.editor.replaceSelection(Array(self.editor.getOption("indentUnit") + 1).join(" "), "end", "+input");
          }
        }

        function negTab(){
          if(self.editor.somethingSelected()){
            self.editor.indentSelection("subtract");
          }
        }

        function toggleEditorFullscreen(evt) {
          evt.preventDefault();
          self.consoleEditor.setOption('fullScreen', false);
          self.editor.focus();
          self.editor.setOption('fullScreen', !self.editor.getOption('fullScreen'));
        }

        function toggleConsoleFullscreen(evt) {
          evt.preventDefault();
          self.editor.setOption('fullScreen', false);
          self.consoleEditor.focus();
          self.consoleEditor.setOption('fullScreen', !self.consoleEditor.getOption('fullScreen'));
        }

        function quitFullscreen(evt) {
          evt.preventDefault();
          self.editor.setOption('fullScreen', false);
          self.consoleEditor.setOption('fullScreen', false);
        }

        function increaseFontSize(evt) {
          evt.preventDefault();
          settings.settings.font_size = Math.min(settings.settings.font_size + 1, 50);
          settings.save();
        }

        function decreaseFontSize(evt) {
          evt.preventDefault();
          settings.settings.font_size = Math.max(settings.settings.font_size - 1, 1);
          settings.save();
        }

        self.refreshSettings = function () {
          // var theme = settings.settings.theme_style === "light" ? "3024-day" : "3024-night";
          var theme = settings.settings.theme_style === "light" ? "default" : "3024-night";
          self.editorReadOnly = !self.ready || self.isBinaryFile || self.fileReadOnly;

          var extraKeys = {
            "Ctrl-Space": "autocomplete",
            "Ctrl-I": self.indentAll,
            // capture save shortcuts and ignore in the editor
            "Ctrl-S": function() { },
            "Cmd-S": function() { },
            "Tab": betterTab,
            "Shift-Tab": negTab,
          };

          self.editorOptions = {
            scrollbarStyle: "overlay",
            autofocus: true,
            lineWrapping: true,
            lineNumbers: !self.isBinaryFile,
            readOnly: self.editorReadOnly,
            mode: mime,
            theme: theme,
            tabSize: parseInt(settings.settings.tab_width),
            indentUnit: parseInt(settings.settings.tab_width),
            onLoad: self.editorLoad,
            matchBrackets: true,
            rulers: [80],
            extraKeys: extraKeys
          };

          self.consoleOptions = {
            scrollbarStyle: "overlay",
            lineWrapping: true,
            readOnly: true,
            mode: "text/plain",
            theme: theme,
            onLoad: self.consoleLoad,
            extraKeys: extraKeys
          };

          var main_hotkeys = [{
            combo: 'ctrl+d',
            description: 'Sends EOF',
            allowIn: ['INPUT', 'TEXTAREA'],
            callback: function(evt) {
              evt.preventDefault();
              self.sendEOF();
            }
          }, {
            combo: 'ctrl+k',
            description: "Kills the currently running program",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: function (evt) {
              evt.preventDefault();
              self.killProgram();
            }
          }, {
            combo: "ctrl+;",
            description: "Editor fullScreen",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: toggleEditorFullscreen
          }, {
            combo: "ctrl+'",
            description: "Console fullScreen",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: toggleConsoleFullscreen
          }, {
            combo: 'ctrl+,',
            description: "Decrease font size",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: decreaseFontSize
          }, {
            combo: 'ctrl+.',
            description: "Increase font size",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: increaseFontSize
          }, {
            combo: 'esc',
            description: "Quit fullScreen",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: quitFullscreen
          }];

          var vim_disabled_hotkeys = [{
            combo: 'ctrl+r',
            description: "Runs the program",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: function (evt) {
              evt.preventDefault();
              if(self.editor.getOption('fullScreen')) self.editor.setOption('fullScreen', false);
              self.runFile();
            }
          }, {
            combo: 'ctrl+e',
            description: "Starts Tests",
            allowIn: ['INPUT', 'SELECT', 'TEXTAREA'],
            callback: function (evt) {
              evt.preventDefault();
              if(self.editor.getOption('fullScreen')) self.editor.setOption('fullScreen', false);
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
          // If the CodeMirror has been loaded, add it to the editor.
          if (self.editor) {
            for (var key in self.editorOptions) {
              self.editor.setOption(key, self.editorOptions[key]);
            }
            $("#editor .CodeMirror *").css({
              fontFamily: sprintf("%s, monospace", settings.settings.font),
              fontSize: sprintf("%dpt", parseInt(settings.settings.font_size))
            });
            self.editor.addKeyMap({'Tab': betterTab});
            self.editor.refresh();
          }
          if (self.consoleEditor) {
            for (var cKey in self.consoleOptions) {
              self.consoleEditor.setOption(cKey, self.consoleOptions[cKey]);
            }
            $("#console .CodeMirror *").css({
              fontFamily: sprintf("%s, monospace", settings.settings.font),
              fontSize: sprintf("%dpt", parseInt(settings.settings.font_size))
            });
            self.consoleEditor.refresh();
          }
        };

        self.renameFile = function() {
          renameModal(self.project, self.question, self.folder, self.file, function(newName) {
            var path = newName.split("/");
            $scope.$parent.editView.refresh();
            $state.go("edit-project.editor.file", {
              question:(path[0]=="common"?self.question:path[0]),
              part:(path.length>2?path[1]:(path[0]=="common"?"common":"question")),
              file:escape(path.length>2?path[2]:path[1])});
          });
        };

	      self.copyFile = function() {
          copyModal(self.project, self.question, self.folder, self.file, function(newName) {
            var path = newName.split("/");
            $scope.$parent.editView.refresh();
            $state.go("edit-project.editor.file", {
              question:(path[0]=="common"?self.question:path[0]),
              part:(path.length>2?path[1]:(path[0]=="common"?"common":"question")),
              file:escape(path.length>2?path[2]:path[1])});
          });
        };

        self.deleteFile = function() {
          confirmModal("Delete File", "Are you sure you want to delete '"+self.file+"'?")
            .then(function() {
              self.project.deleteFile(self.question, self.folder, self.file)
                .then(function() {
                  $scope.$parent.editView.refresh();
                  $state.go("edit-project.editor");
                  self.refreshRunner();
                }).catch(function (res) {
                  errors.report(res, "An error occurred when deleting '"+self.file+"'.");
                  $scope.$parent.editView.refresh();
                  $state.go("edit-project.editor");
                  self.refreshRunner();
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

          var main_undefined = false;

          _.each(msgs, function(res) {
            // If students forget to define a main function, clang spits out a bunch of
            // error messages that aren't informative and confuses students. Don't
            // print these useless error messages.
            var main_undefined_spam = main_undefined &&
                                      (res[4].endsWith("In function `_start':") ||
                                         /relocation \d+ has invalid symbol index \d+$/.test(res[4]));

            if(!main_undefined_spam) {
              self.console.write(sprintf("%s:%d:%d: %s\n", res[1], res[2], res[3], res[4]));
            }

            if(!warn_only && res[4].endsWith("undefined reference to `main'")) {
                main_undefined = true;
                self.console.write("Cannot find the 'main' function.\n");
            }
          });
        }

        self.runFile = function() {runWhenSaved(function () {
          self.killProgram().then(function() {
            self.console.clear();
            self.console.write("Running '"+self.project.name+"/"+self.question+"':\n");
            self.project.run(self.question, false)
              .then(function(res) {
                $scope.$broadcast('program-running');
                self.console.setRunning(self.project, [res.pid], false);
                handleCompileErr(res.messages, true);
              })
              .catch(function(res) {
                if(res.status === "compile-failed") {
                  handleCompileErr(res.messages);
                } else if(typeof res == "string") {
                  self.console.write(res);
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
            self.console.write("Running tests for '"+self.project.name+"/"+self.question+"':\n");
            self.project.run(self.question, true)
              .then(function(res) {
                if(!res.pids) {
                  self.console.write("There are no tests for "+self.project.name+"/"+self.question+".\n");
                }
                else {
                  $q.all(res.pids)
                    .then(function(pids) {
                      self.console.setRunning(self.project, pids, true);
                      handleCompileErr(res.messages, true);
                    });
                }
              })
              .catch(function(res) {
                if(res.status === "compile-failed") {
                  handleCompileErr(res.messages);
                } else if(typeof res == "string") {
                  self.console.write(res);
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
            var def = $q.defer();
            def.resolve();
            return def.promise;
          }
          var p = $q.all(_.map(self.console.PIDs, function(id) {
            return self.project.kill(id);
          }))
          .catch(function (error) {
            errors.report(error, "Could not stop program!");
          });
          self.console.running = false;
          self.console.PIDs = null;
          return p;
        };

        self.indentAll = function() {
          self.editor.operation(function () {
            var lineCount = self.editor.lineCount();
            for (var i = 0; i < lineCount; i++) { self.editor.indentLine(i); }
          });
        };

        self.userInput = "";
        self.sendInput = function($event) {
          if($event.keyCode == 13) {
            if(self.console.running) {
              self.project.sendInput(self.console.PIDs[0], self.userInput + "\n");
              self.console.flushForInput();
              // self.console.write(self.userInput + "\n"); -- not needed for a TTY
              self.userInput = "";
            }
          }
        };

        self.clearConsole = function () {
          self.console.clear();
        };

        self.sendEOF = function() {
          if(self.console.running) {
            var d;
            if(self.userInput) {
              d = self.project.sendInput(self.console.PIDs[0], self.userInput);
            }
            else {
              d = $q.defer();
              d.resolve();
              d = d.promise;
            }
            d.then(function() {
              self.userInput = "";
              self.project.sendEOF(self.console.PIDs[0]).then(function () {
                self.console.running = false;
              });
            });
          }
        };

        self.setFileToRun = function() {
            self.project.setFileToRun(self.question, self.folder, self.file)
              .then(function () {
                  $scope.$emit('setFileToRun', []);
                  self.runnerFile = true;
                  self.isFileToRun = true;
              })
              .catch(function (error) {
                 errors.report(error, "Could not set runner file!");
              });

            // emit an event to the parent scope for
            // since EditorController is in the child scope of EditorFileController

        };

        // Initialization code goes here.
        var settings_key = settings.addWatcher(self.refreshSettings, true);
        $scope.$on("$destroy", function() {
          if (self.timeout && self.ready) {
            $timeout.cancel(self.timeout);
            self.undoHistory = self.editor.getHistory();
            self.project.saveFile(self.question, self.folder, self.file, self.contents, JSON.stringify(self.undoHistory));
          }
          settings.removeWatcher(settings_key);
          ws.unregister_callback(connected_key);
        });
        /** Callback key when connected.
         *  NOTE: This is slightly sketchy -- however, as
         *  the editor will only be loaded if and only if
         *  the socket exists in the first place, this is
         *  fine for now. */
        var connected_key = ws.register_callback('connected', function () {
          //if (self.editor)
          //  self.editor.setOption("readOnly", self.editorReadOnly);
        self.project.openFile(self.question, self.folder, self.file)
          .then(function(conts) {
            self.contents = conts.data;
            if((self.ext === 'rkt' && RegExp("\\s*;;\\s*"+SEASHELL_READONLY_STRING).test(self.contents)) || // racket files
               ((self.ext === 'c' || self.ext === 'h') && RegExp("\\s*\/\/\\s*"+SEASHELL_READONLY_STRING).test(self.contents))  || // c files
               ((self.ext === undefined || self.ext === 'txt') && RegExp("\\s*"+SEASHELL_READONLY_STRING).test(self.contents))) // plaintext files
            {
                self.fileReadOnly = true;
            }
            self.ready = true;
            if (typeof conts.data === "string") {
              if (conts.data.length === 0) self.loaded = true;
              self.project.updateMostRecentlyUsed(self.question, self.folder, self.file);
              self.editor.clearHistory();
              if (conts.history.slice(1).length > 1) {
                self.undoHistory = JSON.parse(conts.history);
                self.editor.setHistory(self.undoHistory);
              } else {
                console.warn("Could not read history");
              }
            } else {
              self.unavailable = true;
            }
            // .ll files store LLVM bytecode, make these appear as binary files to the user
            var splitfile = self.file.split(".");
            if(splitfile[splitfile.length-1] == "ll") {
              self.isBinaryFile = true;
              self.contents = null;
            }
            self.refreshSettings();
          }).catch(function (error) {
            if (typeof error === "string" &&
                error.indexOf("bytes->string/utf-8: string is not a well-formed UTF-8 encoding") != -1) {
              self.isBinaryFile = true;
              self.refreshSettings();
            }
            else {
              errors.report(error, sprintf("Unexpected error while reading file %s!", self.file));
              $state.go('edit-project.editor');
            }
          });
          self.refreshRunner = function () {
            self.project.getFileToRun(self.question)
               .then(function (result) {
                   self.runnerFile = (result !== "");
                   if(self.folder === "question") {
                       self.isFileToRun = (result === (self.question + '/' + self.file));
                   } else {
                       self.isFileToRun = (result === (self.folder + '/' + self.file));
                   }
               });
          };
          self.refreshRunner();
        }, true);
      }]);
