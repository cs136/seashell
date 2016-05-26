/*
 * Angular bindings for the Seashell WebSocket client.
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
angular.module('seashell-websocket', ['ngCookies', 'seashell-local-files'])
  /**
   * WebSocket service:
   *  provides:
   *    register_disconnect_callback |
   *    register_reconnect_callback  | Callbacks to be invoked on change of socket connectivity.
   *    register_fail_callback       | 
   *    register_timein/out_callback |
   *    connect                      - Connects the socket
   *    socket                       - Socket object.  Is invalid after disconnect/fail | before connect.
   */
  .service('socket', ['$q', '$interval', '$cookies', '$timeout', 'localfiles',
    function($q, $interval, $cookies, $timeout, localfiles) {
      "use strict";
      var self = this;
      var SEASHELL_OFFLINE_MODE_COOKIE = 'seashell-offline-mode-cookie';
       
      self._socket = null;
      Object.defineProperty(self, 'socket', {
        get: function () {
          throw new ReferenceError("You forgot to replace something.");
        }
      });
      
      self.syncing = false;
      self.connected = false;
      self.failed = false;
      self.forceOffline = false;

      // load the offline mode setting, which is stored separately
      //  from other Seashell settings as a cookie.
      self.offline_mode = $cookies.get(SEASHELL_OFFLINE_MODE_COOKIE);
      if(self.offline_mode === undefined) {
        self.offline_mode = 0;
        $cookies.put(SEASHELL_OFFLINE_MODE_COOKIE, 0);
      }
      else self.offline_mode = parseInt(self.offline_mode);

      // these will hold workers for offline mode
      self.compiler = null;
      self.runner = null;

      localfiles.init().then(function() {
        self.register_callback("syncing", function() {
          if(self.offlineEnabled()) {
            return self.syncAll();
          }
          return $q.when(true);
        }, true);
      });

      var timeout_count = 0;
      var timeout_interval = null;
      var key = 0;
      var callbacks = {};

      /** Registers callbacks to run when the socket has not seen activity
       *  in some while, and when messages are received after a timeout has passed.
       */
      self.register_callback = function(type, cb, now) {
        callbacks[key] = {
          type: type,
          cb: cb,
          now: now
        };

        if (type === 'disconnected' && !self.connected && now) {
          $timeout(cb, 0);
        } else if (type === 'connected' && self.connected && now) {
          $timeout(cb, 0);
        } else if (type === 'failed' && self.failed && now) {
          $timeout(cb, 0);
        }
        return key++;
      };

      self.unregister_callback = function(key) {
        delete callbacks[key];
      };

      self.unregister_callbacks = function(type) {
        callbacks = _.filter(callbacks, function(item) { return item && item.type == type; });
        key = callbacks.length;
      };

      /** Helper function to invoke the I/O callback. */
      self.io_cb = function(ignored, message) {
        _.each(_.map(_.filter(callbacks, function(x) {
              return x && x.type === 'io';
            }),
            function(x) {
              return x.cb;
            }),
          function(x) {
            x(message);
          });
      };

      self.test_cb = function(ignored, result) {
        _.each(_.map(_.filter(callbacks, function(x) {
              return x.type === 'test';
            }),
            function(x) {
              return x.cb;
            }),
          function(x) {
            x(result);
          });
      };

      /** Connects the socket, sets up the disconnection monitor. */
      self.connect = function() {
        if (!$cookies.get(SEASHELL_CREDS_COOKIE)) {
          self.failed = true;
          $timeout(function() {
            _.each(_.map(_.filter(callbacks, function(x) {
                  return x.type === 'failed';
                }),
                function(x) {
                  return x.cb;
                }),
              function(x) {
                x();
              });
          }, 0);
          return $q.reject("No credentials found!");
        }

        try {
          self._socket = new SeashellWebsocket(sprintf("wss://%s:%d", $cookies.getObject(SEASHELL_CREDS_COOKIE).host, $cookies.getObject(SEASHELL_CREDS_COOKIE).port),
            $cookies.getObject(SEASHELL_CREDS_COOKIE).key,
            /** Failure - probably want to prompt the user to attempt to reconnect/
             *  log in again.
             */
            function() {
              self.failed = true;
              $timeout(function() {
                $interval.cancel(timeout_interval);
                _.each(_.map(_.filter(callbacks, function(x) {
                      return x.type === 'failed';
                    }),
                    function(x) {
                      return x.cb;
                    }),
                  function(x) {
                    x();
                  });
              }, 0);
            },
            /** Socket closed - probably want to prompt the user to reconnect? */
            function() {
              self.connected = false;
              $timeout(function() {
                $interval.cancel(timeout_interval);
                _.each(_.map(_.filter(callbacks, function(x) {
                      return x.type === 'disconnected';
                    }),
                    function(x) {
                      return x.cb;
                    }),
                  function(x) {
                    x();
                  });
              }, 0);
            });
        } catch (e) {
          self.failed = true;
          $timeout(function() {
            _.each(_.map(_.filter(callbacks, function(x) {
                  return x.type === 'failed';
                }),
                function(x) {
                  return x.cb;
                }),
              function(x) {
                x();
              });
          }, 0);
          return $q.reject(e);
        }

        return $q.when(self._socket.ready)
          .then(function() {
            console.log("Seashell socket set up properly.");
            timeout_interval = $interval(function() {
              if (timeout_count++ === 3) {
                _.each(_.map(_.filter(callbacks, function(x) {
                      return x.type === 'timeout';
                    }),
                    function(x) {
                      return x.cb;
                    }),
                  function(x) {
                    x();
                  });
              }
              $q.when(self._socket.ping())
                .then(function() {
                  if (timeout_count >= 3) {
                    _.each(_.map(_.filter(callbacks, function(x) {
                          return x.type === 'timein';
                        }),
                        function(x) {
                          return x.cb;
                        }),
                      function(x) {
                        x();
                      });
                  }
                  timeout_count = 0;
                });
            }, 4000);
            self.connected = true;
            self.failed = false;
            self._socket.requests[-3].callback = self.io_cb;
            self._socket.requests[-4].callback = self.test_cb;
            console.log("Websocket disconnection monitor set up properly.");
            /** Run the callbacks. First the syncing ones, then the
              connected ones when these are resolved. */
            $q.all(_.each(_.map(_.filter(callbacks, function(x) {
                return x.type === 'syncing';
              }), function(x) {
                return x.cb;
              }), function(x) { x(); }))
              .then(function() {
                _.each(_.map(_.filter(callbacks, function(x) {
                      return x.type === 'connected';
                    }),
                    function(x) {
                      return x.cb;
                    }),
                  function(x) {
                    x();
                  });
            });
          });
      };

      self.isConnected = function() {
        return self.connected;
      };

      self.isOffline = function() {
        return self.offline_mode === 2 || (!self.connected && self.offline_mode === 1);
      };

      self.offlineEnabled = function() {
        return self.offline_mode === 1 || self.offline_mode === 2;
      };

      self.setOfflineModeSetting = function(setting) {
        if(setting === 0 || setting === 1 || setting === 2) {
          self.offline_mode = setting;
          $cookies.put(SEASHELL_OFFLINE_MODE_COOKIE, setting);
        }
      };

      /** The following functions are wrappers around sendMessage.
       *  Consult dispatch.rkt for a full list of functions.
       *  These functions take in arguments as specified in server.rkt
       *  and return a JQuery Deferred object. */
      self.ping = function(deferred) {
        return self._socket.ping(deferred);
      };

      self.compileAndRunProject = function(project, question, file, tests, deferred) {
          if(!self.isOffline()) {
            return self._socket.compileAndRunProject(project, question, tests, deferred);
          }
          else {
            var res = $q.defer();
            if(!self.compiler) {
              self.compiler = new Worker("js/offline-compile.js");
            }
            self.compiler.onmessage = function(result) {
              if(result.data.status == "compile-failed") {
                res.reject(result.data);
              }
              else if(result.data.status == "running") {
                self.runner = new Worker("js/offline-run.js");
                self.runner.onmessage = function(msg) {
                  self.io_cb(null, msg.data);
                };
                self.runner.postMessage(result.data.obj);
                res.resolve(result.data);
              }
            };
            return $q.when(file.getDependencies()).then(function(deps) {
              var file_arr = _.map(deps, function(f) { return f.toWorker(); });
              self.compiler.postMessage({
                runnerFile: file.filename(),
                files: file_arr,
                tests: tests
              });
              return res.promise;
            });
          }
      };

      self.programKill = function(pid, deferred) {
        if(pid<0 && self.runner) {
          self.runner.terminate();
          return $q.when();
        }
        return self._socket.programKill(pid, deferred);
      };

      self.sendEOF = function(pid, deferred) {
        // TODO: offline runner
        return self._socket.sendEOF(pid, deferred);
      };

      self.compileProject = function(project, file, deferred) {
        // TODO: offline runner
        return self._socket.compileProject(project, file, deferred);
      };

      self.saveProject = function(project, message, deferred) {
        // TODO: is this even used? 
        if (!self.isOffline()) {
          return self._socket.saveProject(project, message, deferred);
        } else {
          return $q.resolve(false); // noop 
        }
      };

      /**
       * Sync everything, to be called when we first connect to the websocket.
       * Should only be called after offlineEnabled() has been checked.
       *
       * @returns Angular deferred that resolves to true when the sync is done.
       */
      self.syncAll = function() {
        // I don't like the way this works, it'll be very slow.
        //  I am still in favour of a lazy sync process
        $q.all([localfiles.getProjects(), localfiles.getOfflineChanges()])
          .then(function(res) {
            var projects = _.map(res[0], function(p) { return p[0]; });
            $q.all(_.map(projects, localfiles.listProject))
              .then(function(trees) {
                var files = [];
                for(var i=0; i<res[0].length; i++) {
                  console.log(res[0][i][0], trees[i]);
                  if(trees[i]) {
                    files = files.concat(_.map(_.filter(trees[i], function(file) {
                      return !file[1];
                    }), function(file) {
                      return {project: res[0][i][0], path: file[0]};
                    }));
                  }
                }
                // should have everything now, just send it to the backend
                return $q.when(self._socket.sync({
                  projects: projects,
                  files: files,
                  changes: res[1]
                })).then(function(res) {
                  // TODO process the sync results and save them
                  var proms = [];
                  for(var i in res.newProjects) {
                    proms.push(localfiles.newProject(res.newProjects[i]));
                  }
                  for(var i in res.changes) {
                    var file = res.changes[i][1];
                    if(res.changes[i][0] === "editFile") {
                      proms.push(localfiles.writeFile(file.project, file.path, file.contents, file.checksum));
                    }
                    else if(res.changes[i][0] === "deleteFile") {
                      proms.push(localfiles.deleteFile(file.project, file.path));
                    }
                  }
                  for(var i in res.deletedProjects) {
                    proms.push(localfiles.deleteProject(res.deletedProjects[i]));
                  }
                  return $q.all(proms).then(function() {
                    // send the changes back in case we need to act on the files that have
                    //  changed within the open project
                    return res.changes;
                  });
                });
              });
          });
      };

      // saves the directory & file structure of a project locally
      self.updateTree = function(project) {
        if(self.offlineEnabled())
          localfiles._dumpProject(project);
      };

      self.getProjects = function(deferred) {
        if (!self.isOffline()) {
          return self._socket.getProjects(deferred)
          .then(function(projects) {
            localfiles.setProjects(projects);
            return projects;
          });
        }
        else { 
          return localfiles.getProjects();
        }
      };

      self.listProject = function(name, deferred) {
        if (!self.isOffline()) {
          localfiles.listProject(name).then(
              function(tree) {
                console.log("[websocket] offline listProject", tree);
              });
          return self._socket.listProject(name, deferred);
        } else {
          return localfiles.listProject(name);
        }
      };

      self.newProject = function(name, deferred) {
        // TODO: offline mode 
        if (!self.isOffline()) {
          return self._socket.newProject(name, deferred);
        } else {
          return self._rejectOffline();
        }
      };

      self.newProjectFrom = function(name, src_url, deferred) {
        if (!self.isOffline()) {
          return self._socket.newProjectFrom(name, src_url, deferred);
        } else {
          return self._rejectOffline();
        }
      };

      self.deleteProject = function(name, deferred) {
        // TODO: offline mode
        if (!self.isOffline()) {
          return self._socket.deleteProject(name, deferred);
        } else {
          return self._rejectOffline();
        }
      };

      self.lockProject = function(name, deferred) {
        // locking only makes sense when we're online
        if (!self.isOffline()) {
          return self._socket.lockProject(name, deferred);
        } else {
          return $q.when();
        }
      };

      self.forceLockProject = function(name, deferred) {
        if (!self.isOffline()) {
          return self._socket.forceLockProject(name, deferred);
        } else {
          return $q.when();
        }
      };

      self.unlockProject = function(name, deferred) {
        // locking only makes sense when we're online
        if (!self.isOffline()) {
          return self._socket.unlockProject(name, deferred);
        } else {
          return $q.when();
        }
      };


      // These two functions are provided separately
      // because code for handling online/offline stuff
      // is compilcated and needs to be dealt with in project-service
      self.onlineReadFile = function(name, file_name, deferred) {
        if (self.forceOffline) return $q.reject();
        return self._socket.readFile(name, file_name, deferred);
      };

      self.offlineReadFile = function(name, file_name, deferred) {
        return localfiles.readFile(name, file_name);
      };

      self.newFile = function(name, file_name, contents,
        encoding, normalize, deferred) {
        localfiles.newFile(name, file_name, contents, encoding, normalize);
        if (!self.isOffline()) {
          return self.onlineNewFile(name, file_name, contents, encoding, normalize, deferred);
        }
      };

      self.onlineNewFile = function(name, file_name, contents, encoding, normalize, deferred) {
        return self._socket.newFile(name, file_name, contents, encoding, normalize, deferred);
      };

      self.restoreFileFrom = function(projectName, fpath, url) {
        if (!self.isOffline()) {
          return self._socket.restoreFileFrom(projectName, fpath, url);
        } else {
          return self._rejectOffline();
        }
      };


      self.newDirectory = function(name, dir_name, deferred) {
        localfiles.newDirectory(name, dir_name);
        return self._socket.newDirectory(name, dir_name, deferred);
      };

      self.writeFile = function(name, file_name, file_content, deferred) {
        var offlineWrite = function(checksum) {
          localfiles.writeFile(name, file_name, file_content, checksum);
          return checksum;
        };

        if (self.forceOffline) return $q.when(offlineWrite(false));

        return $q.when(self._socket.writeFile(name, file_name, file_content, deferred))
          .then(offlineWrite)  // get checksum from backend and write
          .catch(function () { offlineWrite(false); }); // force write
      };


      self.offlineWriteFile = function(name, file_name, file_content, checksum) {
        localfiles.writeFile(name, file_name, file_content, checksum);
      };

      self.deleteFile = function(name, file_name, deferred) {
        var offlineResult = localfiles.deleteFile(name, file_name);
        if (!self.isOffline()) {
          return self._socket.deleteFile(name, file_name, deferred);
        } else {
          return offlineResult;
        }
      };

      self.deleteDirectory = function(name, dir_name, deferred) {
        // TODO: is this even used? 
        return self._socket.deleteDirectory(name, dir_name, deferred);
      };

      self.programInput = function(pid, contents, deferred) {
        // TODO: offline runner
        return self._socket.programInput(pid, contents, deferred);
      };

      self.getExportToken = function(project, deferred) {
        if (self.isOffline()) {
          return self._rejectOffline(); 
        } else {
          return self._socket.getExportToken(project, deferred);
        }
      };

      self.getUploadFileToken = function(project, file, deferred) {
        if (self.isOffline()) {
          return self._rejectOffline(); 
        } else {
          return self._socket.getUploadFileToken(project, file, deferred);
        }
      };

      self.renameFile = function(project, oldName, newName, deferred) {
        var offlineResult = localfiles.renameFile(project, oldName, newName);
        if (!self.isOffline()) {
          var onlineResult = self._socket.renameFile(project, oldName, newName, deferred);
          return $q.all([onlineResult, offlineResult]);
        } else {
          return $q.all([offlineResult]);
        }
      };

      self.getMostRecentlyUsed = function(project, directory, deferred) {
        // TODO: offline mode 
        if (!self.isOffline()) {
          return self._socket.getMostRecentlyUsed(project, directory, deferred);
        } else {
          return $q.when(false);
        }
      };

      self.updateMostRecentlyUsed = function(project, directory, predicate, data, deferred) {
        // TODO: store this in offline mode 
        if (!self.isOffline()) {
          return self._socket.updateMostRecentlyUsed(project, directory, predicate, data, deferred);
        } else {
          return $q.when();
        }
      };

      self.saveSettings = function(settings, deferred) {
        // TODO: offline mode
        if (!self.isOffline()) {
          return self._socket.saveSettings(settings, deferred);
        } else {
          return $q.when();
        }
      };

      self.getSettings = function(deferred) {
        // TODO: offline mode
        if (!self.isOffline()) {
          return self._socket.getSettings(deferred);
        } else {
          return $q.when();
        }
      };

      self.marmosetSubmit = function(project, assn, subdir, deferred) {
        if (!self.isOffline()) {
          return self._socket.marmosetSubmit(project, assn, subdir, deferred);
        } else {
          return self._rejectOffline();
        }
      };

      self.startIO = function(project, pid, deferred) {
        //  TODO: offline runner
        return self._socket.startIO(project, pid, deferred);
      };

      self.archiveProjects = function(deferred) {
        if (self.isOffline()) {
          return self._rejectOffline();
        } else {
          return self._socket.archiveProjects(deferred);
        }
      };

      self.getFileToRun = function(project, question, deferred) {
        var offlineResult = localfiles.getRunnerFile(project, question);
        if (!self.isOffline()) {
          var onlineResult = self._socket.getFileToRun(project, question, deferred);
          return $q.all([onlineResult, offlineResult])
            .then(function(result) {
              return result[0] || result[1];
            }).catch(
            function(error) {
              // TODO: what if one of them doesn't resolve?
            });
        } else {
          return $q.when(offlineResult);
        }
      };

      self.setFileToRun = function(project, question, folder, file, deferred) {
        var offlineResult = localfiles.setRunnerFile(project, question, folder, file);
        if (!self.isOffline()) {
          var onlineResult = self._socket.setFileToRun(project, question, folder, file, deferred);
          return $q.all([onlineResult, offlineResult]).catch(
            function(error) {
              // TODO: what if one of them doesn't resolve?
            });
        } else {
          return $q.all([offlineResult]);
        }
      };

      self._rejectOffline = function() {
        return $q.reject("Functionality not available in offline mode.");
      };
    }
  ]);
