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
/* jslint esversion: 6 */
angular.module('seashell-websocket', ['ngCookies', 'seashell-local-files', 'seashell-compiler'])
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
  .service('socket', ['$q', '$interval', '$cookies', '$timeout', 'localfiles', 'offline-compiler', 'offline-runner',
    function($q, $interval, $cookies, $timeout, localfiles, compiler, runner) {
      "use strict";
      var self = this;
      var SEASHELL_OFFLINE_MODE_COOKIE = 'seashell-offline-mode-cookie';
      self._socket = null;
      Object.defineProperty(self, 'socket', {
        get: function () {
          throw new ReferenceError("You forgot to replace something.");
        }
      });
      self.synced = false;
      self.connected = false;
      self.failed = false;

      // load the offline mode setting, which is stored separately
      //  from other Seashell settings as a cookie.
      self.offline_mode = $cookies.get(SEASHELL_OFFLINE_MODE_COOKIE);
      if (self.offline_mode === undefined) {
        self.offline_mode = 0;
      }
      else
        self.offline_mode = parseInt(self.offline_mode);

      // these will hold workers for offline mode
      self.compiler = null;
      self.runner = null;


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

      self.invoke_cb = function(type, message) {
        return _.map(_.filter(callbacks, function(x) {
            return x && x.type === type;
          }),
          function(x) {
            return x.cb(message);
          });
      };

      /** Helper function to invoke the I/O callback. */
      self.io_cb = function(ignored, message) {
        return self.invoke_cb('io', message);
      };

      self.test_cb = function(ignored, result) {
        return self.invoke_cb('test', result);
      };

      /** Connects the socket, sets up the disconnection monitor. */
      self.connect = function() {
        if (!$cookies.get(SEASHELL_CREDS_COOKIE)) {
          self.failed = true;
          $timeout(function() {
            self.invoke_cb('failed');
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
                self.invoke_cb('failed');
              }, 0);
            },
            /** Socket closed - probably want to prompt the user to reconnect? */
            function() {
              self.connected = false;
              $timeout(function() {
                $interval.cancel(timeout_interval);
                self.invoke_cb('disconnected');
              }, 0);
            });
        } catch (e) {
          self.failed = true;
          $timeout(function() {
            self.invoke_cb('failed');
          }, 0);
          return $q.reject(e);
        }

        return $q.when(self._socket.ready)
          .then(function() {
            console.log("Seashell socket set up properly.");
            timeout_interval = $interval(function() {
              if (timeout_count++ === 3) {
                self.invoke_cb('timeout');
              }
              $q.when(self._socket.ping())
                .then(function() {
                  if (timeout_count >= 3) {
                    self.invoke_cb('timein');
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
            self.invoke_cb('syncing');
          });
      };

      self.isConnected = function() {
        return self.connected;
      };

      self.isOffline = function() {
        return self.offline_mode === 2 || (!self.connected && self.offline_mode === 1);
      };

      self.offlineEnabled = function() {
        return (self.offline_mode === 1 || self.offline_mode === 2);
      };

      self.setOfflineModeSetting = function(setting) {
        var old = self.offline_mode;
        if(setting === 0 || setting === 1 || setting === 2) {
          self.offline_mode = setting;
          // Set cookie, set expiry to some date sufficiently
          // in the future.
          var expiryDate = new Date();
          expiryDate.setFullYear(expiryDate.getFullYear() + 10);
          $cookies.put(SEASHELL_OFFLINE_MODE_COOKIE, setting,
              {'secure': true, expires: expiryDate});
        }
        if(old === 2 && self.offline_mode !== old) {
          // trigger reconnect and sync
          if(!self.connected)
            self.connect();
          else
            self.invoke_cb('syncing');
        }
      };

      /** The following functions are wrappers around sendMessage.
       *  Consult dispatch.rkt for a full list of functions.
       *  These functions take in arguments as specified in server.rkt
       *  and return a JQuery Deferred object. */
      function make_offline_disabled(name) {
        return function () {
          if (!self.isOffline()) {
            return $q.when(self._socket[name].apply(self._socket, arguments));
          } else {
            return self._rejectOffline(name);
          }
        };
      }
      function make_offline_noop(name) {
        return function () {
          if (!self.isOffline()) {
            return $q.when(self._socket[name].apply(self._socket, arguments));
          } else {
            return $q.when(true);
          }
        };
      }
      function make_offline_enabled(name, offlineWriteThrough) {
        var online_arity = SeashellWebsocket.prototype[name].length;
        var offline_arity = localfiles[name].length;
        if (offline_arity != online_arity && offline_arity != online_arity + 1) {
          throw sprintf("Offline and online arities differ: %s (%d, %d)!", name, online_arity, offline_arity);
        }
        if (online_arity != offline_arity) {
          console.log("Registering function %s which will use online result in write-through mode.", name);
        }
        return function () {
          var args = Array.from(arguments);
          if (args.length > online_arity) {
            return $q.reject(sprintf("Too many arguments passed to function %s!", name));
          }

          if (self.offlineEnabled()) {
            if (self.isOffline()) {
              console.log(sprintf("Invoking %s in offline mode.", name));
              return localfiles[name].apply(localfiles, args);
            } else {
              return $q.when(self._socket[name].apply(self._socket, args))
                .then(function (result) {
                  if (offlineWriteThrough) {
                    console.log(sprintf("Invoking %s in write-through mode.", name));
                    var write_through_args = args
                      .concat(new Array(online_arity - args.length))
                      .concat([result]);
                    return localfiles[name].apply(localfiles, write_through_args)
                      .then(function() {
                        return result;
                      });
                  } else {
                    return result;
                  }
                })
                .catch(function (error) {
                  if (self.isOffline()) {
                    console.log(sprintf("Invoking %s in offline mode.", name));
                    return localfiles[name].apply(localfiles, args);
                  } else {
                    return $q.reject(error);
                  }
                });
            }
          } else {
            return $q.when(self._socket[name].apply(self._socket, args));
          }
        };
      }

      // These functions are not available in offline mode.
      self.ping = make_offline_disabled('ping');
      self.newProject = make_offline_disabled('newProject');
      self.newProjectFrom = make_offline_disabled('newProjectFrom');
      self.deleteProject = make_offline_disabled('deleteProject');
      self.restoreFileFrom = make_offline_disabled('restoreFileFrom');
      self.getUploadFileToken = make_offline_disabled('getUploadFileToken');
      self.getExportToken = make_offline_disabled('getExportToken');
      // These functions do nothing and just resolve in offline mode.
      self.lockProject = make_offline_noop('lockProject');
      self.forceLockProject = make_offline_noop('forceLockProject');
      self.unlockProject = make_offline_noop('unlockProject');
      self.marmosetSubmit = make_offline_noop('marmosetSubmit');
      self.archiveProjects = make_offline_noop('archiveProjects');
      // These functions either:
      //  - return the online result if online.
      //  - return the offline result if offline.
      self.getProjects = make_offline_enabled('getProjects');
      self.listProject = make_offline_enabled('listProject');
      self.readFile = make_offline_enabled('readFile');
      self.getFileToRun = make_offline_enabled('getFileToRun');
      // These functions:
      //  - invoke the offline version if offline
      //  - invoke the both the offline version and the online
      //    version if online, returning the online version.
      self.newDirectory = make_offline_enabled('newDirectory', true);
      self.deleteDirectory = make_offline_enabled('deleteDirectory', true);
      self.newFile = make_offline_enabled('newFile', true);
      self.writeFile = make_offline_enabled('writeFile', true);
      self.deleteFile = make_offline_enabled('deleteFile', true);
      self.renameFile = make_offline_enabled('renameFile', true);
      self.setFileToRun = make_offline_enabled('setFileToRun', true);

      self.compileAndRunProject = function(project, question, file, tests, deferred) {
          if(!self.isOffline()) {
            return self._socket.compileAndRunProject(project, question, tests, deferred);
          }
          else {
            return $q.when(file.getDependencies())
              .then(function(deps) {
                return $q.all(_.map(deps, function(f) { return f.toWorker(); }));
              })
              .then(function (file_arr) {
                return compiler.compile(file_arr, file.filename());
              })
              .then(function (result) {
                // Fill in the PID with a fake, offline PID.
                return runner.run(result.obj,
                    function (message, data) {
                      self.io_cb(message, data);
                    })
                  .then(function (pid) {
                    result.pid = pid;
                    return result;
                  });
              });
          }
      };

      self.programKill = function(pid) {
        if (typeof pid === "object") {
          pid.kill();
          return $q.when();
        } else {
          return self._socket.programKill(pid);
        }
      };

      self.sendEOF = function(pid) {
        if (typeof pid === "object") {
          pid.sendEOF();
          return $q.when();
        } else {
          return self._socket.sendEOF(pid);
        }
      };

      self.programInput = function(pid, contents) {
        if (typeof pid === "object") {
          pid.programInput(contents);
          return $q.when();
        } else {
          return self._socket.programInput(pid, contents);
        }
      };

      self.startIO = function(project, pid, deferred) {
        if (typeof pid === "object") {
          pid.startIO();
          return $q.when();
        } else {
          return self._socket.startIO(pid, contents);
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

      /**
       * Sync everything, to be called when we first connect to the websocket.
       * Should only be called after offlineEnabled() has been checked.
       *
       * @returns Angular deferred that resolves to true when the sync is done.
       */
      self.syncAll = function() {
        console.log("syncAll invoked");
        return $q.all([localfiles.getProjects(), localfiles.listAllProjectsForSync(),localfiles.getOfflineChanges()])
          .then(function(result) {
            var projects = _.map(result[0], function (p) { return p[0]; });
            var files = result[1];
            var changes = result[2];
            return $q.when(self._socket.sync({
              projects: projects,
              files: files,
              changes: changes}));
          })
          .then(function(result) {
            return $q.when(localfiles.applyChanges(result.changes, result.newProjects, result.deletedProjects))
              .then(function () {
                return result.changes;
              });
          })
          .then(function (changes) {
            if(self.connected) {
              self.invoke_cb('connected');
            }
            // send the changes back in case we need to act on the files that have
            //  changed within the open project
            return changes;
          });
      };

      self._rejectOffline = function(name) {
        return $q.reject(name + " is not available in offline mode.");
      };

      // Register callback for syncing.
      self.register_callback("syncing", function() {
        if(self.offlineEnabled()) {
          return self.syncAll();
        } else {
          self.invoke_cb('connected');
        }
      }, true);
    }
  ]);
