/*
 * Angular bindings for the Seashell Offline Compiler.
 * Copyright (C) 2013-2016 The Seashell Maintainers.
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
angular.module('seashell-compiler', [])
  .service('offline-compiler', ['$q',
    function ($q) {
      var self = this;
      self.compile = function(file_array, runner) {
        var defer = $q.defer();
        var compiler = new Worker('js/offline-compile.js');
        compiler.onmessage = function (result) {
          if (result.data.status === "compile-failed") {
            defer.reject(result.data);
          } else if (result.data.status === "running") {
            defer.resolve(result.data);
          }
        };
        compiler.postMessage({
          runnerFile: runner,
          files: file_array});
        return defer.promise;
      };
    }
  ])
  .service('offline-runner', ['$q',
    function ($q) {
      var self = this;

      self.run = function (object, io_cb) {
        var runner = new Worker('js/offline-run.js');
        runner.onmessage = function (result) {
          io_cb(null, result.data);
        };
        return $q.when({
          toString: function () {
            return "<offline-runner>";
          },
          kill: function () {
            runner.terminate();
            io_cb(null, {'type': 'done', status: 255});
          },
          sendEOF: function () {
            runner.postMessage(null);
          },
          programInput: function (input) {
            runner.postMessage(input);
          },
          startIO: function () {
            runner.postMessage(object);
          }
        });
      };
    }
  ]);
