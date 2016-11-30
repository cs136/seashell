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
      self.compile = function(project, question, file_array, runner) {
        var defer = $q.defer();
        var compiler = new Worker('js/offline-compile.min.js');
        compiler.onmessage = function (result) {
          if(result.data.type == "result") {
            if (result.data.status === "compile-failed") {
              defer.reject(result.data);
            } else if (result.data.status === "running") {
              defer.resolve(result.data);
            }
          }
          else if(result.data.type == "error") {
            defer.reject(result.data.err);
          }
          else {
            defer.reject("Unknown result type received from compiler.");
          }
        };
        compiler.postMessage({
          project: project,
          question: question,
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
        var runner = new Worker('js/offline-run.min.js');
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
  ])
  .service('offline-tester', ['$q',
    function ($q) {
      var self = this;

      self.runTests = function (object, test_cb, testdata) {

        var return_promise_pids = [];

        function spawn_runner(test_contents) {
            // run this function when both input and expect files are ready
            var testname = test_contents[0];
            var input_contents = test_contents[1].contents;
            var expect_contents = (test_contents[2] === null ? null : test_contents[2].contents);

            // spawn the runner
            var runner = new Worker('js/offline-run.min.js');

            runner.onmessage = function (message) {
                test_cb(null, message.data);
            };

            // Send in the test case input and expected output
            runner.postMessage({'type': 'testdata',
                                'testname': testname,
                                'input': input_contents,
                                'expect': expect_contents });

            // Return a fake 'pid'. For testing, sending EOF and
            // stdin from console isn't needed nor possible.
            return {
              testname: testname,
              toString: function () {
                return "<offline-tester>";
              },
              kill: function () {
                runner.terminate();
                test_cb(null, {'type': 'done', status: 255});
              },
              sendEOF: function(){},
              programInput: function(){},
              startIO: function () {
                runner.postMessage(object);
              }};
        }

        // for each test case, spawn a runner
        for(var i = 0; i < testdata.length; i++) {
            var testname_promise = $q.when(testdata[i].testname);

            var promise = $q.all([testname_promise, testdata[i].input, testdata[i].expect])
                .then(spawn_runner);

            return_promise_pids.push(promise);
        }

        return return_promise_pids;
      };
    }
  ]);
