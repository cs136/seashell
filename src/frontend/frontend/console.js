/**
 * Seashell's frontend console service
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
  .service('console-service', ['$rootScope', 'socket', '$timeout',
    function($scope, socket, $timeout) {
    var self = this;
    self.PIDs = null;
    // running is true iff we are running with "run", allows input
    self.running = false;
    self.inst = null;
    self.errors = [];
    // Terminal object (if it exists)
    self.terminal = null;
    var ind = "";
    var spl ="";
    var return_codes = {
      "1":"An error occurred",
      "23":"Memory leak",
      "134":"Assertion failed",
      "136":"Erroneous arithmetic operation",
      "139":"Segmentation fault",
      "254":"Program was killed",
      "255":"Timeout"
    };

    // err is a json object (parsed & filtered in backend, only printed here)
    // TODO: better logic (self.write or something?)
    function maybe_log(string){
      var debugmode = false;
      if(debugmode) { console.log(string); }
    }

    function print_asan_error(err) {
      // json object should have:
      // error_type (string)
      // depending on error_type, you can assume each frame-list will have certain key-value pairs, AND
      // at the top level, the json object passed in will also have certain key-value pairs
      // **OR**: have a misc field and just iterate over it and print everything (at the same level as frame_list AND/OR at the very top level with
      // call_stacks: hash (definitely has frame-list; may have other members)
      // frame_list: list of frames (with line numbers, columns, files, etc.
      // frame: column, file, frame#, function, function_offset, line, module, offset
      // raw message
      maybe_log(err);
      if(err.error_type == "unknown" && err.raw_message === "") { return; }
      var to_print = [];
      to_print.push("Memory error occurred! Type of error: " + err.error_type);
      if(err.call_stacks.length === 0) {
        // If no call stack was sent back, fall back to showing the raw ASAN output
        to_print.push("Raw error message:");
        to_print.push(err.raw_message);
      }
      for (var current_stack = 0; current_stack < err.call_stacks.length; current_stack++) {
        maybe_log('current stack: ' + current_stack);
        var framelist = err.call_stacks[current_stack].framelist;
        var framelist_misc = err.call_stacks[current_stack][1];
        // print framelist
        to_print.push('current framelist: ' + current_stack);
        for (var i = 0; i < framelist.length; i++){
          maybe_log('current framelist: ' + i);
          // print each frame
          var framelist_indent = '\t  ';
          if(framelist.length <= 1) { framelist_indent = '\t'; }
          to_print.push(framelist_indent + 'frame ' + framelist[i].frame + ':' +
                        ' function ' + framelist[i].function +
                        ' in file ' + framelist[i].file.replace(/^.*[\\\/]/, '') +
                        ' at line ' + framelist[i].line +
                        ('column' in framelist[i] ? ', column '+framelist[i].column : ''));

        }
        // print misc (Second item)
        for (var key in err.call_stacks[current_stack].misc) {
          maybe_log('printing inner misc: ' + key);
          to_print.push('\t' + key.replace(/_/g, " ") + ': ' + err.call_stacks[current_stack].misc[key]);
        }
        maybe_log('done iterating over inner misc');
      }
      for (var key2 in err.misc) {
        maybe_log('printing outer misc: ' + key2);
        to_print.push(key2.replace(/_/g, " ") + ': ' + err.misc[key2]);
      }
      for (var j = 0; j < to_print.length; j++){
        self.write(to_print[j] + '\r\n');
      }
    }

    socket.register_callback("io", function(io) {
      if(io.type == "stdout") {
        self.write(io.message);
      }
      else if(io.type == "stderr") {
        self.write(io.message);
      }
      else if (io.type == "done") {
        if (io.asan) {
          // Print parsed ASAN output
          print_asan_error(JSON.parse(io.asan));
        }
        self.write("Program finished with exit code "+io.status);
        if(io.status !== 0 && return_codes[io.status]) {
          self.write(sprintf(" (%s)", return_codes[io.status]));
        }
        self.write(".\r\n");
        self.PIDs = null;
        self.running = false;
      }
    });

    function printExpectedFromDiff(res) {
      // res.diff is an array of (string || Array)
      // a string is a line that matches in the diff, so we print 
      // an array a has a[0] false if it came from the expected output
      // a[0] true if it came from the actual output
      // The array contains n lines of text from the lines that differ, n >= 1
      var output = "";
      _.each(res.diff, function(block) {
        if (_.isString(block)) output += "\r\n" + block;
        else if (block[0] === false) {
          _.each(block.slice(1), function(line) {
            output += "\r\n" + line;
          });
        }
      });
      self.write(output.substring(1));
    }

    socket.register_callback("test", function(res) {

      // When online, PIDs are simple PIDs...
      //  In offline tests we use test name instead of PID
      self.PIDs = _.filter(self.PIDs, function(pid) {
        return pid.toString() != res.pid.toString();
      });
      self.PIDs = self.PIDs.length === 0 ? null : self.PIDs;
      var parsedASAN = res.asan_output ? JSON.parse(res.asan_output) : false;

      if(res.result==="passed") {
        self.write('----------------------------------\r\n');
        self.write(sprintf("Test \"%s\" passed.\r\n", res.test_name));
      }
      else if(res.result==="failed") {
        self.write('----------------------------------\r\n');
        self.write(sprintf("Test \"%s\" failed.\r\n", res.test_name));
        self.write('Produced output (stdout):\r\n');
        self.write(res.stdout);
        self.write('---\r\n');
        self.write('Expected output (stdout):\r\n');
        printExpectedFromDiff(res);
        self.write('---\r\n');
        self.write('Produced errors (stderr):\r\n');
        self.write(res.stderr);
        if(parsedASAN.raw_message !== "") {
            self.write("AddressSanitizer Output:\r\n");
            print_asan_error(parsedASAN);
        }
      } else if(res.result==="error") {
        self.write('----------------------------------\r\n');
        self.write(sprintf("Test \"%s\" caused an error (with return code %d)!\r\n", res.test_name, res.exit_code));
        self.write('Produced output (stderr):\r\n');
        self.write(res.stderr);
        if(parsedASAN.raw_message !== "") {
            // Parse the ASAN json string that the backend gives us.
            self.write("AddressSanitizer Output:\r\n");
            print_asan_error(parsedASAN);
        }
      } else if(res.result==="no-expect") {
        self.write('----------------------------------\r\n');
        self.write(sprintf("Test \"%s\" produced output (stdout):\r\n", res.test_name));
        self.write(res.stdout);
        self.write('Produced output (stderr):\r\n');
        self.write(res.stderr);
        if(parsedASAN.raw_message !== "") {
            // Parse the ASAN json string that the backend gives us.
            self.write("AddressSanitizer Output:\r\n");
            print_asan_error(parsedASAN);
        }
      } else if(res.result==="timeout") {
        self.write('----------------------------------\r\n');
        self.write(sprintf("Test \"%s\" timed out.\r\n", res.test_name));
      }
      else if(res.result==="killed") {
        self.write('----------------------------------\r\n');
        self.write(sprintf("Test \"%s\" was killed.\r\n", res.test_name));
      }
    });

    self.setRunning = function(project, PIDs, testing) {
      self.running = !testing;
      self.PIDs = PIDs;
      _.each(self.PIDs, function (pid) {
        socket.startIO(project.name, pid);
      });
    };
    self.clear = function() {
      self._contents = "";
      if (self.terminal)
        self.terminal.clear();
    };
    self.write = function(msg) {
      self.terminal.write(msg);
    };
  }]);
