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
    var return_codes = {
      "1":"An error occurred",
      "23":"Memory leak",
      "134":"Assertion failed",
      "136":"Erroneous arithmetic operation",
      "139":"Segmentation fault",
      "254":"Program was killed",
      "255":"Timeout"
    };
    var asan_contents = [];

    // err is a json object (parsed & filtered in backend, only printed here)
    // TODO: better logic (self.write or something?)
    function maybe_log(string){
      var debugmode = true;
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
      if(err.error_type == "unknown") { return; }
      var to_print = [];
      to_print.push("MEMORY ERROR TYPE: " + err.error_type);
      for (var current_stack = 0; current_stack < err.call_stacks.length; current_stack++) {
        maybe_log('current stack: ' + current_stack);
        var framelist = err.call_stacks[current_stack].framelist;
        var framelist_misc = err.call_stacks[current_stack][1];
        // print framelist
        to_print.push('current framelist: ' + current_stack);
        for (var i = 0; i < framelist.length; i++){
          maybe_log('current framelist: ' + i);
          //to_print.push('framelist: ' + i);
          // print each frame
          // TODO: re-arrange these & decide not to print some out
          to_print.push('\tframe: '      + framelist[i].frame);
          to_print.push('\t\tfile: '     + framelist[i].file.replace(/^.*[\\\/]/, ''));
          //to_print.push('\t\tmodule: ' + framelist[i].module);
          //to_print.push('\t\toffset: ' + framelist[i].offset);
          to_print.push('\t\tfunction: ' + framelist[i].function);
          //to_print.push('\t\tfunction_offset: ' + framelist[i].function_offset);
          to_print.push('\t\tline: '     + framelist[i].line);
          to_print.push('\t\tcolumn: '   + framelist[i].column);
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
      //maybe_log('done iterating over everything; to_print has ' + to_print.length + ' items');
      //to_print.push(err.raw_message);
      for (var j = 0; j < to_print.length; j++){
        self.write(to_print[j] + '\n');
        //console.log(to_print[j]);
      }
    }

    socket.register_callback("io", function(io) {
      if(io.type == "stdout") {
        ind = io.message.indexOf("\n");
        if (ind > -1) {
          spl = io.message.split("\n");
          self._write(self.stdout);
          while (spl.length>1) { self._write(spl.shift() + "\n"); }
          self.stdout = spl[0];
        }
        else {
          self.stdout += io.message;
        }
      }
      else if(io.type == "stderr") {
        ind = io.message.indexOf("\n");
        if (ind > -1) {
          if (!self.asan_parse) {
            self._write(self.stderr);
          } else {
            io.message = self.stderr + io.message;
          }
          spl = io.message.split("\n");
          while (spl.length>1) {
            if (!self.asan_parse && /^=+$/.test(spl[0])) {
              self.asan_parse = true;
            }
            else if (!self.asan_parse) {
              self._write(spl.shift() + "\n");
            } else {
              asan_contents.push(spl.shift());
            }
          }
          self.stderr = spl[0];
        } else {
          self.stderr += io.message;
        }
      }
      else if (io.type == "done") {
        self._write(self.stdout);
        self._write(self.stderr);
        self.stdout = self.stderr = "";
        if (io.asan) {
          // Print parsed ASAN output
          print_asan_error(JSON.parse(io.asan));
        }
        self.write("\nProgram finished with exit code "+io.status);
        if(io.status !== 0 && return_codes[io.status]) {
          self.write(sprintf(" (%s)", return_codes[io.status]));
        }
        self.write(".\n");
        self.PIDs = null;
        self.running = false;
      }
      self.flush();
    });

    function printExpectedFromDiff(res) {
        // res.diff is an array of (string || Array)
        // a string is a line that matches in the diff, so we print 
        // an array a has a[0] false if it came from the expected output
        // a[0] true if it came from the actual output
        // The array contains n lines of text from the lines that differ, n >= 1
        _.each(res.diff, function(block) {
            if (_.isString(block)) self.write(block + "\n");
            else if (block[0] === false) {
                _.each(block.slice(1), function(line) {
                    self.write(line + "\n");
                });
            }
        });
    }

    socket.register_callback("test", function(res) {
      self.PIDs = _.without(self.PIDs, res.pid);
      self.PIDs = self.PIDs.length === 0 ? null : self.PIDs;
      // Try to parse res.stderr for ASAN error output
      // This string is defined in runner.rkt and should equal it.
      var asan_marker_string = "this_is_a_special_asan_marker_string_7f84028cdd719df7570532dbf54cd10c3b5a1cc1b4bb62193079da701fdd9acb";
      var asan_split_result = res.stderr.split(asan_marker_string);
      var asan_json_string = "";
      if(asan_split_result.length == 3) {
        asan_json_string = asan_split_result[1];
      }

      if(res.result==="passed") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test \"%s\" passed.\n", res.test_name));
      }
      else if(res.result==="failed") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test \"%s\" failed.\n", res.test_name));
        self.write('Produced output (stdout):\n');
        self.write(res.stdout);
        self.write('\n'); 
        // need to print a newline so that it matches up with printExpectedFromDiff
        self.write('---\n');
        self.write('Expected output (stdout):\n');
        printExpectedFromDiff(res);   
        self.write('---\n');
        self.write('Produced errors (stderr):\n');
        if(asan_json_string) {
          self.write(asan_split_result[0]);
          self.write(asan_split_result[2]);
          print_asan_error(JSON.parse(asan_json_string));
        } else {
          self.write(res.stderr);
        }
        self.write('\n');
      } else if(res.result==="error") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test \"%s\" caused an error (with return code %d)!\n", res.test_name, res.exit_code));
        self.write('Produced output (stderr):\n');
        if(asan_json_string) {
          self.write(asan_split_result[0]);
          self.write(asan_split_result[2]);
          print_asan_error(JSON.parse(asan_json_string));
        } else {
          self.write(res.stderr);
        }
        self.write('\n');
      } else if(res.result==="no-expect") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test \"%s\" produced output (stdout):\n", res.test_name));
        self.write(res.stdout);
        self.write('Produced output (stderr):\n');
        if(asan_json_string) {
          self.write(asan_split_result[0]);
          self.write(asan_split_result[2]);
          print_asan_error(JSON.parse(asan_json_string));
        } else {
          self.write(res.stderr);
        }
        self.write('\n');
      } else if(res.result==="timeout") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test \"%s\" timed out.\n", res.test_name));
      }
      else if(res.result==="killed") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test \"%s\" was killed.\n", res.test_name));
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
  }]);
