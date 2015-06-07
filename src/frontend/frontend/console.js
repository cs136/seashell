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

    // contents is an array of lines of address sanitizer output
    function parse_asan_output(contents) {
      var only_free = true;
      var traced_main = false;

      function stack_trace_line(line) {
        try {
          if(/^{/.test(line)) {
            var json = line.replace(/'/g, '"');
            var frame = JSON.parse(json);
            var short_module = frame.module && frame.module.split('/').pop();
            var short_file = frame.file && frame.file.split('/').pop();
            if (frame.function === "__interceptor_free" || frame.function === "free") {
              traced_main = false;
              if (only_free) {
                self._write("  Freed:\n");
                only_free = false;
              } else {
                self._write("  First freed:\n");
              }
            } else if (frame.function === "malloc" || frame.function === "__interceptor_malloc") {
              traced_main = true;
              self._write("  Allocated:\n");
            }
            else if (frame.function !== "<null>" && frame.file !== "<null>") {
              traced_main = traced_main || (frame.function === "main");
              if (!traced_main || (frame.function !== "_start" && frame.function !== "__libc_start_main"))
                self._write(sprintf("  in %s, %s:%d:%d\n",
                  frame.function,
                  short_file,
                  frame.line,
                  frame.column));
            } else if (frame.function !== "<null>") {
              traced_main = traced_main || (frame.function === "main");
              if (!traced_main || (frame.function !== "_start" && frame.function !== "__libc_start_main"))
                self._write(sprintf("  in %s, from module %s (+%x)\n", frame.function, short_module, frame.offset));
            } else {
              self._write(sprintf("  in module %s (+%x)\n", short_module, frame.offset));
            }
          }
        } catch (e) {
          console.log("Could not produce stack trace from line: %s, %s", line, e);
        }
      }
      function stack_trace(contents) {
        for(var i=0; i<contents.length; i++) {
          stack_trace_line(contents[i]);
        }
      }
       
      var filepatt = /\/([^\/]+(:[0-9]+|[^\)]+))\)?$/;
      var addrpatt = /0x[0-9a-f]{12}/;
      if(/ SEGV /.test(contents[1]) && filepatt.test(contents[2])) { // segfault
        self._write(sprintf("Attempt to access invalid address %s.\n",
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/ SEGV /.test(contents[1])) {
        self._write(sprintf("%s\n",
          /^[^\(]*/.exec(contents[1])));
      }
      else if(/stack-overflow /.test(contents[1])) {
        self._write(sprintf("Stack overflow on address %s. Check call stack.\n",
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/stack-buffer-(over|under)flow /.test(contents[1])) { // stack buffer overflow
        self._write(sprintf("Stack buffer overflow on address %s. Check array indices.\n",
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/heap-buffer-(over|under)flow /.test(contents[1])) { // heap buffer overflow
        self._write(sprintf("Heap buffer overflow on address %s. Check indices used for dynamically allocated arrays.\n",
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/LeakSanitizer:/.test(contents[1])) { // memory leak
        self._write("Memory leaks occurred:\n");
        for(var idx=3; idx < contents.length; idx++) {
          if(/^(Direct|Indirect)/.test(contents[idx])) {
            var last = idx;
            for(; !/^$/.test(contents[last]); last++);
            last--;
            self._write(sprintf("  %s byte(s) allocated, never freed.\n",
              /[0-9]+/.exec(contents[idx])));
            for(var i=idx; i <= last; i++)
              stack_trace_line(asan_contents[i]);
            idx = last+1;
            self._write("\n");
          }
        }
      }
      else if(/heap-use-after-free /.test(contents[1])) { // use after free
        self._write(sprintf("Using address %s after it has been freed.\n",
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/double-free /.test(contents[1])) { // double free
        self._write(sprintf("Attempting to free address %s, which has already been freed.\n",
          addrpatt.exec(contents[1])));
        stack_trace(contents);
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
        self.write("Program finished with exit code "+io.status);
        if(io.status !== 0 && return_codes[io.status]) {
          self.write(sprintf(" (%s)", return_codes[io.status]));
        }
        self.write(".\n");
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
  }]);
