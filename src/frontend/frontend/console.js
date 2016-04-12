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
      console.warn("ASAN output:", contents);
      var traced_main = false;
      var filepatt = /\/([^\/]+(:[0-9]+|[^\)]+))\)?$/;
      var addrpatt = /0x[0-9a-f]+/;
      // 0xff is ... 3 bytes ... left ... 10-byte region
      var suppallocaddrpatt = /^(0x[0-9a-f]+)(?:[^0-9])*([0-9]+).*(left|right|inside)(?:[^0-9])*([0-9]+)-byte/;
      // .... 0xff is located in stack ...
      var suppstackaddrpatt = /(0x[0-9a-f]+).*located in stack.*offset ([0-9]+)/;
      // 0xff is ... 0 bytes ... left ... global variable 'x' defined in 'foo.c:9:15' (0xff) of size 10
      var suppglobaladdrpatt = /^(0x[0-9a-f]+)(?:[^0-9])*([0-9]+).*(left|right|inside).*global variable '([^']+)'.*'([^':]+):([0-9]+):([0-9]+)' \((0x[0-9a-f]+)\).*size ([0-9]+)$/;
      // frame ... 2 object(s)
      var frameinfopatt = /frame(?:[^0-9])*([0-9]+) object/;
      // [32, 40) 'x'
      var framevarpatt = /\[([0-9]+), ([0-9]+)\) '([^']+)'/;

      function stack_trace_line(line) {
        try {
          if(/^{/.test(line)) {
            var json = line.replace(/'/g, '"');
            var frame = JSON.parse(json);
            var short_module = frame.module && frame.module.split('/').pop();
            var short_file = frame.file && frame.file.split('/').pop();
            traced_main = traced_main || (frame.function === "main");
            if (frame.frame === 0) {
              /** Reset the trace status when tracing a new frame. */
              traced_main = false;
            }

            if (frame.function !== "<null>" && frame.file !== "<null>") {
              if (!traced_main || (frame.function !== "_start" && frame.function !== "__libc_start_main"))
                self._write(sprintf("  frame %d: %s, %s:%d:%d\n",
                  frame.frame,
                  frame.function,
                  short_file,
                  frame.line,
                  frame.column));
            } else if (frame.function !== "<null>") {
              if (!traced_main || (frame.function !== "_start" && frame.function !== "__libc_start_main"))
                self._write(sprintf("  frame %d: %s, from module %s (+%x)\n", frame.frame, frame.function, short_module, frame.offset));
            } else {
              self._write(sprintf("  frame %d: module %s (+%x)\n", frame.frame, short_module, frame.offset));
            }
          } else if (/^(Direct|Indirect)/.test(line)) {
            self._write(sprintf("\n  %s byte(s) allocated, never freed.\n",
              /[0-9]+/.exec(line)));
          } else if (/^WRITE/.test(line)) {
            var sizeWrite = /size ([0-9]+)/.exec(line)[1];
            var addrWrite = addrpatt.exec(line)[0];
            self._write(sprintf("  Error caused by write of size %d byte(s) to %s:\n", sizeWrite, addrWrite));
          } else if (/^READ/.test(line)) {
            var sizeRead = /size ([0-9]+)/.exec(line)[1];
            var addrRead = addrpatt.exec(line)[0];
            self._write(sprintf("  Error caused by read of size %d byte(s) to %s:\n", sizeRead, addrRead));
          } else if (suppallocaddrpatt.test(line)) {
            var suppAllocAddrInfo = suppallocaddrpatt.exec(line);
            self._write(sprintf("\n  %s is %s bytes %s of %s-byte region ", suppAllocAddrInfo[1], suppAllocAddrInfo[2], suppAllocAddrInfo[3], suppAllocAddrInfo[4]));
          } else if (/^allocated by/.test(line)) {
            self._write("allocated by:\n");
          } else if (suppstackaddrpatt.test(line)) {
            var stackAddressInfo = suppstackaddrpatt.exec(line);
            self._write(sprintf("\n  %s is contained %s bytes into stack frame:\n", stackAddressInfo[1], stackAddressInfo[2]));
          } else if (frameinfopatt.test(line)) {
            var numFrameObjects = frameinfopatt.exec(line)[1];
            self._write(sprintf("\n  This frame has %s object(s):\n", numFrameObjects));
          } else if (framevarpatt.test(line)) {
            var frameVarInfo = framevarpatt.exec(line);
            var objectSize = parseInt(frameVarInfo[2]) - parseInt(frameVarInfo[1]);
            self._write(sprintf("  %d byte object %s located %s bytes into frame.", objectSize, frameVarInfo[3], frameVarInfo[1]));
            if (/overflow/.test(line)) {
              self._write("  Access overflew this variable.\n");
            } else if (/underflow/.test(line)) {
              self._write("  Access underflew this variable.\n");
            } else {
              self._write("\n");
            }
          } else if (/^previously allocated/.test(line)) {
            self._write("\n  Allocated by:\n");
          } else if (/^freed by/.test(line)) {
            self._write("freed already by:\n");
          } else if(suppglobaladdrpatt.test(line)) {
            var globalAddrInfo = suppglobaladdrpatt.exec(line);
            var shortAddrFileInfo = globalAddrInfo[5].split('/').pop();
            self._write(sprintf("\n  %s is %s bytes %s of %s byte(s) global variable %s (located %s) defined at %s:%s:%s.\n",
                                globalAddrInfo[1],
                                globalAddrInfo[2],
                                globalAddrInfo[3],
                                globalAddrInfo[9],
                                globalAddrInfo[4],
                                globalAddrInfo[8],
                                shortAddrFileInfo,
                                globalAddrInfo[6],
                                globalAddrInfo[7]));
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
      else if(/global-buffer-(over|underflow)/.test(contents[1])) {
        var globalErrorType = /(overflow|underflow)/.exec(contents[1])[1];
        self._write(sprintf("Global buffer %s on address %s. Check array indices.\n",
          globalErrorType,
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/stack-buffer-(over|under)flow /.test(contents[1])) { // stack buffer overflow
        var stackErrorType = /(overflow|underflow)/.exec(contents[1])[1];
        self._write(sprintf("Stack buffer %s on address %s. Check array indices.\n",
          stackErrorType,
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/heap-buffer-(over|under)flow /.test(contents[1])) { // heap buffer overflow
        var heapErrorType = /(overflow|underflow)/.exec(contents[1])[1];
        self._write(sprintf("Heap buffer %s on address %s. Check indices used for dynamically allocated arrays.\n",
          heapErrorType,
          addrpatt.exec(contents[1])));
        stack_trace(contents);
      }
      else if(/LeakSanitizer:/.test(contents[1])) { // memory leak
        self._write("Memory leaks occurred:");
        stack_trace(contents);
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
      } else if(/attempting free on/.test(contents[1])) { // free, not malloc
        self._write(sprintf("Attempting to free address %s, which not been malloc'd.\n",
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
        if (self.asan_parse) {
          parse_asan_output(asan_contents);
          self.asan_parse = false;
          asan_contents = [];
        }
        if (io.sanitizerMsg) {
         self.write(io.sanitizerMsg);
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
      if(res.result==="passed") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test %s passed.\n", res.test_name));
      }
      else if(res.result==="failed") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test %s failed.\n", res.test_name));
        self.write('Produced output (stdout):\n');
        self.write(res.stdout);
        self.write('\n'); 
        // need to print a newline so that it matches up with printExpectedFromDiff
        self.write('---\n');
        self.write('Expected output (stdout):\n');
        printExpectedFromDiff(res);   
        self.write('---\n');
        self.write('Produced errors (stderr):\n');
        self.write(res.stderr);
        self.write('\n');
      } else if(res.result==="error") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test %s caused an error (with return code %d)!\n", res.test_name, res.exit_code));
        self.write('Produced output (stderr):\n');
        self.write(res.stderr);
        self.write('\n');
        self.write('---\n');
        self.write(res.sanitizerMsg);
      } else if(res.result==="no-expect") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test %s produced output (stdout):\n", res.test_name));
        self.write(res.stdout);
        self.write('Produced output (stderr):\n');
        self.write(res.stderr);
        self.write('\n');
      } else if(res.result==="timeout") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test %s timed out.\n", res.test_name));
      }
      else if(res.result==="killed") {
        self.write('----------------------------------\n');
        self.write(sprintf("Test %s was killed.\n", res.test_name));
      }
    });

    self.setRunning = function(projectName, questionName, PIDs, testing) {
      self.running = !testing;
      self.PIDs = PIDs;
      _.each(self.PIDs, function (pid) {
        socket.socket.startIO(projectName, questionName, pid);
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
