"use strict";
/*
  Web worker called by the offline compiler worker to actually run the compiled
    object files. Initialized as a shared worker in the main thread for Chrome
    compatibility.
*/

var initialized = false;
var toRun = null;
var pid = null;
var testcase_data = null;

// TODO: Make sure the runtime is initialized before running code.
function onInit() {
  initialized = true;
  if (toRun !== null) {
    runObj(toRun);
  }
}

self.Module = {setStatus: function (s) {console.log(s);}, onRuntimeInitialized: onInit};
self.importScripts(require("file-loader!seashell-clang-js/bin/seashell-runner"));

var stdout = "";
var stderr = "";

Module._RT_stdout_write = function(str) {
  stdout += str;
};

Module._RT_stderr_write = function(str) {
  stderr += str;
};

var req = new XMLHttpRequest();
var runtime = "";
req.open("GET", require("file-loader!seashell-clang-js/bin/seashell-crt.bc"), false);
req.responseType = "arraybuffer";
req.send();
var view = new Uint8Array(req.response);
for(var i=0; i<view.length; i++) {
  runtime += String.fromCharCode(view[i]);
}

function runObj(obj) {
  var runner = new Module.SeashellInterpreter();
  runner.assemble(obj);
  runner.assemble(runtime);
  runner._RT_stdin_buffer = "";

  var running = true;
  
  self.onmessage = function(obj) {
    if (typeof obj.data === "string") {
      Module._RT_stdin_buffer += obj.data;
      stdout += obj.data;
    }
    else
      Module._RT_stdin_buffer = null;
    if (!running)
      run_loop();
  };

  function run_loop() {
    running = true;
    try {
      var loop = runner.run();
    } catch (e) {
      postMessage({message: e.toString(), type: "stderr"});
    }

    // send stdout contents
    if (testcase_data === null) {
      postMessage({message: stdout, type: "stdout"});
      stdout = "";

      // send stderr contents
      postMessage({message: stderr, type: "stderr"});
      stderr = "";
    }


    if (loop) {
      running = false;
    }
    if (!loop) {
      if (testcase_data === null) {
        postMessage({status: runner.result(),
                     pid: pid,
                     type: "done"});
      } else {
        var result;
        if (runner.result() !== 0) {
          result = "error";
        } else if (!testcase_data.expect) {
          result = "no-expect";
        } else if (testcase_data.expect == stdout) {
          result = "passed";
        } else {
          result = "failed";
        }
        postMessage({
          pid: testcase_data.pid,
          result: result,
          stderr: stderr,
          stdout: stdout,
          test_name: testcase_data.test_name,
          diff: testcase_data.expect ? testcase_data.expect.split("\n") : undefined
        });
      }
      close();
    }
  }

  if (testcase_data === null) {
    run_loop();
  } else {
    run_loop();
    Module._RT_stdin_buffer += testcase_data.in;
    if (!running) { run_loop(); }
    Module._RT_stdin_buffer = null;
    if (!running) { run_loop(); }
  }
}

self.onmessage = function(msg) {
  data = msg.data;
  if ("pid" in data) {
    pid = data.pid;
  }
  if (typeof data === "object" && ("type" in data) && data.type === "testdata") {
    testcase_data = data;
  } else {
    if (initialized) {
      runObj(data.obj);
    } else {
      toRun = data.obj;
    }
  }
};
