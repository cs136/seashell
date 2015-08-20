/*
  Web worker called by the offline compiler worker to actually run the compiled
    object files. Initialized as a shared worker in the main thread for Chrome
    compatibility.
*/

var initialized = false;
var toRun = null;

// TODO: Make sure the runtime is initialized before running code.
function onInit() {
  initialized = true;
  if(toRun !== null) {
    runObj(toRun);
  }
}

Module = {setStatus: function (s) {console.log(s);}, onRuntimeInitialized: onInit};
self.importScripts("seashell-clang-js/bin/seashell-runner.js");

var stdout = "";

Module._RT_stdout_write = function(str) {
  stdout += str;
};

var req = new XMLHttpRequest();
var runtime = "";
req.open("GET", "seashell-clang-js/bin/seashell-crt.bc", false);
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
  runner.run();
  postMessage({message: stdout,
               type: 'stdout'});
  postMessage({status: runner.result(),
               type: 'done'});
  close();
}

self.onmessage = function(obj) {
  obj = obj.data;
  if(initialized)
    runObj(obj);
  else
    toRun = obj;
};
