/*
  Web worker called by the offline compiler worker to actually run the compiled
    object files. Initialized as a shared worker in the main thread for Chrome
    compatibility.
*/

var initialized = false;
var toRun = null;

console.log("offline-run thread begin");

// TODO: Make sure the runtime is initialized before running code.
function onInit() {
  initialized = true;
  console.log("onRuntimeInitialized");
  if(toRun !== null) {
    run(toRun);
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

function run(obj) {
  console.log("run()");
  var runner = Module.SeashellInterpreter();
  runner.assemble(obj);
  runner.assemble(runtime);
  runner.run();
  postMessage({message: stdout,
               type: 'stdout'});
  postMessage({message: run.result(),
               type: 'done'});
  close();
}

self.onmessage = function(obj) {
  obj = obj.data;
  if(initialized)
    run(obj);
  else
    toRun = obj;
};
