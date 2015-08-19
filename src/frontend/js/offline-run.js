/*
  Web worker called by the offline compiler worker to actually run the compiled
    object files. Initialized as a shared worker in the main thread for Chrome
    compatibility.
*/

// TODO: Make sure the XMLHTTPRequest is completed before running code.
// TODO: Make sure the runtime is initialized before running code.
function onInit() {
  console.log("onRuntimeInitialized");
}
Module = {setStatus: function (s) {console.log(s);}, onRuntimeInitialized: onInit};
self.importScripts("seashell-clang-js/bin/seashell-runner.js");

var req = new XMLHttpRequest();
var runtime = "";
req.open("GET", "seashell-clang-js/bin/seashell-crt.bc", false);
req.responseType = "arraybuffer";
req.send();
var view = new Uint8Array(req.response);
for(var i=0; i<view.length; i++) {
  runtime += String.fromCharCode(view[i]);
}

var stdout = "";

self.onmessage = function(obj) {
  var run = new Module.SeashellInterpreter();
  obj = obj.data;
  run.assemble(obj);
  run.assemble(runtime);
  run.run();
  postMessage({message: stdout,
               type: 'stdout'});
  postMessage({message: run.result(),
               type: 'done'});
  close();
};
