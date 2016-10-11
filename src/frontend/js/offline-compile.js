/*
  This file will be run as a web worker to compile and run code offline.
  TODO implement running tests with the compiled code. Do this after we
     have the offline FS implemented.
*/

var init = false;
var init_queue = [];


function block_for_libraries(callback) {
  try {
    FS.readFile("/include/stdio.h");
    callback();
  }
  catch(e) {
    setTimeout(function() {
      block_for_libraries(callback);
    }, 200);
  }
}

// TODO: should block everything until the runtime is loaded.
function onInit() {
  init = true;
  for(var i=0; i<init_queue.length; i++) {
    block_for_libraries(init_queue[i]);
  }
  init_queue = [];
}

Module = {onRuntimeInitialized:onInit, setStatus:(function (s) {console.log(s);})};

self.importScripts('seashell-clang-js/bin/crt-headers.js');
self.importScripts('seashell-clang-js/bin/seashell-clang.js');

self.onmessage = function(msg) {
  var data = msg.data;

  function diagnostics(cc, files) {
    var res = [];
    for(var i=0; i<files.length; i++) {
      var n = Module.seashell_compiler_get_diagnostic_count(cc, i);
      if(n>0) {
        for(var k=0; k<n; k++) {
          res.push([
            Module.seashell_compiler_get_diagnostic_error(cc, i, k),
            files[i],
            Module.seashell_compiler_get_diagnostic_line(cc, i, k),
            Module.seashell_compiler_get_diagnostic_column(cc, i, k),
            Module.seashell_compiler_get_diagnostic_message(cc, i, k)
          ]);
        }
      }
    }
    return res;
  }

  function compile(runnerFile) {
    console.log("compile called");
    var pp = Module.seashell_preprocessor_make();
    Module.seashell_preprocessor_set_main_file(pp, "/working/question/"+runnerFile);
    console.log("Running preprocessor...");
    var pres = Module.seashell_preprocessor_run(pp);
    console.log("Preprocessor finished.");
    // TODO handle this error case better.. return an actual message
    if(pres !== 0) {
      Module.seashell_preprocessor_free(pp);
      return { messages: [],
               type: "result",
               pid: -1,
               status: "compile-failed" };
    }
    var cc = Module.seashell_compiler_make();
    var numDeps = Module.seashell_preprocessor_get_include_count(pp);
    var sources = [];
    for(var ind = 0; ind < numDeps; ind++) {
      var source = Module.seashell_preprocessor_get_include(pp, ind);
      sources.push(source);
      console.log("Adding "+source+" to compiler");
      Module.seashell_compiler_add_file(cc, source);
    }
    Module.seashell_preprocessor_free(pp);

    var cres = Module.seashell_compiler_run(cc, false);
    var diags = diagnostics(cc, sources);
    if(cres === 0) {
      var obj = Module.seashell_compiler_get_object(cc);
      Module.seashell_compiler_free(cc);
      return { messages: diags,
               obj: obj,
               type: "result",
               pid: -1,
               status: 'running' };
    }
    else {
      Module.seashell_compiler_free(cc);
      return { messages: diags,
               type: "result",
               pid: -1,
               status: 'compile-failed' };
    }
  }
  
  try {
    FS.mkdir("/working");
    FS.mkdir("/working/question");
  } catch(e) { }
  var rf = data.runnerFile;
  for(var i=0; i<data.files.length; i++) {
    // TODO handle common files in the way expected by dependency resolution code
    if(data.files[i].contents) {
      var split = data.files[i].name.split(".");
      if(split[split.length-1] == "ll") {
        console.log(data.files[i].name, data.files[i].contents);
      }
      var file = FS.open("/working/question/"+data.files[i].name, 'w');
      var len = lengthBytesUTF8(data.files[i].contents)+1;
      var arr = new Uint8Array(len);
      var copied = stringToUTF8Array(data.files[i].contents, arr, 0, len);
      FS.write(file, arr, 0, copied);
      FS.close(file);
    }
    else {
      console.warn("Binary file "+data.files[i].name+" ignored by offline compiler.");
    }
  }

  if(init) {
    var res = compile(rf);
    postMessage(res);
    close();
  }
  else {
    init_queue.push(function() {
      var res = compile(rf);
      postMessage(res);
      close();
    });
  }
};
