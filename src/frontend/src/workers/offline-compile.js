"use strict";
/*
  This file will be run as a web worker to compile and run code offline.
*/

var init = false;
var init_queue = [];

function block_for_libraries(callback) {
  try {
    FS.readFile("/include/stdio.h");
    callback();
  }
  catch(e) {
    if(e.code != "ENOENT" || e.errno != 2) {
      // in this case we have an actual error to deal with
      postMessage({
        status: 'error',
        err: (e.stack ? e.stack : e.message ? e.message : "Unknown error")
      });
      close();
    }
    else {
      setTimeout(function() {
        block_for_libraries(callback);
      }, 200);
    }
  }
}

function onInit() {
  init = true;
  for(var i=0; i<init_queue.length; i++) {
    block_for_libraries(init_queue[i]);
  }
  init_queue = [];
}

self.Module = {
  onRuntimeInitialized:onInit,
  setStatus:(function (s) {console.log(s);}),
  noExitRuntime: true,
  onExit: function(s) { console.log(s); }
};

self.importScripts(require("file-loader!seashell-clang-js/bin/crt-headers"));
self.importScripts(require("file-loader!seashell-clang-js/bin/seashell-clang"));

self.onmessage = function(msg) {
  var data = msg.data;

  function diagnostics(cc) {
    var res = [];
    var n = Module.seashell_compiler_get_diagnostic_count(cc);
    if(n>0) {
      for(var k=0; k<n; k++) {
        var file = Module.seashell_compiler_get_diagnostic_file(cc, k);
        if(file.startsWith("/seashell/")) {
          res.push([
            Module.seashell_compiler_get_diagnostic_error(cc, k),
            file,
            Module.seashell_compiler_get_diagnostic_line(cc, k),
            Module.seashell_compiler_get_diagnostic_column(cc, k),
            Module.seashell_compiler_get_diagnostic_message(cc, k)
          ]);
        }
      }
    }
    return res;
  }

  function compile(runnerFile, source_dirs) {
    console.log("Attempting to compile " + runnerFile + ".");
    var cc = Module.seashell_compiler_make();
    Module.seashell_compiler_set_main_file(cc, runnerFile);
    for(var i=0; i<source_dirs.length; i++) {
      Module.seashell_compiler_add_source_dir(cc, source_dirs[i]);
    }

    // add compiler flags
    // TODO: import these flags from elsewhere so we don't have them hard coded on both
    //       backend and frontend.
    var flags = ["-Wall", "-Werror=int-conversion", "-Werror=int-to-pointer-cast", "-Werror=return-type",
                 "-Werror=import-preprocessor-directive-pedantic", "-Werror=incompatible-pointer-types",
                 "-O0", "-mdisable-fp-elim", "-fno-common", "-std=c99"];
    for(var ind = 0; ind<flags.length; ind++) {
      Module.seashell_compiler_add_compile_flag(cc, flags[ind]);
    }

    var cres = Module.seashell_compiler_run(cc, false);
    var diags = diagnostics(cc);

    // check for use of .o files
    if(Module.seashell_compiler_get_object_dep_count(cc) > 0) {
      diags = [[false,
        Module.seashell_compiler_get_object_dep(cc, 0),
        0, 0,
        "This program depends on an object file, which is not compatible with Seashell's offline mode."
      ]].concat(diags);
    }
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

  var source_dirs = ["/seashell/"+data.project+"/"+data.question,
                     "/seashell/"+data.project+"/common"];
  
  try {
    FS.mkdir("/seashell");
    FS.mkdir("/seashell/"+data.project);
    FS.mkdir("/seashell/"+data.project+"/"+data.question);
    FS.mkdir("/seashell/"+data.project+"/"+data.question+"/tests");
    FS.mkdir("/seashell/"+data.project+"/common");
  } catch(e) { }
  var rf = "/seashell/"+data.project+"/"+data.runnerFile;
  try {
    for (var i=0; i<data.files.length; i++) {
      console.log("Compiler: Adding file: ", data.files[i]);
      var contents = "";
      if (data.files[i].contents.contents) contents = data.files[i].contents.contents;
      var file = FS.open("/seashell/"+data.project+"/"+data.files[i].name, 'w');
      var len = lengthBytesUTF8(contents)+1;
      var arr = new Uint8Array(len);
      var copied = stringToUTF8Array(contents, arr, 0, len);
      FS.write(file, arr, 0, copied);
      FS.close(file);
    }
  } catch (e) {
    console.error("Compiler: Error caught while adding file", e);
    postMessage({
      status: "error",
      err: e.message + "\n" + e.stack
    });
    close();
  }
  console.log("Compiler: Done adding files");
  if(init) {
    console.log("Compiler ready, compiling.");
    var res = compile(rf, source_dirs);
    postMessage(res);
    close();
  }
  else {
    console.log("Compiler not ready, waiting for header files.");
    init_queue.push(function() {
      var res = compile(rf, source_dirs);
      postMessage(res);
      close();
    });
  }
};
