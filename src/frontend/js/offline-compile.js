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
        type: 'error',
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

// TODO: should block everything until the runtime is loaded.
function onInit() {
  init = true;
  for(var i=0; i<init_queue.length; i++) {
    block_for_libraries(init_queue[i]);
  }
  init_queue = [];
}

Module = {
  onRuntimeInitialized:onInit,
  setStatus:(function (s) {console.log(s);}),
  noExitRuntime: true,
  onExit: function(s) { console.log(s); }
};

self.importScripts('seashell-clang-js/bin/crt-headers.js');
self.importScripts('seashell-clang-js/bin/seashell-clang.js');

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
    FS.mkdir("/seashell/"+data.project+"/common");
  } catch(e) { }
  var rf = "/seashell/"+data.project+"/"+data.runnerFile;
  for(var i=0; i<data.files.length; i++) {
    if(data.files[i].contents) {
      var file = FS.open("/seashell/"+data.project+"/"+data.files[i].name, 'w');
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
    var res = compile(rf, source_dirs);
    postMessage(res);
    close();
  }
  else {
    init_queue.push(function() {
      var res = compile(rf, source_dirs);
      postMessage(res);
      close();
    });
  }
};
