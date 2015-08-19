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
    }, 50);
  }
}

// TODO: should block everything until the runtime is loaded.
function onInit() {
  console.log('onRuntimeInitialized');
  init = true;
  for(var i=0; i<init_queue.length; i++) {
    block_for_libraries(init_queue[i]);
  }
  init_queue = [];
}

Module = {onRuntimeInitialized:onInit, setStatus:(function (s) {console.log(s);})};

self.importScripts('seashell-clang-js/bin/crt-headers.js');
self.importScripts('seashell-clang-js/bin/seashell-clang.js');

onmessage = function(msg) {
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

  function compile(sources) {
    var cc = Module.seashell_compiler_make();
    for(var i=0; i<sources.length; i++) {
      Module.seashell_compiler_add_file(cc, "/working/"+sources[i]);
    }
    var cres = Module.seashell_compiler_run(cc);
    var diags = diagnostics(cc, sources);
    if(cres === 0) {
      var obj = Module.seashell_compiler_get_object(cc);
      Module.seashell_compiler_free(cc);
      return { messages: diags,
               obj: obj,
               type: "result",
               status: 'running' };
    }
    else {
      Module.seashell_compiler_free(cc);
      return { messages: diags,
               type: "result",
               status: 'compile-failed' };
    }
  }
  
  try {
    FS.mkdir("/working");
  } catch(e) { }
  var sources = [];
  for(var i=0; i<data.files.length; i++) {
    var file = FS.open("/working/"+data.files[i].name, 'w');
    var len = lengthBytesUTF8(data.files[i].contents)+1;
    var arr = new Uint8Array(len);
    var copied = stringToUTF8Array(data.files[i].contents, arr, 0, len);
    FS.write(file, arr, 0, copied);
    FS.close(file);
    sources.push(data.files[i].name);
  }

  if(init) {
    var res = compile(sources);
    postMessage(res);
  }
  else {
    init_queue.push(function() {
      var res = compile(sources);
      postMessage(res);
    });
  }
};
