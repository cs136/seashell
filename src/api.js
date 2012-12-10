/* Requires jQuery. */

var seashell_server_prefix = "";

function s_k(api, method, args, callback) {
  $.ajax({
    url:      api.k[method],
    type:     'POST',
    data:     $.toJSON({ key : api['__key__'], args : args }),
    dataType: "json",
    success:  function(data, textStatus, jqXHR) {
      if(typeof(data) !== "object" || data['status'] !== true) {
        if(data['access'] === false) {
          console.log("Access denied for method " + method);
          return;
        }
        seashell_new(
          function(new_api) {
            if(api['__retry__'] === undefined) {
              api['__retry__'] = 
                (function(method, args, callback) {
                  return function() {
                    delete api['__retry__'];
                    s_k(api, method, args, callback);
                  }
                 })(method, args, callback);
            } else {
              api['__key__'] = new_api['__key__'];
            }
            for(c in new_api['k']) {
              api['k'][c] = new_api['k'][c];
            }
            api.isValidSession(function(){api.__retry__()});
          },
          function(error) {
            console.log(error);
            alert("There has been an error processing your request. Please refresh the page.");
        });
      } else {
        callback(data['result']);
      }
    },
    error:  (function(api, method, args, callback){
      return function(data, textStatus, jqXHR) {
        if(textStatus == "timeout") {
          s_k(api, method, args, callback);
        } else {
          console.log("Error during method " + method + " invocation: " + textStatus);
          throw new Array(method, textStatus);
        }
      }})(api, method, args, callback)
  });
}

function s_update(api, data) {
  function make_caller(method) {
    return function() {
      var args = Array.prototype.slice.call(arguments);
      callback = args.shift();
      if(typeof(callback) !== "function") {
        console.log("API method called without a callback.");
        return;
      }
      s_k(this, method, args, callback);
    };
  }
  if(typeof(data['key']) !== "string") {
    return false;
  }
  for(c in data['k']) {
    api.k[c] = seashell_server_prefix + data['k'][c];
    api[c] = make_caller(c).bind(api);
  }
  api['__key__'] = data['key'];
  return true;
}

function seashell_new(success_call, error_call) {
  var req = $.ajax({
    url:      seashell_server_prefix + "/api/init",
    dataType: "json",
    success:  function(data, textStatus, jqXHR) {
      var api = new Object();
      api.k = new Object();
      if(!s_update(api, data)) {
        error_call("Could not initialize session.");
      }
      success_call(api);
    },
    error:    function(jqXHR, textStatus, errorThrown) {
      console.log(textStatus);
      error_call(textStatus);
    }
  });
}

