/* Requires jQuery. */

var seashell_server_prefix = "";

function seashell_k(ss, method, text, callback) {
  $.ajax({
    url:      ss.k[method],
    type:     'POST',
    data:     text,
    dataType: 'json',
    success:  function(data, textStatus, jqXHR) {
      if(typeof(data['k']) === "object") {
        console.log("Updating continuation store after call to " + method);
        seashell_update(ss, data.k);
      } else if(data['k'] === "restart") {
        console.log("Exchanging key after call to " + method);
        badnews = function() {
          alert("Something went wrong. Please try refreshing the page.");
        }
        seashell_new(
          ss['_key_'],
          function(new_ss) {
            for(k in new_ss) {
              ss[k] = new_ss[k];
            }
            return seashell_k(ss, method, text, callback);
          },
          function(error_text) {
            badnews();
          });
      }
      console.log("Call to method " + method + " with args " + text + " returned " + data['val']);
      callback(data['val']);
    }
  });
}

function seashell_update(ss, k) {
  function make_caller(method) {
    return function() {
      var args = Array.prototype.slice.call(arguments);
      callback = args.shift();
      if(typeof(callback) !== "function") {
        console.log("API method called without a callback.");
        return;
      }
      seashell_k(this, method, $.toJSON(args), callback);
    };
  }
  for(c in k) {
    ss.k[c] = seashell_server_prefix + k[c];
  }
  for(x in k) {
    ss[x] = make_caller(x).bind(ss);
  }
}

function seashell_new(ss_key, ss_success, ss_error) {
  var req = $.ajax({
    url:      seashell_server_prefix + "/api/k",
    type:     'POST',
    data:     {key : ss_key},
    dataType: 'json',
    success:  function(data, textStatus, jqXHR) {
      var ss = new Object();
      ss.k = new Object();
      ss._key_ = ss_key;
      seashell_update(ss, data);
      ss_success(ss);
    },
    error:    function(jqXHR, textStatus, errorThrown) {
      ss_error(errorThrown);
    }
  });
}

function seashell_login(ss_success, ss_error, user, passwd) {
  var req = $.ajax({
    url:      seashell_server_prefix + "/api/login",
    type:     'POST',
    data:     $.toJSON({user: user, passwd: passwd}),
    dataType: 'json',
    success:  function(data, textStatus, jqXHR) {
      if(typeof(data['session-key']) !== "string") {
        ss_error("Couldn't authenticate.");
      }
      seashell_new(data['session-key'], ss_success, ss_error);
    },
    error:    function(jqXHR, textStatus, errorThrown) {
      ss_error(errorThrown);
    }
  });
}


