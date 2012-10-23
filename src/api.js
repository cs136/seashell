/* Requires jQuery. */

function seashell_k(ss, k, text) {
  $.ajax({
    url:      k,
    data:     text,
    dataType: "json",
    success:  function(data, textStatus, jqXHR) {
      if(typeof(data['k'] === "object")) {
        seashell_update(ss, data.k);
      }
    }
  });
}

function seashell_update(ss, k) {
  function make_caller(method) {
    return function() { seashell_k(this, this.k[method], $.toJSON(arguments)); };
  }
  for(c in k) {
    ss.k[c] = k[c];
  }
  for(x in k) {
    ss[x] = make_caller(x).bind(ss);
  }
}

function seashell_new(ss_success, ss_error) {
  var req = $.ajax({
    url:      "http://localhost:9876/api/new",
    dataType: "json",
    success:  function(data, textStatus, jqXHR) {
      var ss = new Object();
      ss.k = new Object();
      seashell_update(ss, data);
      ss_success(ss);
    },
    error:    function(jqXHR, textStatus, errorThrown) {
      ss_error(errorThrown);
    }
  });
}

