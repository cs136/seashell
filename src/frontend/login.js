function createCookie(name, value, days) {
    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        var expires = "; expires=" + date.toGMTString();
    } else var expires = "";
    document.cookie = escape(name) + "=" + escape(value) + expires + "; path=/";
}

function readCookie(name) {
    var nameEQ = escape(name) + "=";
    var ca = document.cookie.split(';');
    for (var i = 0; i < ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) == ' ') c = c.substring(1, c.length);
        if (c.indexOf(nameEQ) == 0) return unescape(c.substring(nameEQ.length, c.length));
    }
    return null;
}

function eraseCookie(name) {
    createCookie(name, "", -1);
}

function fixedEncodeURIComponent (str) {
    return encodeURIComponent(str)
            .replace(/[!'()]/g, escape)
            .replace(/\*/g, "%2A")
            .replace("%20", "+");
}

function init_login() {
  $('#login-username')[0].disabled = false;
  $('#login-password')[0].disabled = false;
  $('#login-submit')  [0].disabled = false;
  $('#login-username')[0].focus();
}

function submit_login() {
  var user = $('#login-username')[0].value;
  var pass = $('#login-password')[0].value;
  var upload = "u=" + fixedEncodeURIComponent(user)
               + "&p=" + fixedEncodeURIComponent(pass);
  var target = "https://" + document.location.host
              + document.location.pathname.substring
                  (0, document.location.pathname.lastIndexOf('/'))
              + "/cgi-bin/login.cgi";
  $.post(target,
         upload,
         function(data, textStatus, jqXHR) {
           if(typeof(data) == "object") {
             if(data.error !== undefined) {
               alert("Login error: " + data.error.message + " (code " + data.error.code + ")");
             } else if(data.port !== undefined) {
               createCookie("seashell-session", JSON.stringify(data), 365);
               console.log("All done login.");
               top.location = "frontend.html";
             } else {
               alert("Internal error (1).");
             }
           } else {
             alert("Internal error (2).");
           }
         });
}
