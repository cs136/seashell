/**
 * Seashell's login tools.
 * Copyright (C) 2013-2014 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * createCookie(name, value, days)
 * Creates a new cookie.
 *
 * @param {String} name - Name of the cookie.
 * @param {String} value - Value of the cookie.
 * @param {String} days - Time to live.
 */
function createCookie(name, value, days) {
    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        var expires = "; expires=" + date.toGMTString();
    } else var expires = "";
    document.cookie = escape(name) + "=" + escape(value) + expires + "; path=/";
}

/**
 * readCookie(name)
 * Reads a cookie.
 *
 * @param {String} name - Name of cookie.
 *
 * @return {String/null} Value of the cookie, null if not found.
 */
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

/**
 * eraseCookie(name)
 * Erases a cookie.
 *
 * @param {String} name - Name of cookie.
 */
function eraseCookie(name) {
    createCookie(name, "", -1);
}

/**
 * fixedEncodeURIComponent (str)
 * URI encoding function that wraps encodeURIComponent in a way
 * that produces HTTP POST safe data.
 *
 * @param {String} str - String to encode.
 * @return {String} Encoded string.
 */
function fixedEncodeURIComponent (str) {
    return encodeURIComponent(str)
            .replace(/[!'()]/g, escape)
            .replace(/\*/g, "%2A")
            .replace("%20", "+");
}

/**
 * init_login()
 *
 * Sets up the page.
 */
function init_login() {
  $('#login-username')[0].disabled = false;
  $('#login-password')[0].disabled = false;
  $('#login-submit')  [0].disabled = false;
  $('#login-username')[0].focus();
}

/**
 * submit_login()
 *
 * Submits Seashell login credentials to the launcher CGI executable.
 * Redirects to the main page if successful.
 */
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
               /** Set additional fields. */
               data["user"] = user;

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

/**
 * read_login_credentials()
 *
 * Reads Seashell login credentials.
 *
 * @return {Object} - Login credentials, null if none.
 * As of now [keep this documentation in sync with login-gateway.rkt],
 * the returned object will have three fields:
 *  - key - Array of 4 words, for a 128-bit AES session key.
 *  - port - String, representing the port to connect to.
 *  - host - String, representing the host to connect to.
 */
function read_login_credentials() {
  var creds = readCookie("seashell-session");
  if (creds) {
    return JSON.parse(creds);
  } else {
    return null;
  }
}
