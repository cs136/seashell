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
  "use strict";
  if (days) {
      var date = new Date();
      date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
      var expires = "; expires=" + date.toGMTString();
  } else var expires = "";
  document.cookie = encodeURIComponent(name) + "=" + encodeURIComponent(value) + expires + "; secure";
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
  "use strict";
  var nameEQ = encodeURIComponent(name) + "=";
  var ca = document.cookie.split(';');
  for (var i = 0; i < ca.length; i++) {
      var c = ca[i];
      while (c.charAt(0) == ' ') c = c.substring(1, c.length);
      if (c.indexOf(nameEQ) == 0) return decodeURIComponent(c.substring(nameEQ.length, c.length));
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
  "use strict";
  createCookie(name, "", -1);
}
