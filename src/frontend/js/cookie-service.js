/**
 * Angular bindings for $.cookie
 * Copyright (C) 2013-2015 The Seashell Maintainers.
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
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */
angular.module('jquery-cookie', [])
  .service('cookie', function () {
    var self = this;
    self.add = $.cookie;
    self.remove = $.removeCookie;
    self.get = $.cookie;
  })
  .service('cookieStore', ['cookie', function(cookie) {
    var self = this;
    self.add = function(name, value, opts) {
      cookie.add(name, JSON.stringify(value), opts);
    };
    self.remove = cookie.remove;
    self.get = function(name) {
      var c = cookie.get(name);
      return c ? JSON.parse(c) : {};
    };
  }]);
