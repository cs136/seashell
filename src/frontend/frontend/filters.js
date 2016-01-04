/**
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* jshint supernew: true */

angular.module('frontend-app')
  .filter('projectFilter', function() {
    return function(input, type) {
      var pattAssn = new RegExp('^A[0-9]+$');
      var pattTut = new RegExp('^Tut([0-9]+|[0-9]+Sol)$');
      var pattLec = new RegExp('^Lec[0-9]+$');

      var out = [];
      var pattern;
      var isNone = false;
      switch (type) {
        case 'A':
          pattern = pattAssn;
          break;
        case 'LEC':
          pattern = pattLec;
          break;
        case 'TUT':
          pattern = pattTut;
          break;
        case 'NONE':
          isNone = true;
      }

      out = _.filter(_.map(input, function(x) {
          return x[0]
        }),
        function(name) {
          var assnMatch = pattAssn.test(name);
          var lecMatch = pattLec.test(name);
          var tutMatch = pattTut.test(name);

          if (isNone && !assnMatch && !lecMatch && !tutMatch) {
            return true;
          } else if (!isNone && pattern.test(name)) {
            return true;
          } else {
            return false;
          }
        });

      return out;
    };
  })
  .filter('escapeFilter', function() {
    return window.escape;
  });
