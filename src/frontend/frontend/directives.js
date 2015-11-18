/**
 * Seashell's frontend directives
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
  // Directive for binding a mutator watcher (HTML5)
  .directive('whenVisible', ['$parse', function ($parse) {
    return {
      link: function (scope, elem, attrs) {
        var triggered = false;
        scope.$watch(function () {
          if (elem.is(':visible') && !triggered) {
            $parse(attrs.whenVisible)(scope);
            triggered = true;
          }
        });
      }
    };
  }])
  // Directive for binding file uploads.
  .directive('filelistBind', ['$parse', function ($parse) {
    return {
      link: function (scope, elem, attrs) {
        elem.bind('change', function (event) {
          scope.$apply(function () {
            $parse(attrs.filelistBind).assign(scope, event.target.files);
          });
        });
      }
    };
  }])
  .directive('focusOn', ['$timeout', function($timeout) {
     return function(scope, elem, attr) {
        scope.$on(attr.focusOn, function(e) {
            $timeout(function () {elem[0].focus();});
        });
     };
  }])
  .directive('focusMe', function($timeout, $parse) {
     return {
       link: function(scope, element, attrs) {
         var model = $parse(attrs.focusMe);
         scope.$watch(model, function(value) {
           if(value === true) { 
             $timeout(function() {
               element[0].focus(); 
             });
           }
         });
       }; 
 }});
