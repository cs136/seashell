/*
 * Seashell's front-end.
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
angular.module('marmoset-bindings', ['ngCookies'])
  .service('marmoset', ['$q', '$cookies', '$http',
      function ($q, $cookies, $http) {
        "use strict";
        var self = this;
        var list_url = "https://student.cs.uwaterloo.ca/~cs136/cgi-bin/marmoset-utils/project-list.rkt";
        var test_url = "https://student.cs.uwaterloo.ca/~cs136/cgi-bin/marmoset-utils/public-test-results.rkt";
        var projects_loaded = false;
        var project_list = [];
        self.show_all_projects = false;

        /**
         * Refreshs project list.
         */
        self.refresh = function() {
          // If user is white listed, then show all Marmoset projects
          return $http({url: list_url, params: {show_all: self.show_all_projects}})
            .then(function (results) {
              project_list = results.data;
              projects_loaded = true;
            });
        };
        /** Load projects (initially) */
        self.refresh();

        /**
         * List projects.
         *
         * @returns
         *  a promise resolved with list of project names ie. A1P2
         */
        self.projects = function () {
          if(projects_loaded) {
            var def = $q.defer();
            def.resolve(_.map(project_list, function (x) {return x.project;}));
            return def.promise;
          }
          else {
            return self.refresh().then(function() {
              return $q.when(_.map(project_list, function(x) {return x.project;}));
            });
          }
        };

        /**
         * Fetches project marmoset test results.
         *
         * @param {String} project - Marmoset Project to fetch results for.
         * @returns Angular promise that resolves to:
         *  a list of:
         *    objects containing:
         *      Marmoset results for the specified project.
         */
        self.results = function(project) {
          return $http({url: test_url,
                             params: {user: $cookies.getObject(SEASHELL_CREDS_COOKIE).user,
                                      project: project}})
                 .then(function (result) {
                   return result.data;
                 });
        };
     }]);
