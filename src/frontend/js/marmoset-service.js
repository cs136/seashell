/*
 * Seashell's front-end.
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
angular.module('marmoset-bindings', ['jquery-cookie'])
  .service('marmoset', ['$q', 'cookieStore',
      function ($q, cookieStore) {
        "use strict";
        var self = this;
        var list_url = "https://www.student.cs.uwaterloo.ca/~cs136/cgi-bin/marmoset-utils/project-list.rkt";
        var test_url = "https://www.student.cs.uwaterloo.ca/~cs136/cgi-bin/marmoset-utils/public-test-results.rkt"; 
        var project_list = [];

        /**
         * Refreshs project list.
         */
        self.refresh = function() {
          $q.when($.ajax({url: list_url,
                          dataType: "json"}))
            .then(function (projects) {
              project_list = projects;
            });
        };
        /** Load projects (initially) */
        self.refresh();

        /**
         * List projects.
         *
         * @returns
         *  a list of project names:
         */
        self.projects = function () {
          return _.map(project_list, function (x) {return x.project;});
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
          return $q.wrap($.ajax({url: test_url,
                                 data: {user: cookieStore.get("seashell-session").user,
                                       project: project},
                                 dataType: "json"}));
        };
     }]);
