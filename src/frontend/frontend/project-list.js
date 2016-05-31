/**
 * Seashell's frontend project list controller
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
  // Controller for Project Lists
  .controller('ProjectListController', ['projectList', 'projects',
      'NewProjectModal', 'DeleteProjectModal', 'error-service',
      function (projectList, projects, newProjectModal, deleteProjectModal, errors) {
    var self = this;
    self.projectList = projectList;
    self.state = "list-projects";

    /** Delete onClick handler. */
    self.delete = function(project) {
      deleteProjectModal(project).then(function () {
        self.projectList.refresh();
      });
    };

    /** New Project Handler */
    self.new = function() {
      newProjectModal().then(function () {
        self.projectList.refresh();
      });
    };

    /** Refresh onClick handler. */
    self.refresh = function () {
      self.projectList.refresh();
    };
    $scope.on("projects-refreshed", function () {
      self.refresh();
    });

    // Tests if project is deleteable
    self.isDeletable = function(project) {
      return ! /^[aA][0-9]+/.test(project);
    };
  }]);
