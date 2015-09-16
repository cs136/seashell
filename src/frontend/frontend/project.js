/**
 *
 * Seashell's frontend project controller
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
  // Project controller.
  .controller("ProjectController", ['$state', '$stateParams', '$scope', 'error-service',
      'openProject', '$cookies', 'NewQuestionModal', 'DeleteProjectModal', 'projects',
    function($state, $stateParams, $scope,  errors, openProject, $cookies, newQuestionModal, deleteProjectModal, projects) {
      var self = this;
      self.state = 'edit-project';
      self.project = openProject;
      self.userid = $cookies.getObject(SEASHELL_CREDS_COOKIE).user;
      self.is_deleteable = projects.isDeleteable(self.project.name);
      self.download = function(){
        openProject.getDownloadToken().then(function (token){
            var raw = JSON.stringify(token);
            var url = sprintf("https://%s:%s/export/%s.zip?token=%s",
                              $cookies.getObject(SEASHELL_CREDS_COOKIE).host,
                              $cookies.getObject(SEASHELL_CREDS_COOKIE).port,
                              encodeURIComponent(openProject.name),
                              encodeURIComponent(raw));

            var ifrm = document.createElement("IFRAME");
            ifrm.setAttribute("src", url);
            ifrm.setAttribute("style", "display:none");
            document.body.appendChild(ifrm);
        });};
        self.newQuestion = function () {
          newQuestionModal(openProject);
        };
        self.close = function () {
          $state.go('list-projects');
        };
        self.delete = function () {
          deleteProjectModal(openProject.name).then(
              function () {$state.go('list-projects');});
        };

      self.project.mostRecentlyUsed()
        .then(function (recent) {
          if (recent && $state.is('edit-project')) {
            $state.go('edit-project.editor',
                      {question: recent},
                      {location: "replace"});
          }
          return recent;
        });
    }]);
