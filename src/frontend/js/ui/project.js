/**
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
currentProject = null;

/**
 * Sets the current project.
 *
 * @param {String} project - Project to switch to.
 */
function switchToProject(project) {
  
}

/**
 * Updates the list of projects.
 *
 * @param {jQuery} tag - <li class="dropdown"> tag to update wrapped
 *    in a jQuery selector.
 */
function updateListOfProjects(tag) {
  let promise = socket.getProjects();

  promise.done(function(projects) {
    tag.empty();

    tag.append(
      $("<a>",
        {
          "class": "dropdown-toggle",
          "data-toggle": "dropdown",
          "role": "button",
          "text": "Projects"
        }));
    
    projects_tag = $("<ul>",
        {
          "class": "dropdown-menu",
          "aria-labelledby": "projects",
          "role": "menu"
        });

    for(let i = 0; i < projects.length; i++) {
      projects_tag.append(
          $("<li>", {"role": "presentation"})
             .append(
               $("<a>",
                 {
                  "href": "#",
                  "click": function() {
                       switchToProject(projects[i]);}
                  "text": projects[i]})));
    }

    projects_tag.append(
      $("<li>", {"role": "presentation"}).append(
        $("<div>", {"class": "input-group").append(
          $("<span>", {"class": "input-group-btn"}).append(
            $("<button>", {"class":"btn btn-default", "type":"button", "text":"+")))
        .append("<input>", {"type":"text", "class":"form-control", "id":"new-project-name"})));
  });
  promise.fail(seashellError);
}
