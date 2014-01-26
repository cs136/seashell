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
  promise = socket.getProjects();
}

/**
 * Creates a new file in this project.
 */
function projectNewFile() {
}
function newFileDialog() {
}

/**
 * Runs the project.
 */
function projectRun() {
}

/**
 * Switches to the next file.
 */
function projectLoadNextFile() {
}

/**
 * Switches to the previous file.
 */
function projectLoadPreviousFile() {
}
