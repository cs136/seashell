"use strict";
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
var currentFiles = null;
var currentProject = null;

/**
 * Updates the list of projects.
 */
function updateListOfProjects() {
  var promise = socket.getProjects();

  /** Fill the selector */
  promise.done(function(projects) {
    var projects_tag = $("#projects_list");

    projects_tag.empty();
    for (var i = 0; i < projects.length; i++) {
      projects_tag.append(
        $("<option>").attr("value", projects[i]).text(projects[i]));
    }
  }).fail(function(){
    // TODO: Error handling.
  });
}

/**
 * Saves everything in the current project.
 */
function saveProject() {
  for (var file in currentFiles) {
    if (currentFiles[file] != null) {
      socket.writeFile(currentProject, file, currentFiles[file].getValue())
        .fail(function() {
          // TODO: Error handling.
        });
    }
  }
}

/**
 * Opens and sets the current project.
 * @param {String} project Name of project to open.
 */
function projectOpen(name) {
  var promise = socket.listProject(name);

  /** Update the list of files. */
  promise.done(function(files) {
    currentFiles = {};
    currentProject = name;

    for (var i = 0; i < files.length; i++) {
      /** Lazily load the documents later. */
      currentFiles[files[i]] = null;
    }
  }).fail(function(){
    // TODO: error handling.
  });
}

/** 
 * Creates and opens a project.
 * @param {String} project Name of project to create.
 */
function projectNew(name) {
  var promise = socket.newProject(name);

  /** Open it. */
  promise.done(function() {
    projectOpen(name);
  }).fail(function() {
    // TODO: error handling.
  });
}

/**
 * Delete and closes the current project.
 */
function projectDelete(name) {
  var promise = socket.deleteProject(currentProject);

  /** Deal with it. */
  promise.done(function() {
  }).fail(function() {
    // TODO: error handling.
  });
}
function projectRun(){}
function projectLoadNextFile(){}
function projectLoadPreviousFile(){}
