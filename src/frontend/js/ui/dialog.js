"use strict";
/**
 * Seashell.
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

/** 
 * handleSaveSettings
 * This function will handle saving settings. */
function handleSaveSettings( ) {
  // TODO: implement
}

/** 
 * handleNewFile
 * This function will handle creating new files. */
function handleNewFile( ) {
  fileNew($("#new_file_name").val());
  $("#new-file-dialog").modal("hide");
}

/**
 * handleDeleteFile 
 * This function will handle deleting files. */
function handleDeleteFile( ) {
  fileDelete($("#delete-file-dialog").data("file"));
  $("#delete-file-dialog").modal("hide");
}

/** 
 * handleNewProject
 * This function will handle creating new projects. */
function handleNewProject( ) {
  projectNew($("#new_project_name").val());
  $("#new-project-dialog").modal("hide");
}

/** This function will handle opening projects. */
function handleOpenProject( ) {
  projectOpen($("#projects_list").val());
  $("#open-project-dialog").modal("hide");
}

/** 
 * handleDeleteProject
 * This function will handle deleting projects. */
function handleDeleteProject( ) {
  projectDelete();
  $("#delete-project-dialog").modal("hide");
}

/** 
 * handleRevertProject
 * This function will handle reverting projects. */
function handleRevertProject( ) {
  // TODO: implement
}

/**
 * newFileDialog
 * Pops up the New File Dialog
 */
function newFileDialog( ) {
  $("#new-file-dialog").modal("show");
}

/**
 * deleteFileDialog
 * Pops up the Delete File Dialog
 *
 * @param {String} file Name of file to delete.
 */
function deleteFileDialog(file) {
  $("#delete-file-name").text(file);
  $("#delete-file-dialog").data("file", file).modal("show");
}

/**
 * setupDialogs()
 * Sets up and attaches actions to all the dialogs. */
function setupDialogs() {
  /** Set up the open-project-dialog. */
  $("#open-project-dialog").on("show.bs.modal",
      updateListOfProjects);
  $("#button-open-project").on("click",
      handleOpenProject);
  /** Set up the new-project-dialog. */
  $("#button-new-project").on("click",
      handleNewProject);
  /** Set up the delete-project-dialog. */
  $("#button-delete-project").on("click",
      handleDeleteProject);
  /** Set up the revert-project-dialg. */
  $("#button-delete-project").on("click",
      handleRevertProject);
  /** Set up the delete-file-dialog. */
  $("#button-delete-project").on("click",
      handleDeleteProject);
  /** Set up the new-file-dialog. */
  $("#button-new-file").on("click",
      handleNewFile);
  /** Set up the delete-file-dialog. */
  $("#button-delete-file").on("click",
      handleDeleteFile);
}
