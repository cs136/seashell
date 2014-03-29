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
  if($("font_size") < 6 || $("font_size") > 40){
    displayErrorMessage("Font size must be an integer between 6 and 40");
    return;
  }

  writeSettings({
    font_size : $("#editor_font").val(),
    edit_mode : $("#editor_mode").val(),
    tab_width : $("#tab_width").val(),
    use_space : $("#use-spaces").val()
  });

  refreshSettings(function (){
    $("#settings-dialog").modal("hide");
  }, function (){
    displayErrorMessage("Failed to apply settings.");
  });
}

/**
 * handleCommit
 * This function will handle committing projects.
 */
function handleCommit( ) {
  projectCommit($("#project_comment_description").val());
  $("#commit-project-dialog").modal("hide");
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
 * handleCommitProject
 * This function will handle committing projects */
function handleCommitProject( ) {
  var description = $("#commit_descr").val();
  $("#commit_descr").val("");
  projectCommit(description);
  $("#commit-project-dialog").modal("hide");
}

/**
 * handleUploadFile
 * This handles uploading files.
 */
function handleUploadFile() {
  projectUploadHandler();
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
  $("#button-revert-project").on("click",
      handleRevertProject);
  /** Set up the new-file-dialog. */
  $("#button-new-file").on("click",
      handleNewFile);
  /** Set up the delete-file-dialog. */
  $("#button-delete-file").on("click",
      handleDeleteFile);
  /** Set up the commit-project-dialog */
  $("#button-commit-project").on("click",
      handleCommitProject);
  /** Set up the upload-file-dialog */
  $("#button-upload-file").on("click",
      handleUploadFile);
}

/**
* diplayErrorMessage
* Displays a small dialogue box to show a user an error has occured
*   on the front end */
function displayErrorMessage(msg) {
  $("#error-message-body").text(msg);
  $("#error-message-modal").modal("show");
}

/**
* diplayConfirmationMessage
* Displays a small dialogue box which allows the user to press OK or cancel */
function displayConfirmationMessage(title, msg, okCallback, cancelCallback) {
  $("#confirmation-message-title").text(title);
  $("#confirmation-message-body").text(msg);
  $("#confirmation-message-modal").modal("show");
  $("#confirmation-message-ok").click(function(){
    $("#confirmation-message-modal").modal("hide");
    okCallback();
  });
  $("#confirmation-message-cancel,#confirmation-message-x").click(function(){
    $("#confirmation-message-modal").modal("hide");
    if(cancelCallback) cancelCallback();
  });
}
