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
  if(!($("#editor_font").val() >= 6 && $("#editor_font").val() <= 40)){
    displayErrorMessage("Font size must be an integer between 6 and 40");
    return;
  }

  writeSettings({
    font_size  : $("#editor_font").val(),
    edit_mode  : $("#editor_mode").val(),
    tab_width  : $("#tab_width").val(),
    text_style : $("#text_style").val()
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
 * handleNewFolder
 * Handles creating a new directory in the current project
 */
function handleNewFolder() {
  var name = $("#new_folder_name").val().split('/')[0];
  var prom = SeashellProject.currentProject.createDirectory(name);
  if(prom) prom.done(function() {
    var p = SeashellProject.currentProject;
    updateQuestionsMenu(p);
    updateFileMenu();
  });
  $("#new-folder-dialog").modal("hide");
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
  var name = $("#new_project_name").val();
  SeashellProject.new(name).done(function() {
    handleOpenProject(name);
    updateListOfProjects();
  });
  $("#new-project-dialog").modal("hide");
}

/** This function will handle opening projects. */
function handleOpenProject(name) {
  name = name ? name : $("#projects_list").val();
  console.log("Opening project "+name);
  SeashellProject.open(name, function(proj, files) {
    $(".show-on-null-project, .hide-on-null-file").addClass("hide");
    $(".hide-on-null-project, .show-on-null-file").removeClass("hide");

    $("#project-menu").text(name);

    consoleRefresh();
    updateFileMenu();
    updateQuestionsMenu(proj);
    updateProjectsDropdown();

    $("#open-project-dialog").modal("hide");
  });
}

/**
 * handleDeleteProject
 * This function will handle deleting projects. */
function handleDeleteProject( ) {
  if(SeashellProject.currentProject) {
    SeashellProject.currentProject.remove(function() {
      updateListOfProjects();
      $(".hide-on-null-project").addClass("hide");
      $(".show-on-null-project").removeClass("hide");
    });
  }
  $("#delete-project-dialog").modal("hide");
}

/**
 * handleRevertProject
 * This function will handle reverting projects. */
function handleRevertProject( ) {
  // TODO: implement
}

function selectDefaultFileDialogFolder() {
  var p = SeashellProject.currentProject;
  var default_folder =
    p.currentFile ? p.currentFile.name[p.currentFile.name.length - 2] :
                    p.currentQuestion;
  $('#new-file-folder option').filter(function() {
    return $(this).text() == default_folder;
  }).prop('selected', true);
}

function showNewFileDialog() {
  var p = SeashellProject.currentProject;
  selectDefaultFileDialogFolder();
  $('#upload-file-row').show();
  $("#file-to-upload").val('');
  $('#new_file_name').val('');
  $('#new-file-label').text('add file');
  $("#button-new-file")
    .unbind('click')
    .on("click", function() {
      $("#new-file-dialog").modal("hide");
      var folder = $('#new-file-folder option:selected').text();
      if ('tests' == folder)
        folder = p.currentQuestion + '/' + folder;
      if ($("#file-to-upload").val().length)
      {
        handleUploadFile(folder);
        return;
      }
      var prom =
        p.createFile(sprintf('%s/%s', folder, $("#new_file_name").val()));
      if (prom)
        prom.done(updateFileMenu);
    });
  $('#new-file-dialog').modal('show');
}

function showMarmosetDetailsDialog() {
  $("#marmoset-details-dialog").modal("show");
}

function showRenameMoveFileDialog() {
  var p = SeashellProject.currentProject;
  var file = p.currentFile;
  $('#upload-file-row').hide();
  $('#new_file_name').val(_.last(file.name));
  $('#new-file-label').text('rename/move file');
  selectDefaultFileDialogFolder();
  $('#button-new-file')
    .unbind('click')
    .on("click", function() {
      var dirname = $('#new-file-folder option:selected').text();
      if (dirname == 'tests') {
        dirname = p.currentQuestion + '/' + dirname;
      }
      var new_fname = _.last($("#new_file_name").val().split('/'));
      p.renameFile(file, sprintf('%s/%s', dirname, new_fname))
        .done(updateFileMenu);
      $("#new-file-dialog").modal("hide");
    });
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
 *
 * @param {String} folder - Folder to upload into
 */
function handleUploadFile(folder) {
  folder = folder || $('#new-file-folder option:selected').text();

  var filename =
        sprintf('%s/%s',
                folder,
                $("#file-to-upload").val().replace(/.*(\/|\\)/, ''));
  for(var file in SeashellProject.currentProject.files){
    if(file.name == filename){
      displayErrorMessage("File '" + filename + "' already exists.");
      return false;
    }
  }
  socket.getUploadFileToken(SeashellProject.currentProject.name, filename)
    .done(function (token) {
      var raw = JSON.stringify(token);
      var options = {
        target: null,
        dataType: null,
        error: function() {
          displayErrorMessage("File could not be successfully uploaded.");
        },
        success: function() {
          SeashellProject.currentProject.onUploadSuccess(filename);
          SeashellProject.currentProject.openFilePath(filename)
            .done(updateFileMenu);
        },
        data: {token: raw},
        url: sprintf("https://%s:%s/upload", creds.host, creds.port)
      };
      $("#upload-file-form").ajaxSubmit(options);
    }).fail(function() {
      displayErrorMessage("Error retrieving file upload ticket.");
    });
  return false;
}

function updateListOfProjects() {
  var num_in_col = 15; // constant controlling the number of projects listed per column

  SeashellProject.getListOfProjects().done(function(projects) {
    var pdiv = $("#project-list");
    if(projects) {
      var html = "<table>";
      var w = projects.length / num_in_col;
      for(var i=0; i < num_in_col; i++) {
        html += "<tr>";
        for(var j=0; j < w && (num_in_col*j + i < projects.length); j++) {
          html += "<td><a class='project-list-item' href='#'>"+projects[(num_in_col*j)+i]+"</a></td>";
        }
        html += "</tr>";
      }
      html += "</table>";
      pdiv.html(html);
      $("a.project-list-item").click(function() {
        handleOpenProject($(this).text());
      });
    }
    else {
      pdiv.html("<p><a href='#new-project-dialog' data-toggle='modal' class='btn btn-primary btn-lg' role='button'>Create</a> a project to get started!</p>");
    }
  }).fail(function() {
    displayErrorMessage("List of projects could not be updated.");
  });
}

/**
 * handleMarmosetSubmit
 * This handles submitting files to marmoset.
*/
function handleMarmosetSubmit() {
    $("#toolbar-submit-results").text("submitting...").removeClass("hide");
   SeashellProject.currentProject.submit($("#marmoset_project").val())
      .done(function(){
        $("#toolbar-submit-results").text("results");
        $("#marmoset-submit-dialog").modal("hide");
    }).fail(function(){
        displayErrorMessage("Failed to submit project to Marmoset.");
        $("#toolbar-submit-results").addClass("hide");
    });
}

function generateDiff(data) {
  var lines = data.split("\n").slice(3);
  var output = "";
  var outputline = 1;
  var expectline = 1;
  var expect = "";
  for(var i=0; i < lines.length; i++) {
    var first = lines[i].substring(0,1);
    var line = lines[i].substring(1).replace(" ", "&nbsp;");
    if(first == " ") {
      output += outputline+"&nbsp;"+line+"<br />";
      expect += expectline+"&nbsp;"+line+"<br />";
      outputline++;
      expectline++;
    }
    else if(first == "+") {
      expect += "<span class='test-result-highlight'><span class='test-result-line-num'>"+expectline+"</span>&nbsp;"+line+"</span><br />";
      output += "<br />";
      expectline++;
    }
    else {
      output += "<span class='test-result-highlight'><span class='test-result-line-num'>"+outputline+"</span>&nbsp;"+line+"</span><br />";
      expect += "<br />";
      outputline++;
    }
  }
  return "<div class='test-result-output'><div class='test-result-diff'>Expected output:<br /><br />"
    +expect+"</div><div class='test-result-diff'>Program output:<br /><br />"
    +output+"</div></div>";
}

function handleRunWithTests() {
  var tests = [];
  var last;
  var boxes = $("input[name=run-with-tests-input]:checked");
  for(var i=0; i < boxes.length; i++) {
    tests.push($(boxes[i]).val());
  }
  tests.sort();
  $("#run-with-tests-dialog").modal("hide");
  $("[href=#test-tab]").tab("show");
  function runTestError() {
    displayErrorMessage("An error occurred while running the project with tests.");
  }
  function runNext(message) {
    /* message: first element tag, second data*/
    if(message) {
      if(message.tag == "pass") {
        $("#test-tab").append("<div class='alert alert-success' role='alert'><span class='glyphicon glyphicon-ok-circle'></span> Passed test <strong>"+last+".</div>");
      }
      else if(message.tag == "fail") {
        $("#test-tab").append("<div class='alert alert-danger' role='alert'><span class='glyphicon glyphicon-remove-circle'></span> Failed test <strong>"
          +last+":"+generateDiff(message.data)+"</div>");
      }
      else if(message.tag == "error") {
        $("#test-tab").append("<div class='alert alert-warning' role='alert'><span class='glyphicon glyphicon-warning-sign'></span> Program crashed on test <strong>"
          +last+":"+"<div class='test-result-output'><pre>" + message.data + "</pre></div>");
      }
      else {
        $("#test-tab").append("<div class='alert alert-info' role='alert'><span class='glyphicon glyphicon-info-sign'></span> Ran test <strong>"
          +last+"</strong> and produced the following output:<div class='test-result-output'><pre>"
          +message.data+"</pre></div>");
      }
    }
    else {
      $("#test-tab").html("");
    }
    if(tests.length) {
      last = tests.shift();
      SeashellProject.currentProject.run(last)
        .done(runNext)
        .fail(runTestError);
    }
  }
  runNext();
}

/**
 * handleLogout
 * This handles logging out of seashell.
*/
function handleLogout() {
    console.log("in handleLogout");
    displayConfirmationMessage(
        "Log out of Seashell",
        "Do you want to log out? You will lose any unsaved data.",
        function (){
            console.log("setting onbeforeunload");
            window.onbeforeunload = function() {};
            console.log("changing window");
            window.location.replace("https://cas.uwaterloo.ca/cas/logout");
            console.log("after window change");
        });
}

/**
 * setupDialogs()
 * Sets up and attaches actions to all the dialogs. */
function setupDialogs() {
  /** Set up the open-project-dialog. */
  $("#open-project-dialog").on("show.bs.modal",
      updateListOfProjects);
  $("#marmoset-submit-dialog").on("show.bs.modal",
      updateMarmosetProjects);
  $("#button-open-project").on("click",
      function() { handleOpenProject(); });
  /** Set up the new-project-dialog. */
  $("#button-new-project").on("click",
      handleNewProject);
  /** Set up the delete-project-dialog. */
  $("#button-delete-project").on("click",
      handleDeleteProject);
  /** Set up the revert-project-dialg. */
  $("#button-revert-project").on("click",
      handleRevertProject);
  $("#button-new-folder").on("click",
    handleNewFolder);
  /** Set up the delete-file-dialog. */
  $("#button-delete-file").on("click",
      handleDeleteFile);
  /** Set up the commit-project-dialog */
  $("#button-commit-project").on("click",
      handleCommitProject);
  /** Set up the upload-file-dialog */
  $("#button-upload-file").on("click",
      handleUploadFile);
  /** Set up the marmoset-submit-dialog */
  $("#button-marmoset-submit").on("click",
      handleMarmosetSubmit);
  $("#button-run-with-tests").on("click",
      handleRunWithTests);
  /** Set up the logout dialog */
  $("#menu-logout").on("click",
      handleLogout);
  /** Select the first input element, or last button if no input elements */
  $(".modal").on("shown.bs.modal", function() {
    var inp = $(this).find("input");
    if(inp) {
      // the timeout makes it work in Firefox for some reason
      setTimeout(function() {
        inp.first().focus();
      }, 5);
    }
    else {
      $(this).find("button").last().focus();
    }
  }).each(function() {
    var button = $(this).find("button").last();
    $(this).keydown(function(e) {
      if(e.which == 13) {
        button.click();
      }
    });
  });
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
  $('#confirmation-message-ok')
    .unbind('click')
    .click(function() {
      $("#confirmation-message-modal").modal("hide");
      okCallback();
    });
  $("#confirmation-message-cancel,#confirmation-message-x")
    .unbind('click')
    .click(function(){
      $("#confirmation-message-modal").modal("hide");
      if(cancelCallback) cancelCallback();
    });
}
