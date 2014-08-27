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
 * handleSaveProject( )
 * This functions handles the save project menu item.
 */
function handleSaveProject() {
  return SeashellProject.currentProject.save();
}

/**
 * handleCompileProject( )
 * This function handles the compile project menu item.
 */
function handleCompileProject() {
  consoleClear();
  SeashellProject.currentProject.compile();
}

/**
 * handleUndo( )
 * This function handles the undo menu item.
 */
function handleUndo( ) {
  editorUndo();
}

/**
 * handleRedo( )
 * This function handles the redo menu item.
 */
function handleRedo() {
  editorRedo();
}

/**
 * handleCommentLines( )
 * This function handles the comment lines menu item.
 */
function handleCommentLines() {
  // TODO: Implement comments.
}

/**
 * handleCommentSelection( )
 * This function handles the comment selection menu item.
 */
function handleCommentSelection() {
  // TODO: Implement comments.
}

/**
 * handleAutoformat( )
 * This function handles the autoformat menu item.
 */
function handleAutoformat() {
  // TODO: Implement autoformat.
}

/**
 * handleRunProject()
 * This function handles running projects.
 */
function handleRunProject() {
  // TODO: Multiple running projects?
  consoleClear();
  SeashellProject.run();
  $('#input-line').focus();
}

/**
 * handleProgramKill()
 * This function handles killing the program.
 */
function handleProgramKill() {
  SeashellProject.currentProject.kill()
    .done(function() {
      setPlayStopButtonPlaying(false);
      editor.focus();
      consoleWrite("\n--- Program stopped by user ---\n");
    });
}


function handleRunWithTestsDialog() {
  var tests = SeashellProject.currentProject.getTestsForFile(SeashellProject.currentProject.currentFile);
  var html = "";
  for(var i=0; i < tests.length; i++) {
    html += "<label class='checkbox'><input type='checkbox' name='run-with-tests-input' checked='checked' value='"+tests[i]+"' />"+tests[i]+"</label>";
  }
  if(!html) {
    html = "<p>No tests could be found.</p>";
  }
  $("#form-run-with-tests").html(html);
  $("#run-with-tests-dialog").modal("show");
}

/**
 * handleDownloadProject()
 * This function handles downloading projects.
 */
function handleDownloadProject() {
  SeashellProject.currentProject.getDownloadToken()
    .done(function(token) {
      $("#download-iframe").remove();
      var raw = JSON.stringify(token);
      var frame = $("<iframe>").attr("src",
        sprintf("https://%s:%s/export/%s.zip?token=%s",
          creds.host,
          creds.port,
          encodeURIComponent(SeashellProject.currentProject.name),
          encodeURIComponent(raw)))
        .attr("id", "download-iframe");
      $("#download-project-body").append(frame);
      $("#download-project-dialog").modal("show");
    });
}

function setPlayStopButtonPlaying(playing) {
  var a = '#toolbar-run', b = '#toolbar-kill';
  if (!playing)
    b = [a, a = b][0];
  $(a).addClass('hidden');
  $(b).removeClass('hidden');
}

/**
 * Sets up the menu; attaches actions to each menu item that does not already have
 * an action attached to it. */
function setupMenu() {
  $("#menu-save-project").on("click", handleSaveProject);
  $("#toolbar-save-project").on("click", handleSaveProject);

  $("#menu-compile").on("click", handleCompileProject);
  $("#toolbar-compile").on("click", handleCompileProject);

  $("#menu-run").on("click", handleRunProject);
  $("#toolbar-run").on("click", handleRunProject);

  $("#menu-run-tests").on("click", handleRunWithTestsDialog);
  $("#toolbar-run-tests").on("click", handleRunWithTestsDialog);

  $("#menu-kill").on("click", handleProgramKill);
  $("#toolbar-kill").on("click", handleProgramKill);

  $("#menu-undo").on("click", handleUndo);
  $("#toolbar-undo").on("click", handleUndo);
  $("#menu-redo").on("click", handleRedo);
  $("#toolbar-redo").on("click", handleRedo);

  $("#menu-commentLine").on("click", handleCommentLines);
  $("#menu-commentSelection").on("click", handleCommentSelection);
  $("#menu-autoformat").on("click", handleAutoformat);

  $("#menu-download").on("click", handleDownloadProject);
}
