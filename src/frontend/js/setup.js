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
var socket = null;
var creds = null;

/**
 * Sets up the development environment.
 */
function seashellInit(rest) {
  SeashellCoder.addEntropy();
  creds = read_login_credentials();
  if (creds) {
    socket = new SeashellWebsocket("wss://" + creds.host + ":" + creds.port, creds.key);
    socket.ready.done(function() {
      console.log("Seashell socket set up properly.");
      setupUI();
      console.log("User interface set up properly.");
      setInterval(setupDisconnectMonitor, 4000);
      console.log("Websocket disconnection monitor set up properly.");
      fetchNewAssignments().done(updateListOfProjects);
      updateMarmosetProjects();
      /** Install refresh handler. */
      window.onbeforeunload = function () {
        if(SeashellProject.currentProject && SeashellProject.currentProject.isUnsaved())
          return "Are you sure you want to leave Seashell?  Unsaved data may be lost.";
      };
      /** Run the rest of the code. */
      if (rest)
        rest();
    });
    socket.ready.fail(function() {
      displayErrorMessage("Seashell socket could not be set up.");
    });
  } else {
    window.location.replace("/seashell/");
  }
}

/**
 * Sets up the user interface.
 */
function setupUI() {
  /** Clear out everything that can be hidden. */
  $(".hide-on-null-project").addClass("hide");
  $(".show-on-null-project").removeClass("hide");
  $(".hide-on-null-question").addClass("hide");
  $(".hide-on-null-file").addClass("hide");
  $(".show-on-null-file").removeClass("hide");
  $(".userid").text(creds.user).attr("title", "Logged in as "+creds.user);
  $("#main-panel-tabs").tabs();
  $("#master-container").removeClass("hide"); // show the UI
  updateListOfProjects();
  $("[href=#edit-tab]").click(function() {
    setTimeout(function() {
      editorDocument(SeashellProject.currentProject.currentFile.document);
    }, 50);
  });
  /** OK, set everything up. */
  setupEditor();
  setupConsole();
  setupDialogs();
  setupMenu();
  setupTooltips();
  setupHotkeys();
  setupDynamicResizing();
}

/*
 * These are hotkeys that will work everywhere, ie. not just
 *    in the editor.
 */
function setupHotkeys() {
  var ctrl = false;
  $("body").keydown(function(e) {
    if (!ctrl) {
      if (17 == e.keyCode)
        ctrl = true;
      return;
    }
    if (!SeashellProject.currentProject)
      return;

    function when_focused(element, fun) {
      return function() {
        if ($(element).is(':focus'))
          fun();
      };
    }
    var action = { 'r' : handleRunProject, '\r' : handleRunProject,
                   'd' : when_focused('#input-line', sendEOF),
                   'u' : handleRunTests,
		   'y' : showCommitDialog,
                   'k' : handleProgramKill
                 }[String.fromCharCode(e.keyCode).toLowerCase()];
    if (action) {
      action();
      e.preventDefault();
    }
  })
  .keyup(function(e) {
    if (e.keyCode == 17)
      ctrl = false;
  });
}

function updateDynamicUISizes()
{
  var min_height = 500, margin_bottom = 60;
  var min_y_element = $('#editor > .CodeMirror');
  var h = Math.max($(window).height()
                   - (min_y_element.offset().top - $(window).scrollTop())
                   - margin_bottom,
                   min_height);
  var narrow = $(document).width() < 992;
  $('#editor > .CodeMirror')
    .height(Math.floor(narrow ? h * 0.7 : h)
            - $('#current-file-controls').outerHeight());
  $('#console > .CodeMirror')
    .height((narrow ? (h * 0.3 - $('#console-title').outerHeight()) : h)
            - $('.console-input').outerHeight());
}

function setupDynamicResizing()
{
  $(window).resize(updateDynamicUISizes);
}

/** Accessor functions. */
function getSocket() {
  return socket;
}

function getCreds() {
  return creds;
}
