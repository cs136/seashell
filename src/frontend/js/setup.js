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
      fetchNewAssignments();
      updateListOfProjects();
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
    if(ctrl) {
      switch(e.keyCode) {
        case 83: // ctrl-s
          if(SeashellProject.currentProject) {
            handleSaveProject();
            e.preventDefault();
          }
          break;
        case 78: // ctrl-n
          if(SeashellProject.currentProject) {
            newFileDialog();
            e.preventDefault();
          }
          break;
        case 82: // ctrl-r
        case 13: // ctrl-enter
          if(SeashellProject.currentProject) {
            handleRunProject();
            e.preventDefault();
          }
          break;
      }
    }
    else {
      if(e.keyCode == 17) ctrl = true;
    }
  })
  .keyup(function(e) {
    if(e.keyCode == 17) ctrl = false;
  });
}

function updateDynamicUISizes()
{
  var h = _.max([$(window).height() - $('.editor-console').offset().top - 150,
                 250]);
  if ($(document).width() < 992)
  {
    $('#editor > .CodeMirror').height(h * 0.7);
    $('#console > .CodeMirror').height(h * 0.3);
    return;
  }
  $('#editor > .CodeMirror').height(h);
  $('#console > .CodeMirror')
    .height(h - $('#console-title').outerHeight() -
            $('.console-input').outerHeight() + 2);
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
